------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T R A C E B A C K                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2006, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with System.Address_To_Access_Conversions;
with Interfaces.C;
with System.Machine_Code; use System.Machine_Code;

--  This is the bare board version of this package for SPARC targets

package body System.Traceback is

   package Addr is new System.Address_To_Access_Conversions (System.Address);
   use Addr;

   procedure Call_Chain
     (Traceback   : in out Ada.Exceptions.Traceback.Tracebacks_Array;
      Max_Len     : Natural;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1) is

      Frame_Link_Offset : constant System.Address :=
        14 * System.Address'Size / System.Storage_Unit;
      --  Offset of memory location where address of previous frame
      --  is stored, relative to current frame. This must be
      --  a multiple of System.Address'Alignment.

      pragma Assert (Frame_Link_Offset mod System.Address'Alignment = 0);

      Return_Address_Offset : constant System.Address :=
        15 * System.Address'Size / System.Storage_Unit;
      --  Offset in bytes where return address of current frame stored,
      --  relative to current frame. This must be a multiple of
      --  System.Address'Alignment.

      pragma Assert (Return_Address_Offset mod System.Address'Alignment = 0);

      PC_Adjust : constant := 0;
      --  Size of call instruction to substract from return address to
      --  get the PC for the calling frame.

      Frame : System.Address;
      --  Frame being processed

      Last  : Integer := Traceback'First - 1;
      --  Index of last traceback written to the buffer

   begin
      Len := 0;

      --  Flush register windows before we attempt to access them on the stack

      Asm ("ta 3", Volatile => True);

      --  Move contents of r30 (fp) to "Frame"

      Asm ("mov %%fp, %0",
           Outputs => Address'Asm_Output ("=r", Frame),
           Volatile => True);

      --  Exclude Skip_Frames frames from the traceback.
      --  SPARC ABI has 0 (System.Null_Address) as the back
      --  pointer address of the shallowest frame in the stack.

      for J in 1 .. Skip_Frames - 1 loop
         if Frame = System.Null_Address
              or else
            Frame mod System.Address'Alignment /= 0
              or else
            To_Pointer (Frame + Frame_Link_Offset).all = System.Null_Address
         then
            --  Something is wrong.  Skip_Frames > the number of
            --  frames on the current stack. Do not return a trace.

            return;
         end if;

         Frame := To_Pointer (Frame + Frame_Link_Offset).all;
      end loop;

      pragma Assert (Frame /= System.Null_Address);

      while Frame mod System.Address'Alignment = 0
              and then
            To_Pointer (Frame + Frame_Link_Offset).all /= System.Null_Address
              and then
            Last < Traceback'Last
              and then
            Last < Max_Len
      loop
         declare
            PC : constant System.Address :=
                   To_Pointer (Frame + Return_Address_Offset).all - PC_Adjust;

         begin
            if PC not in Exclude_Min .. Exclude_Max then

               --  Skip Ada.Exceptions routines

               Last := Last + 1;
               Len := Len + 1;
               Traceback (Last) := PC;
            end if;

            Frame := To_Pointer (Frame + Frame_Link_Offset).all;
         end;

         pragma Assert (Frame /= System.Null_Address);
      end loop;
   end Call_Chain;

end System.Traceback;
