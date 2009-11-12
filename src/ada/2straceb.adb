------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T R A C E B A C K                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

--  This version is for the AE653 Level A runtime

--  The unit is intended to replace tracebak.c

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

      Frame_Link_Offset : constant System.Address := 0;
      pragma Assert (Frame_Link_Offset mod System.Address'Alignment = 0);
      --  Offset of memory location where address of previous frame is stored,
      --  relative to current frame. The address is required to be a multiple
      --  of System.Address'Alignment as the frame addresses will be
      --  dereferenced as the stack is traversed.

      Return_Address_Offset : constant System.Address := 4;
      pragma Assert (Return_Address_Offset mod System.Address'Alignment = 0);
      --  Offset in bytes where return address of current frame stored,
      --  relative to current frame. This value is required to be a
      --  multiple of the ppc instruction size (4).

      PC_Adjust : constant := 4;
      --  Size of call instruction to subtract from return address to get the
      --  PC for the calling frame.

      Frame : System.Address;
      --  Frame being processed

      Last : Integer := Traceback'First - 1;
      --  Index of last traceback written to the buffer

      procedure Forced_Callee;
      --  Force save of return address of Call_Chain on PPC

      -------------------
      -- Forced_Callee --
      -------------------

      --  The PPC ABI has an unusual characteristic: the return address saved
      --  by a subprogram is located in its caller's frame, and the save
      --  operation only occurs if the function performs a call.

      --  To make Call_Chain able to consistently retrieve its own return
      --  address, we define Forced_Callee and call it.  This routine should
      --  never be inlined.

      procedure Forced_Callee is
         Dummy : aliased Integer := 0;
         pragma Volatile (Dummy);
         pragma Warnings (Off, Dummy);
         --  Force allocation of a frame. Dummy must be both volatile and
         --  referenced (achieved by declaring it aliased). Suppress warning
         --  that it could be declared a constant, and that pragma Volatile
         --  has no effect (it forces creation of the frame).
      begin
         null;
      end Forced_Callee;

   --  Start of processing for Call_Chain

   begin
      Forced_Callee;
      Len := 0;

      --  Move contents of r1 (sp)  to "Frame"; on VxWorks, r31 (fp) contains
      --  the same value at this point in the execution

      Asm ("mr %0, 1",
           Outputs  => Address'Asm_Output ("=r", Frame),
           Volatile => True);

      --  Exclude Skip_Frames frames from the traceback. The PPC ABI has
      --  (System.Null_Address) as the back pointer address of the shallowest
      --  frame in the stack.

      for J in 1 .. Skip_Frames + 1 loop
         if Frame = System.Null_Address
           or else Frame mod System.Address'Alignment /= 0
           or else To_Pointer (Frame).all = System.Null_Address
         then
            --  Something is wrong.  Skip_Frames is greater than the number of
            --  frames on the current stack. Do not return a trace.

            return;
         end if;

         Frame := To_Pointer (Frame).all;
      end loop;

      pragma Assert (Frame /= System.Null_Address);

      while Frame mod System.Address'Alignment = 0
        and then To_Pointer (Frame).all /= System.Null_Address
        and then Last < Traceback'Last
        and then Last < Max_Len
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

            Frame := To_Pointer (Frame).all;
         end;

         pragma Assert (Frame /= System.Null_Address);
      end loop;
   end Call_Chain;

end System.Traceback;
