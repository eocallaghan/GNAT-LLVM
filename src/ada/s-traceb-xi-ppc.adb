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

with Ada.Unchecked_Conversion;
with System.Machine_Code; use System.Machine_Code;

--  This is the bare board version of this package for PPC targets

package body System.Traceback is

   type Address_Ptr is access all System.Address;
   function To_Pointer is new
     Ada.Unchecked_Conversion (System.Address, Address_Ptr);

   procedure Call_Chain
     (Frame_Pointer : System.Address;
      Traceback     : out Tracebacks_Array;
      Len           : out Natural;
      Exclude_Min   : System.Address := System.Null_Address;
      Exclude_Max   : System.Address := System.Null_Address;
      Skip_Frames   : Natural := 0)
   is
      Return_Address_Offset : constant System.Address := 4;
      pragma Assert (Return_Address_Offset mod System.Address'Alignment = 0);
      --  Offset in bytes where return address of current frame stored,
      --  relative to current frame. This value is required to be a
      --  multiple of the ppc instruction size (4).

      PC_Adjust : constant := 4;
      --  Size of call instruction to subtract from return address to get the
      --  PC for the calling frame.

      Frame : System.Address := Frame_Pointer;
      --  Frame being processed

      Index : Natural := Traceback'First;
      --  Index of next item to store in Traceback

   begin
      Len := 0;

      --  Set to correct frame location, with correct return address

      --  Exclude Skip_Frames frames from the traceback. The PPC ABI has
      --  (System.Null_Address) as the back pointer address of the shallowest
      --  frame in the stack.

      for J in 1 .. Skip_Frames loop
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
        and then Len < Traceback'Length
      loop
         declare
            PC : constant System.Address :=
                   To_Pointer (Frame + Return_Address_Offset).all - PC_Adjust;

         begin
            if PC not in Exclude_Min .. Exclude_Max then

               --  Skip specified routines, if any (e.g. Ada.Exceptions)

               Traceback (Index) := PC;
               Len   := Len + 1;
               Index := Index + 1;
            end if;

            Frame := To_Pointer (Frame).all;
         end;

         pragma Assert (Frame /= System.Null_Address);
      end loop;
   end Call_Chain;

   procedure Call_Chain
     (Traceback   : out Tracebacks_Array;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1)
   is
      procedure Forced_Callee;
      --  Force save of return address of Call_Chain on PPC

      -------------------
      -- Forced_Callee --
      -------------------

      --  The PPC ABI has an unusual characteristic: the return address saved
      --  by a subprogram is located in its caller's frame, and the save
      --  operation only occurs if the function performs a call.

      --  To make Call_Chain able to consistently retrieve its own return
      --  address, we define Forced_Callee and call it. This routine should
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

      Frame_Pointer : System.Address;

   begin
      Forced_Callee;

      --  Move contents of r1 (sp) to "Frame_Pointer"

      Asm ("mr %0, 1",
           Outputs  => Address'Asm_Output ("=r", Frame_Pointer),
           Volatile => True);
      Call_Chain
        (Frame_Pointer, Traceback, Len, Exclude_Min, Exclude_Max, Skip_Frames);
   end Call_Chain;

   ------------------
   -- C_Call_Chain --
   ------------------

   function C_Call_Chain
     (Frame_Pointer : System.Address;
      Traceback     : System.Address;
      Traceback_Len : Integer) return Integer
   is
      subtype Tracebacks is Tracebacks_Array (1 .. Traceback_Len);
      type Ptr is access all Tracebacks;
      function To_Ptr is new Ada.Unchecked_Conversion (System.Address, Ptr);

      Len : Integer;

   begin
      Call_Chain (Frame_Pointer, To_Ptr (Traceback).all, Len);
      return Len;
   end C_Call_Chain;

end System.Traceback;
