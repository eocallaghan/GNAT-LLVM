------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S E C O N D A R Y _ S T A C K                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2008, Free Software Foundation, Inc.         --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the VxWorks/Cert version of this package, to be used with thread
--  registration. It is a simplified version of the package that assumes the
--  fixed allocation of the secondary stack, and includes only the interfaces
--  needed for the fixed allocation case.

with Unchecked_Conversion;
with System.Soft_Links;

package body System.Secondary_Stack is

   use System.Soft_Links;
   use type SSE.Storage_Offset;

   type Memory is array (Mark_Id range <>) of SSE.Storage_Element;
   for Memory'Alignment use Standard'Maximum_Alignment;
   --  This is the type used for actual allocation of secondary stack
   --  areas. We require maximum alignment for all such allocations.

   --  The following type represents the secondary stack

   type Fixed_Stack_Id is record
      Top : Mark_Id;
      --  Index of next available location in Mem. This is initialized to
      --  0, and then incremented on Allocate, and Decremented on Release.

      Last : Mark_Id;
      --  Length of usable Mem array, which is thus the index past the
      --  last available location in Mem. Mem (Last-1) can be used. This
      --  is used to check that the stack does not overflow.

      Max : Mark_Id;
      --  Maximum value of Top. Initialized to 0, and then may be incremented
      --  on Allocate, but is never Decremented. The last used location will
      --  be Mem (Max - 1), so Max is the maximum count of used stack space.

      Mem : Memory (0 .. 0);
      --  This is the area that is actually used for the secondary stack.
      --  Note that the upper bound is a dummy value properly defined by
      --  the value of Last. We never actually allocate objects of type
      --  Fixed_Stack_Id, so the bounds declared here do not matter.
   end record;

   type Fixed_Stack_Ptr is access Fixed_Stack_Id;
   --  Pointer to record used to describe statically allocated sec stack

   function To_Fixed_Stack_Ptr is new
     Unchecked_Conversion (Address, Fixed_Stack_Ptr);
   --  Convert from address stored in task data structures

   -----------------
   -- SS_Allocate --
   -----------------

   procedure SS_Allocate
     (Addr         : out Address;
      Storage_Size : SSE.Storage_Count)
   is
      Max_Align    : constant Mark_Id := Mark_Id (Standard'Maximum_Alignment);
      Max_Size     : constant Mark_Id :=
                       ((Mark_Id (Storage_Size) + Max_Align - 1) / Max_Align)
                         * Max_Align;
      Fixed_Stack  : constant Fixed_Stack_Ptr :=
                       To_Fixed_Stack_Ptr (Get_Sec_Stack_Addr.all);

   begin
      --  Check if max stack usage is increasing

      if Fixed_Stack.Top + Max_Size > Fixed_Stack.Max then

         --  If so, check if max size is exceeded

         if Fixed_Stack.Top + Max_Size > Fixed_Stack.Last then
            raise Storage_Error;
         end if;

         --  Record new max usage

         Fixed_Stack.Max := Fixed_Stack.Top + Max_Size;
      end if;

      --  Set resulting address and update top of stack pointer

      Addr := Fixed_Stack.Mem (Fixed_Stack.Top)'Address;
      Fixed_Stack.Top := Fixed_Stack.Top + Max_Size;
   end SS_Allocate;

   ----------------
   -- SS_Get_Max --
   ----------------

   function SS_Get_Max return Long_Long_Integer is
      Fixed_Stack : constant Fixed_Stack_Ptr :=
                      To_Fixed_Stack_Ptr (Get_Sec_Stack_Addr.all);
   begin
      return Long_Long_Integer (Fixed_Stack.Max);
   end SS_Get_Max;

   -------------
   -- SS_Init --
   -------------

   procedure SS_Init
     (Stk  : Address;
      Size : Natural := Default_Secondary_Stack_Size)
   is
      Fixed_Stack : constant Fixed_Stack_Ptr := To_Fixed_Stack_Ptr (Stk);

   begin
      pragma Assert (Size >= 2 * Mark_Id'Max_Size_In_Storage_Elements);
      Fixed_Stack.Top  := 0;
      Fixed_Stack.Max  := 0;
      Fixed_Stack.Last :=
        Mark_Id (Size) - Fixed_Stack.Mem'Position;
   end SS_Init;

   -------------
   -- SS_Mark --
   -------------

   function SS_Mark return Mark_Id is
   begin
      return To_Fixed_Stack_Ptr (Get_Sec_Stack_Addr.all).Top;
   end SS_Mark;

   ----------------
   -- SS_Release --
   ----------------

   procedure SS_Release (M : Mark_Id) is
   begin
      To_Fixed_Stack_Ptr (Get_Sec_Stack_Addr.all).Top := M;
   end SS_Release;

   -------------------------
   -- Package Elaboration --
   -------------------------

   --  Allocate a secondary stack for the main program to use

   subtype Stack is Memory (1 .. Mark_Id (Default_Secondary_Stack_Size));

   type Secondary_Stack_Pointer is access Stack;

   function To_Address is new Unchecked_Conversion
     (Secondary_Stack_Pointer, Address);

   Stack_Address : Address;

begin
   Stack_Address := To_Address (new Stack);
   SS_Init (Stack_Address, Default_Secondary_Stack_Size);
   System.Soft_Links.Set_Sec_Stack_Addr_NT (Stack_Address);
end System.Secondary_Stack;
