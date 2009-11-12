------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S E C O N D A R Y _ S T A C K                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
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

--  Version for use in HI-E mode, when thread registration is used

with System.Storage_Elements;

package System.Secondary_Stack is

   package SSE renames System.Storage_Elements;

   Default_Secondary_Stack_Size : Natural := 10 * 1024;
   --  Default size of a secondary stack. May be modified by binder -D switch

   procedure SS_Init
     (Stk  : Address;
      Size : Natural := Default_Secondary_Stack_Size);
   --  Initialize the secondary stack with a main stack of the given Size.
   --
   --  Stk is an "in" parameter that is already pointing to a memory area of
   --  size Size.
   --
   --  The secondary stack is fixed, and any attempt to allocate more than the
   --  initial size will result in a Storage_Error being raised.

   procedure SS_Allocate
     (Addr         : out Address;
      Storage_Size : SSE.Storage_Count);
   --  Allocate enough space for a 'Storage_Size' bytes object with Maximum
   --  alignment. The address of the allocated space is returned in 'Address'

   type Mark_Id is private;
   --  Type used to mark the stack

   function SS_Mark return Mark_Id;
   --  Return the Mark corresponding to the current state of the stack

   procedure SS_Release (M : Mark_Id);
   --  Restore the state of the stack corresponding to the mark M

   function SS_Get_Max return Long_Long_Integer;
   --  Return maximum used space in storage units for the current secondary
   --  stack. The returned value shows the largest amount of space allocated
   --  so far during execution of the program.

private

   SS_Pool : Integer;
   --  Unused entity that is just present to ease the sharing of the pool
   --  mechanism for specific allocation/deallocation in the compiler

   type Mark_Id is new SSE.Integer_Address;

end System.Secondary_Stack;
