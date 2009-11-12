------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--    S Y S T E M . S E C O N D A R Y _ S T A C K . S I N G L E _ T A S K   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2005-2008, Free Software Foundation, Inc.         --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Elaboration_Code);
--  We want to guarantee the absence of elaboration code because the
--  binder does not handle references to this package.

with System.Storage_Elements;

package body System.Secondary_Stack.Single_Task is

   ----------------
   -- Local Data --
   ----------------

   Initialized : Boolean := False;
   --  Boolean flag that indicates whether the memory area to be used as a
   --  secondary stack has already been initialized.

   Secondary_Stack : aliased Storage_Elements.Storage_Array
     (1 .. Storage_Elements.Storage_Offset (Default_Secondary_Stack_Size));
   for Secondary_Stack'Alignment use Standard'Maximum_Alignment;
   --  The secondary stack

   -------------------
   -- Get_Sec_Stack --
   -------------------

   function Get_Sec_Stack return Address is
   begin
      if not Initialized then
         --  Initialize the secondary stack

         SS_Init (Secondary_Stack'Address, Default_Secondary_Stack_Size);

         Initialized := True;
      end if;

      return Secondary_Stack'Address;
   end Get_Sec_Stack;

end System.Secondary_Stack.Single_Task;
