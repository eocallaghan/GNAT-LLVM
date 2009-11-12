------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--               SYSTEM.TASK_PRIMITIVES.INTERRUPT_OPERATIONS                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1998-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

package body System.Task_Primitives.Interrupt_Operations is

   --  ??? The VxWorks version of System.Interrupt_Management needs to access
   --  this array, but due to elaboration problems, it can't with this
   --  package directly, so we export this variable for now.

   Interrupt_ID_Map : array (IM.Interrupt_ID) of ST.Task_Id;
   pragma Export (Ada, Interrupt_ID_Map,
     "system__task_primitives__interrupt_operations__interrupt_id_map");

   ----------------------
   -- Get_Interrupt_ID --
   ----------------------

   function Get_Interrupt_ID (T : ST.Task_Id) return IM.Interrupt_ID is
      use type ST.Task_Id;

   begin
      for Interrupt in IM.Interrupt_ID loop
         if Interrupt_ID_Map (Interrupt) = T then
            return Interrupt;
         end if;
      end loop;

      raise Program_Error;
   end Get_Interrupt_ID;

   -----------------
   -- Get_Task_Id --
   -----------------

   function Get_Task_Id (Interrupt : IM.Interrupt_ID) return ST.Task_Id is
   begin
      return Interrupt_ID_Map (Interrupt);
   end Get_Task_Id;

   ----------------------
   -- Set_Interrupt_ID --
   ----------------------

   procedure Set_Interrupt_ID (Interrupt : IM.Interrupt_ID; T : ST.Task_Id) is
   begin
      Interrupt_ID_Map (Interrupt) := T;
   end Set_Interrupt_ID;

end System.Task_Primitives.Interrupt_Operations;
