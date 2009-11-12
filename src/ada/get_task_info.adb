------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        G E T _ T A S K _ I N F O                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1998-2008, Free Software Foundation, Inc.         --
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

--  This is an AlphaVMS package
--
with Ada.Task_Identification; use Ada.Task_Identification;
with System.Tasking;
with Unchecked_Conversion;

package body GET_TASK_INFO is

   pragma Warnings (Off);  --  ??? needs comment
   function To_Nat is new Unchecked_Conversion
     (System.Tasking.Task_Id, Natural);
   pragma Warnings (On);

   function GET_CURRENT_TASK_ID return Natural is
   begin
      return To_Nat (System.Tasking.Self);
   end GET_CURRENT_TASK_ID;

   function GET_CURRENT_TASK_PARENT_ID return Natural is
   begin
      return To_Nat (System.Tasking.Self.Common.Parent);
   end GET_CURRENT_TASK_PARENT_ID;

   function GET_CURRENT_TASK_TYPE_NAME return String is
   begin
      return Image (Current_Task);
   end GET_CURRENT_TASK_TYPE_NAME;

end GET_TASK_INFO;
