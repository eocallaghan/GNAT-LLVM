------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        G E T _ T A S K _ I N F O                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1998-2005, Free Software Foundation, Inc.         --
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

with System; use System;

package GET_TASK_INFO is

   function GET_CURRENT_TASK_ID return Natural;
   --  Returns the Current Task ID

   function GET_CURRENT_TASK_PARENT_ID return Natural;
   --  Returns the Current Task's Parent Task ID

   function GET_CURRENT_TASK_CREATED_AT_PC return System.Address;
   pragma Import (Stubbed, GET_CURRENT_TASK_CREATED_AT_PC);

   function GET_CURRENT_TASK_STACK_TOP return System.Address;
   pragma Import (Stubbed, GET_CURRENT_TASK_STACK_TOP);

   function GET_CURRENT_TASK_STACK_BASE return System.Address;
   pragma Import (Stubbed, GET_CURRENT_TASK_STACK_BASE);

   function GET_CURRENT_TASK_TYPE_NAME return String;
   --  Returns the task image string from Ada.Task_Identification.Image

end GET_TASK_INFO;
