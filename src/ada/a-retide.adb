------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   A D A . R E A L _ T I M E . D E L A Y S                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1992-2008, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;

with System.Tasking;
with System.Task_Primitives.Operations;

package body Ada.Real_Time.Delays is

   package STPO renames System.Task_Primitives.Operations;

   ----------------
   -- Local Data --
   ----------------

   Absolute_RT : constant := 2;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
      Self_Id : constant System.Tasking.Task_Id := STPO.Self;

   begin
      --  If pragma Detect_Blocking is active, Program_Error must be
      --  raised if this potentially blocking operation is called from a
      --  protected action.

      if System.Tasking.Detect_Blocking
        and then Self_Id.Common.Protected_Action_Nesting > 0
      then
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity, "potentially blocking operation");
      else
         STPO.Timed_Delay (Self_Id, To_Duration (T), Absolute_RT);
      end if;
   end Delay_Until;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (T : Time) return Duration is
   begin
      return To_Duration (Time_Span (T));
   end To_Duration;

begin
   --  Ensure that the tasking run time is initialized when using delay
   --  operations. The initialization routine has the required machinery to
   --  prevent multiple calls to Initialize.

   System.Tasking.Initialize;
end Ada.Real_Time.Delays;
