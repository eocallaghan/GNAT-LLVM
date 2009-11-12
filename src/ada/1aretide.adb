------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   A D A . R E A L _ T I M E . D E L A Y S                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                     Copyright (C) 2001-2005, AdaCore                     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

with System.Task_Primitives.Operations;
--  Used for Timed_Delay
--           Self

with System.Tasking;
--  Used for Initialize

package body Ada.Real_Time.Delays is

   package STPO renames System.Task_Primitives.Operations;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
   begin
      --  pragma Detect_Blocking is mandatory in this run time, so that
      --  Program_Error must be raised if this delay (potentially blocking
      --  operation) is called from a protected operation.

      if STPO.Self.Common.Protected_Action_Nesting > 0 then
         raise Program_Error;
      else
         STPO.Delay_Until (STPO.Time (T));
      end if;
   end Delay_Until;

   -----------------
   -- To_Duration --
   -----------------

   --  This function is not supposed to be used by the Ravenscar run time and
   --  it is not supposed to be with'ed by the user either (because it is an
   --  internal GNAT unit). It is kept here (returning a junk value) just for
   --  sharing the same package specification with the regular run time.

   function To_Duration (T : Time) return Duration is
      pragma Unreferenced (T);
   begin
      return 0.0;
   end To_Duration;

begin
   --  Ensure that the tasking run time is initialized when using delay
   --  operations. The initialization routine has the required machinery to
   --  prevent multiple calls to Initialize.

   System.Tasking.Initialize;
end Ada.Real_Time.Delays;
