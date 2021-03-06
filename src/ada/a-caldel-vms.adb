------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   A D A . C A L E N D A R . D E L A Y S                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--                     Copyright (C) 1995-2008, AdaCore                     --
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

--  This is the Alpha/VMS version

with System.OS_Primitives;
with System.Soft_Links;

package body Ada.Calendar.Delays is

   package OSP renames System.OS_Primitives;
   package TSL renames System.Soft_Links;

   use type TSL.Timed_Delay_Call;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Timed_Delay_NT (Time : Duration; Mode : Integer);
   --  Timed delay procedure used when no tasking is active

   ---------------
   -- Delay_For --
   ---------------

   procedure Delay_For (D : Duration) is
   begin
      TSL.Timed_Delay.all
        (Duration'Min (D, OSP.Max_Sensible_Delay), OSP.Relative);
   end Delay_For;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
   begin
      TSL.Timed_Delay.all (To_Duration (T), OSP.Absolute_Calendar);
   end Delay_Until;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (T : Time) return Duration is
      Safe_Ada_High : constant Time := Time_Of (2250, 1, 1, 0.0);
      --  A value distant enough to emulate "end of time" but which does not
      --  cause overflow.

      Safe_T : Time;

   begin
      if T > Safe_Ada_High then
         Safe_T := Safe_Ada_High;
      else
         Safe_T := T;
      end if;

      return OSP.To_Duration (OSP.OS_Time (Safe_T), OSP.Absolute_Calendar);
   end To_Duration;

   --------------------
   -- Timed_Delay_NT --
   --------------------

   procedure Timed_Delay_NT (Time : Duration; Mode : Integer) is
   begin
      OSP.Timed_Delay (Time, Mode);
   end Timed_Delay_NT;

begin
   --  Set up the Timed_Delay soft link to the non tasking version if it has
   --  not been already set. If tasking is present, Timed_Delay has already set
   --  this soft link, or this will be overridden during the elaboration of
   --  System.Tasking.Initialization

   if TSL.Timed_Delay = null then
      TSL.Timed_Delay := Timed_Delay_NT'Access;
   end if;
end Ada.Calendar.Delays;
