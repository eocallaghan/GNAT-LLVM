------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                  S Y S T E M . O S _ P R I M I T I V E S                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1998-2008, Free Software Foundation, Inc.         --
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

--  This version is for GNU/Linux (Xenomai)

with System.OS_Interface;
--  Since the task library is part of the Xenomai kernel, using OS_Interface
--  is not a problem here, as long as we only use System.OS_Interface as a
--  set of C imported routines: using Ada routines from this package would
--  create a dependency on libgnarl in libgnat, which is not desirable.

with Interfaces.C;

with Ada.Unchecked_Conversion;

package body System.OS_Primitives is

   use System.OS_Interface;
   use type Interfaces.C.int;

   function To_Duration (T : RTime) return Duration;
   pragma Inline (To_Duration);

   function To_RTime (D : Duration) return RTime;
   pragma Inline (To_RTime);

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (T : RTime) return Duration is
      function To_Duration is new Ada.Unchecked_Conversion (SRTime, Duration);
      --  Duration and SRTime are both 64-bits types containing a count of
      --  nanoseconds so we can do unchecked conversions between them.

   begin
      return To_Duration (timer_ticks2ns (SRTime (T)));
   end To_Duration;

   --------------
   -- To_RTime --
   --------------

   function To_RTime (D : Duration) return RTime is
      Result : RTime;

      function To_SRTime is new Ada.Unchecked_Conversion (Duration, SRTime);
      --  Duration and SRTime are both 64-bits types containing a count of
      --  nanoseconds so we can do unchecked conversions between them.

   begin
      Result := RTime (timer_ns2ticks (To_SRTime (D)));

      --  The value RTime'(0) has an special meaning (infinite) so we must
      --  avoid this value in the translation.

      if Result = 0 then
         Result := 1;
      end if;

      return Result;
   end To_RTime;

   -----------
   -- Clock --
   -----------

   function Clock return Duration is
   begin
      return To_Duration (timer_read);
   end Clock;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Duration renames Clock;

   -----------------
   -- Timed_Delay --
   -----------------

   procedure Timed_Delay
     (Time : Duration;
      Mode : Integer)
   is
      Now      : Duration := Monotonic_Clock;
      Abs_Time : Duration;
      Ticks    : RTime;
      Result   : int;

   begin
      if Mode = Relative then
         if Time > 0.0 then
            Abs_Time := Now + Time;
            Ticks := To_RTime (Time);

         --  Ticks equal to zero indicates that the expiration time has
         --  already passed and no delay is needed.

         else
            Abs_Time := Now;
            Ticks := 0;
         end if;

      --  Absolute delay

      else
         Abs_Time := Time;

         if Abs_Time > Now then
            Ticks := To_RTime (Abs_Time - Now);

         --  Ticks equal to zero indicates that the expiration time has
         --  already passed and no delay is needed.

         else
            Ticks := 0;
         end if;
      end if;

      if Ticks /= 0 then
         loop
            Result := task_sleep (Ticks);
            pragma Assert (Result = 0 or else Result = EINTR);

            Now := Monotonic_Clock;

            exit when Abs_Time <= Now;

            Ticks := To_RTime (Abs_Time - Now);
         end loop;
      end if;
   end Timed_Delay;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

end System.OS_Primitives;
