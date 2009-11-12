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

--  This version is for Nucleus targets

with System.OS_Interface;
--  Since the thread library is part of the Nucleus kernel, using OS_Interface
--  is not a problem here, as long as we only use System.OS_Interface as a
--  set of C imported routines: using Ada routines from this package would
--  create a dependency on libgnarl in libgnat, which is not desirable.

with Interfaces.C;

package body System.OS_Primitives is

   use System.OS_Interface;
   use type Interfaces.C.int;

   ------------------------
   -- Internal functions --
   ------------------------

   function To_Timespec (D : Duration) return timespec;
   --  Duplicated here from System.OS_Interface to avoid dependency

   -----------
   -- Clock --
   -----------

   function Clock return Duration is
      TS     : aliased timespec;
      Result : int;
   begin
      Result := clock_gettime (CLOCK_REALTIME, TS'Unchecked_Access);
      pragma Assert (Result = 0);
      return Duration (TS.ts_sec) + Duration (TS.ts_nsec) / 10**9;
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
      Request    : aliased timespec;
      Remaind    : aliased timespec;
      Rel_Time   : Duration;
      Abs_Time   : Duration;
      Base_Time  : constant Duration := Clock;
      Check_Time : Duration := Base_Time;

      Result : int;
      pragma Unreferenced (Result);

   begin
      if Mode = Relative then
         Rel_Time := Time;
         Abs_Time := Time + Check_Time;
      else
         Rel_Time := Time - Check_Time;
         Abs_Time := Time;
      end if;

      if Rel_Time > 0.0 then
         loop
            Request := To_Timespec (Rel_Time);
            Result := nanosleep (Request'Access, Remaind'Access);
            Check_Time := Clock;

            exit when Abs_Time <= Check_Time or else Check_Time < Base_Time;

            Rel_Time := Abs_Time - Check_Time;
         end loop;
      end if;
   end Timed_Delay;

   -----------------
   -- To_Timespec --
   -----------------

   function To_Timespec (D : Duration) return timespec is
      S : time_t;
      F : Duration;

   begin
      S := time_t (Long_Long_Integer (D));
      F := D - Duration (S);

      --  If F is negative due to a round-up, adjust for positive F value

      if F < 0.0 then
         S := S - 1;
         F := F + 1.0;
      end if;

      return timespec'(ts_sec  => S,
                       ts_nsec => long (Long_Long_Integer (F * 10**9)));
   end To_Timespec;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

end System.OS_Primitives;
