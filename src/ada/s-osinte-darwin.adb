------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1999-2009, Free Software Foundation, Inc.         --
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

--  This is a Darwin Threads version of this package

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

package body System.OS_Interface is

   use Interfaces.C;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : timespec) return Duration is
   begin
      return Duration (TS.tv_sec) + Duration (TS.tv_nsec) / 10#1#E9;
   end To_Duration;

   function To_Duration (TV : struct_timeval) return Duration is
   begin
      return Duration (TV.tv_sec) + Duration (TV.tv_usec) / 10#1#E6;
   end To_Duration;

   ------------------------
   -- To_Target_Priority --
   ------------------------

   function To_Target_Priority
     (Prio : System.Any_Priority) return Interfaces.C.int
   is
   begin
      return Interfaces.C.int (Prio);
   end To_Target_Priority;

   -----------------
   -- To_Timespec --
   -----------------

   function To_Timespec (D : Duration) return timespec is
      S : time_t;
      F : Duration;

   begin
      S := time_t (Long_Long_Integer (D));
      F := D - Duration (S);

      --  If F has negative value due to a round-up, adjust for positive F
      --  value.

      if F < 0.0 then
         S := S - 1;
         F := F + 1.0;
      end if;

      return timespec'(tv_sec => S,
        tv_nsec => long (Long_Long_Integer (F * 10#1#E9)));
   end To_Timespec;

   ----------------
   -- To_Timeval --
   ----------------

   function To_Timeval (D : Duration) return struct_timeval is
      S : time_t;
      F : Duration;

   begin
      S := time_t (D);
      F := D - Duration (S);

      --  If F has negative value due to a round-up, adjust for positive F
      --  value.

      if F < 0.0 then
         S := S - 1;
         F := F + 1.0;
      end if;

      return struct_timeval'
               (Tv_Sec  => S,
                tv_usec => int32_t (Long_Long_Integer (F * 10#1#E6)));
   end To_Timeval;

   -------------------
   -- clock_gettime --
   -------------------

   function clock_gettime
     (clock_id : clockid_t;
      tp       : access timespec) return int
   is
      pragma Unreferenced (clock_id);
      Result : int;
      tv     : aliased struct_timeval;

      function gettimeofday
        (tv : access struct_timeval;
         tz : System.Address := System.Null_Address) return int;
      pragma Import (C, gettimeofday, "gettimeofday");

   begin
      Result := gettimeofday (tv'Unchecked_Access);
      tp.all := To_Timespec (To_Duration (tv));
      return Result;
   end clock_gettime;

   -----------------
   -- sched_yield --
   -----------------

   function sched_yield return int is
      procedure sched_yield_base (arg : System.Address);
      pragma Import (C, sched_yield_base, "pthread_yield_np");

   begin
      sched_yield_base (System.Null_Address);
      return 0;
   end sched_yield;

   --------------
   -- lwp_self --
   --------------

   function lwp_self return Address is
      function pthread_mach_thread_np (thread : pthread_t) return Address;
      pragma Import (C, pthread_mach_thread_np, "pthread_mach_thread_np");
   begin
      return pthread_mach_thread_np (pthread_self);
   end lwp_self;

   ------------------
   -- pthread_init --
   ------------------

   procedure pthread_init is
   begin
      null;
   end pthread_init;

   ----------------
   -- Stack_Base --
   ----------------

   function Get_Stack_Base (thread : pthread_t) return Address is
      pragma Unreferenced (thread);
   begin
      return System.Null_Address;
   end Get_Stack_Base;

end System.OS_Interface;
