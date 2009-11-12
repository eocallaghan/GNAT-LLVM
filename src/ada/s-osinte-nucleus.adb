------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--         Copyright (C) 1997-2008, Free Software Foundation, Inc.          --
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

--  This is the Nucleus version

--  This package encapsulates all direct interfaces to OS services that are
--  needed by children of System.

with Interfaces.C;
use Interfaces.C;

package body System.OS_Interface is

   -----------------
   -- sigaltstack --
   -----------------

   function sigaltstack
     (ss  : not null access stack_t;
      oss : access stack_t) return int
   is
   begin
      raise Program_Error;
      return -1;
   end sigaltstack;

   --------------
   -- mprotect --
   --------------

   function mprotect
     (addr : System.Address;
      len  : size_t;
      prot : int) return int
   is
   begin
      raise Program_Error;
      return -1;
   end mprotect;

   ------------------
   -- pthread_init --
   ------------------

   procedure pthread_init is
   begin
      null;
   end pthread_init;

   --------------------
   -- To_Clock_Ticks --
   --------------------

   function To_Clock_Ticks (D : Duration) return int is
      Ticks_Per_Sec  : int;

   begin
      if D < 0.0 then
         return -To_Clock_Ticks (-D);
      end if;

      Ticks_Per_Sec := 100; --  ??? Should get actual value from Nucleus

      --  Ensure that the duration can be converted to ticks
      --  at the current clock tick rate without overflowing.

      if D * Duration (Ticks_Per_Sec) > Duration (int'Last) then
         return int'Last;
      end if;

      return int (Duration (Ticks_Per_Sec) * D);
   end To_Clock_Ticks;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : timespec) return Duration is
   begin
      return Duration (TS.ts_sec) + Duration (TS.ts_nsec) / 10**9;
   end To_Duration;

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

end System.OS_Interface;
