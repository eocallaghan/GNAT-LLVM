------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                  S Y S T E M . O S _ P R I M I T I V E S                 --
--                                                                          --
--                                  B o d y                                 --
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

--  This version uses gettimeofday and select
--  This file is suitable for Solaris (32 and 64 bits).

package body System.OS_Primitives is

   --  ??? These definitions are duplicated from System.OS_Interface
   --  because we don't want to depend on any package. Consider removing
   --  these declarations in System.OS_Interface and move these ones in
   --  the spec.

   type struct_timeval is record
      tv_sec  : Long_Integer;
      tv_usec : Long_Integer;
   end record;
   pragma Convention (C, struct_timeval);

   procedure gettimeofday
     (tv : not null access struct_timeval;
      tz : Address := Null_Address);
   pragma Import (C, gettimeofday, "gettimeofday");

   procedure C_select
     (n         : Integer := 0;
      readfds,
      writefds,
      exceptfds : Address := Null_Address;
      timeout   : not null access struct_timeval);
   pragma Import (C, C_select, "select");

   -----------
   -- Clock --
   -----------

   function Clock return Duration is
      TV : aliased struct_timeval;

   begin
      gettimeofday (TV'Access);
      return Duration (TV.tv_sec) + Duration (TV.tv_usec) / 10#1#E6;
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
      Rel_Time   : Duration;
      Abs_Time   : Duration;
      Base_Time  : constant Duration := Clock;
      Check_Time : Duration := Base_Time;
      timeval    : aliased struct_timeval;

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
            timeval.tv_sec := Long_Integer (Rel_Time);

            if Duration (timeval.tv_sec) > Rel_Time then
               timeval.tv_sec := timeval.tv_sec - 1;
            end if;

            timeval.tv_usec :=
              Long_Integer ((Rel_Time - Duration (timeval.tv_sec)) * 10#1#E6);

            C_select (timeout => timeval'Unchecked_Access);
            Check_Time := Clock;

            exit when Abs_Time <= Check_Time or else Check_Time < Base_Time;

            Rel_Time := Abs_Time - Check_Time;
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
