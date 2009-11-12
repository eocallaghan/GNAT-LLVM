------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                         A D A . R E A L _ T I M E                        --
--                                                                          --
--                                 B o d y                                  --
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
--  used for Monotonic_Clock

with Unchecked_Conversion;

package body Ada.Real_Time is

   package STPO renames System.Task_Primitives.Operations;

   function To_Integer is new Unchecked_Conversion (Duration, Integer);

   function Convert_To_Duration is new
     Unchecked_Conversion (Integer, Duration);

   ---------
   -- "*" --
   ---------

   function "*" (Left : Time_Span; Right : Integer) return Time_Span is
   begin
      return Left * Time_Span (Right);
   end "*";

   function "*" (Left : Integer; Right : Time_Span) return Time_Span is
   begin
      return Time_Span (Left) * Right;
   end "*";

   ---------
   -- "+" --
   ---------

   function "+" (Left : Time; Right : Time_Span) return Time is
   begin
      return Left + Time (Right);
   end "+";

   function "+" (Left : Time_Span; Right : Time) return Time is
   begin
      return Time (Left) + Right;
   end "+";

   function "+" (Left, Right : Time_Span) return Time_Span is
   begin
      return Time_Span (Integer (Left) + Integer (Right));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Time; Right : Time_Span) return Time is
   begin
      return Left - Time (Right);
   end "-";

   function "-" (Left, Right : Time) return Time_Span is
   begin
      return Time_Span (Integer (Left) - Integer (Right));
   end "-";

   function "-" (Left, Right : Time_Span) return Time_Span is
   begin
      return Time_Span (Integer (Left) - Integer (Right));
   end "-";

   function "-" (Right : Time_Span) return Time_Span is
   begin
      return Time_Span (-Integer (Right));
   end "-";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Time_Span) return Integer is
   begin
      return Integer (Left) / Integer (Right);
   end "/";

   function "/" (Left : Time_Span; Right : Integer) return Time_Span is
   begin
      return Left / Time_Span (Right);
   end "/";

   -----------
   -- Clock --
   -----------

   function Clock return Time is
   begin
      return Time (System.Task_Primitives.Operations.Monotonic_Clock);
   end Clock;

   ------------------
   -- Microseconds --
   ------------------

   function Microseconds (US : Integer) return Time_Span is
   begin
      return
        Time_Span (US * Integer (STPO.RT_Resolution)) / Time_Span (10#1#E6);
   end Microseconds;

   ------------------
   -- Milliseconds --
   ------------------

   function Milliseconds (MS : Integer) return Time_Span is
   begin
      return
        Time_Span (MS * Integer (STPO.RT_Resolution)) / Time_Span (10#1#E3);
   end Milliseconds;

   -------------
   -- Minutes --
   -------------

   function Minutes (M : Integer) return Time_Span is
   begin
      return Milliseconds (M) * Integer'(60_000);
   end Minutes;

   -----------------
   -- Nanoseconds --
   -----------------

   function Nanoseconds (NS : Integer) return Time_Span is
   begin
      return
        Time_Span (NS * Integer (STPO.RT_Resolution)) / Time_Span (10#1#E9);
   end Nanoseconds;

   -------------
   -- Seconds --
   -------------

   function Seconds (S : Integer) return Time_Span is
   begin
      return Milliseconds (S) * Integer'(1000);
   end Seconds;

   -----------
   -- Split --
   -----------

   procedure Split (T : Time; SC : out Seconds_Count; TS : out Time_Span) is
      Res : constant Time := Time (STPO.RT_Resolution);
   begin
      SC := Seconds_Count (T / Res);
      TS := Time_Span (T) - Time_Span (Time (SC) * Res);
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of (SC : Seconds_Count; TS : Time_Span) return Time is
   begin
      return Time (SC) * Time (STPO.RT_Resolution) + TS;
   end Time_Of;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : Time_Span) return Duration is
   begin
      return Convert_To_Duration
        ((Integer (TS) * 10000) / Integer (STPO.RT_Resolution));
   end To_Duration;

   ------------------
   -- To_Time_Span --
   ------------------

   function To_Time_Span (D : Duration) return Time_Span is
   begin
      return Time_Span (To_Integer (D) * Integer (STPO.RT_Resolution) / 10000);
   end To_Time_Span;

end Ada.Real_Time;
