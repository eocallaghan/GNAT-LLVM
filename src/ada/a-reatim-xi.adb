------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                         A D A . R E A L _ T I M E                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2001-2009, AdaCore                     --
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

--  This is the Ravenscar version of this package for generic bare board
--  targets. Note that the operations here assume that Time is a 64-bit
--  unsigned integer and Time_Span is a 64-bit signed integer.

with System.Task_Primitives.Operations;

with Ada.Unchecked_Conversion;

package body Ada.Real_Time is

   package OSI renames System.OS_Interface;

   -----------------------
   -- Local definitions --
   -----------------------

   type Uint_64 is mod 2 ** 64;
   --  Type used to represent intermediate results of arithmetic operations

   -----------------------
   -- Local subprograms --
   -----------------------

   function To_Duration is
     new Ada.Unchecked_Conversion (Long_Long_Integer, Duration);

   function To_Integer is
     new Ada.Unchecked_Conversion (Duration, Long_Long_Integer);

   ---------
   -- "*" --
   ---------

   function "*" (Left : Time_Span; Right : Integer) return Time_Span is
      Max_Pos_Time_Span : constant := Uint_64 (Time_Span_Last);
      --  Absoulute value of the biggest number of type Time_Span

      Max_Neg_Time_Span : constant Uint_64 := Uint_64 (2 ** 63);
      --  Absolute value of the most negative number of type Time_Span

   begin
      --  Overflow checks are be performed by hand assuming that Time_Span is a
      --  64-bit signed integer. Otherwise these checks would need an
      --  intermediate type with more than 64-bit.

      if (((Left > 0 and then Right > 0) or else (Left < 0 and then Right < 0))
             and then
          Max_Pos_Time_Span / Uint_64 (abs (Right)) < Uint_64 (abs (Left)))
        or else
         (((Left > 0 and then Right < 0) or else (Left < 0 and then Right > 0))
             and then
          Max_Neg_Time_Span / Uint_64 (abs (Right)) < Uint_64 (abs (Left)))
      then
         raise Constraint_Error;
      else
         return Left * Time_Span (Right);
      end if;
   end "*";

   function "*" (Left : Integer; Right : Time_Span) return Time_Span is
   begin
      return Right * Left;
   end "*";

   ---------
   -- "+" --
   ---------

   function "+" (Left : Time; Right : Time_Span) return Time is
   begin
      --  Overflow checks are be performed by hand assuming that Time and
      --  Time_Span are 64-bit unsigned and signed integers respectively.
      --  Otherwise these checks would need an intermediate type with more
      --  than 64-bit.

      if Right >= 0
        and then Uint_64 (Time_Last) - Uint_64 (Left) >= Uint_64 (Right)
      then
         return Time (Uint_64 (Left) + Uint_64 (Right));

      elsif Right < 0 and then Left >= Time (abs (Right)) then
         return Time (Uint_64 (Left) - Uint_64 (abs (Right)));

      else
         raise Constraint_Error;
      end if;
   end "+";

   function "+" (Left : Time_Span; Right : Time) return Time is
   begin
      --  Overflow checks must be performed by hand assuming that Time and
      --  Time_Span are 64-bit unsigned and signed integers respectively.
      --  Otherwise these checks would need an intermediate type with more
      --  than 64-bit.

      if Left >= 0
        and then Uint_64 (Time_Last) - Uint_64 (Right) >= Uint_64 (Left)
      then
         return Time (Uint_64 (Left) + Uint_64 (Right));

      elsif Left < 0 and then Right >= Time (abs (Left)) then
         return Time (Uint_64 (Right) - Uint_64 (abs (Left)));

      else
         raise Constraint_Error;
      end if;
   end "+";

   function "+" (Left, Right : Time_Span) return Time_Span is
      pragma Unsuppress (Overflow_Check);
   begin
      return Time_Span (Long_Long_Integer (Left) + Long_Long_Integer (Right));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Time; Right : Time_Span) return Time is
   begin
      --  Overflow checks must be performed by hand assuming that Time and
      --  Time_Span are 64-bit unsigned and signed integers respectively.
      --  Otherwise these checks would need an intermediate type with more
      --  than 64-bit.

      if Right >= 0 and then Left >= Time (Right) then
         return Time (Uint_64 (Left) - Uint_64 (Right));

      elsif Right < 0
        and then Uint_64 (Time_Last) - Uint_64 (Left) >= Uint_64 (abs (Right))
      then
         return Left + Time (abs (Right));

      else
         raise Constraint_Error;
      end if;
   end "-";

   function "-" (Left, Right : Time) return Time_Span is
   begin
      --  Overflow checks must be performed by hand assuming that Time and
      --  Time_Span are 64-bit unsigned and signed integers respectively.
      --  Otherwise these checks would need an intermediate type with more
      --  than 64-bit.

      if Left >= Right
        and then Uint_64 (Left) - Uint_64 (Right) <= Uint_64 (Time_Span_Last)
      then
         return Time_Span (Uint_64 (Left) - Uint_64 (Right));

      elsif Left < Right
        and then Uint_64 (Right) - Uint_64 (Left) <= Uint_64 (2 ** 63)
      then
         return Time_Span (-(Uint_64 (Right) - Uint_64 (Left)));

      else
         raise Constraint_Error;
      end if;
   end "-";

   function "-" (Left, Right : Time_Span) return Time_Span is
      pragma Unsuppress (Overflow_Check);
   begin
      return Time_Span (Long_Long_Integer (Left) - Long_Long_Integer (Right));
   end "-";

   function "-" (Right : Time_Span) return Time_Span is
      pragma Unsuppress (Overflow_Check);
   begin
      return Time_Span (-Long_Long_Integer (Right));
   end "-";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Time_Span) return Integer is
      pragma Unsuppress (Overflow_Check);
   begin
      return Integer (Long_Long_Integer (Left) / Long_Long_Integer (Right));
   end "/";

   function "/" (Left : Time_Span; Right : Integer) return Time_Span is
      pragma Unsuppress (Overflow_Check);
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
      return Time_Span
        (Long_Long_Integer (US) * Long_Long_Integer (OSI.Ticks_Per_Second)) /
          Time_Span (10#1#E6);
   end Microseconds;

   ------------------
   -- Milliseconds --
   ------------------

   function Milliseconds (MS : Integer) return Time_Span is
   begin
      return Time_Span
        (Long_Long_Integer (MS) * Long_Long_Integer (OSI.Ticks_Per_Second)) /
          Time_Span (10#1#E3);
   end Milliseconds;

   -------------
   -- Minutes --
   -------------

   function Minutes (M : Integer) return Time_Span is
   begin
      return Time_Span
        (Long_Long_Integer (M) * 60 *
          Long_Long_Integer (OSI.Ticks_Per_Second));
   end Minutes;

   -----------------
   -- Nanoseconds --
   -----------------

   function Nanoseconds (NS : Integer) return Time_Span is
   begin
      return Time_Span
        (Long_Long_Integer (NS) * Long_Long_Integer (OSI.Ticks_Per_Second)) /
          Time_Span (10#1#E9);
   end Nanoseconds;

   -------------
   -- Seconds --
   -------------

   function Seconds (S : Integer) return Time_Span is
   begin
      return Time_Span
        (Long_Long_Integer (S) * Long_Long_Integer (OSI.Ticks_Per_Second));
   end Seconds;

   -----------
   -- Split --
   -----------

   procedure Split (T : Time; SC : out Seconds_Count; TS : out Time_Span) is
      pragma Unsuppress (Overflow_Check);
      Res : constant Time := Time (OSI.Ticks_Per_Second);
   begin
      SC := Seconds_Count (T / Res);
      TS := T - Time (SC) * Res;
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of (SC : Seconds_Count; TS : Time_Span) return Time is
   begin
      return Time (SC) * Time (OSI.Ticks_Per_Second) + TS;
   end Time_Of;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : Time_Span) return Duration is
      Min_Time_Span : constant Time_Span :=
                        Time_Span (Long_Long_Integer'First /
                          (Long_Long_Integer (1.0 / Duration'Small) /
                            Long_Long_Integer (OSI.Ticks_Per_Second)));
      --  Minimum value for a Time_Span variable that can be transformed into
      --  Duration without overflow.

      Max_Time_Span : constant Time_Span :=
                        Time_Span (Long_Long_Integer'Last /
                          (Long_Long_Integer (1.0 / Duration'Small) /
                            Long_Long_Integer (OSI.Ticks_Per_Second)));
      --  Maximum value for a Time_Span value that can be transformed into
      --  Duration without overflow.

      TS_In_Duration_Range : constant Boolean :=
                               TS <= Max_Time_Span
                                 and then
                               TS >= Min_Time_Span;
      --  True if TS can be transformed into Duration without overflow

   begin
      --  Perform range checks required by AI-00432. Use the intermediate
      --  Integer representation of Duration to allow for simple Integer
      --  operations. We take advantage of the fact that Duration is
      --  represented as an Integer with units of Small.

      if TS_In_Duration_Range then
         return To_Duration
           (Long_Long_Integer (TS) *
             (Long_Long_Integer (1.0 / Duration'Small) /
               Long_Long_Integer (OSI.Ticks_Per_Second)));

      else
         --  The resulting conversion would be out of range for Duration

         raise Constraint_Error;
      end if;
   end To_Duration;

   ------------------
   -- To_Time_Span --
   ------------------

   function To_Time_Span (D : Duration) return Time_Span is
   begin
      --  Use the intermediate Integer representation of Duration to allow for
      --  simple Integer operations. We take advantage of the fact that
      --  Duration is represented as an Integer with units of Small.

      --  Overflow checks are not needed here if the clock tick is greater or
      --  equal than 1 nanosecond. Duration is a 64-bit type counting number
      --  of nanoseconds, and Time_Span is a 64-bit type counting number of
      --  clock ticks. The range of Time_Span is larger than the range of
      --  Duration if the clock frequency is under 1GHz (1 tick per
      --  nanosecond).

      pragma Assert (OSI.Ticks_Per_Second <= 10#1#E9);

      return Time_Span
        (To_Integer (D) /
          (Long_Long_Integer (1.0 / Duration'Small) /
            Long_Long_Integer (OSI.Ticks_Per_Second)));
   end To_Time_Span;

end Ada.Real_Time;
