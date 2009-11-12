------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         A D A . R E A L _ T I M E                        --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                     Copyright (C) 2001-2008, AdaCore                    --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the Ravenscar version of this package for generic bare board
--  targets

with System.OS_Interface;
pragma Elaborate_All (System.OS_Interface);

package Ada.Real_Time is

   type Time is private;
   Time_First : constant Time;
   Time_Last  : constant Time;

   Time_Unit : constant Duration :=
                 1.0 / Duration (System.OS_Interface.Ticks_Per_Second);
   --  This does not conform to the RM, which requires a named number here ???
   --  These platforms use a time stamp counter driven by the system clock,
   --  where the duration of the clock tick (Time_Unit) depends on the speed
   --  of the underlying hardware. The system clock frequency is used here to
   --  determine Time_Unit.

   type Time_Span is private;
   Time_Span_First : constant Time_Span;
   Time_Span_Last  : constant Time_Span;
   Time_Span_Zero  : constant Time_Span;
   Time_Span_Unit  : constant Time_Span;

   Tick : constant Time_Span;
   function Clock return Time;

   function "+"  (Left : Time;      Right : Time_Span) return Time;
   function "+"  (Left : Time_Span; Right : Time)      return Time;
   function "-"  (Left : Time;      Right : Time_Span) return Time;
   function "-"  (Left : Time;      Right : Time)      return Time_Span;

   function "<"  (Left, Right : Time) return Boolean;
   function "<=" (Left, Right : Time) return Boolean;
   function ">"  (Left, Right : Time) return Boolean;
   function ">=" (Left, Right : Time) return Boolean;

   function "+"  (Left, Right : Time_Span)             return Time_Span;
   function "-"  (Left, Right : Time_Span)             return Time_Span;
   function "-"  (Right : Time_Span)                   return Time_Span;
   function "*"  (Left : Time_Span; Right : Integer)   return Time_Span;
   function "*"  (Left : Integer;   Right : Time_Span) return Time_Span;
   function "/"  (Left, Right : Time_Span)             return Integer;
   function "/"  (Left : Time_Span; Right : Integer)   return Time_Span;

   function "abs" (Right : Time_Span) return Time_Span;

   function "<"  (Left, Right : Time_Span) return Boolean;
   function "<=" (Left, Right : Time_Span) return Boolean;
   function ">"  (Left, Right : Time_Span) return Boolean;
   function ">=" (Left, Right : Time_Span) return Boolean;

   function To_Duration  (TS : Time_Span) return Duration;
   function To_Time_Span (D : Duration)   return Time_Span;

   function Nanoseconds  (NS : Integer) return Time_Span;
   function Microseconds (US : Integer) return Time_Span;
   function Milliseconds (MS : Integer) return Time_Span;

   function Seconds (S : Integer) return Time_Span;
   pragma Ada_05 (Seconds);

   function Minutes (M : Integer) return Time_Span;
   pragma Ada_05 (Minutes);

   type Seconds_Count is mod 2 ** Long_Integer'Size;

   procedure Split (T : Time; SC : out Seconds_Count; TS : out Time_Span);
   function Time_Of (SC : Seconds_Count; TS : Time_Span) return Time;

private
   type Time is new System.OS_Interface.Time;

   Time_First : constant Time := Time'First;

   Time_Last  : constant Time := Time'Last;

   type Time_Span is new System.OS_Interface.Time_Span;

   Time_Span_First : constant Time_Span := Time_Span'First;

   Time_Span_Last  : constant Time_Span := Time_Span'Last;

   Time_Span_Zero  : constant Time_Span := 0;

   Time_Span_Unit  : constant Time_Span := 1;

   Tick : constant Time_Span := 1;

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");
   pragma Import (Intrinsic, "abs");

   pragma Inline (Microseconds);
   pragma Inline (Milliseconds);
   pragma Inline (Nanoseconds);
   pragma Inline (Seconds);
   pragma Inline (Minutes);

end Ada.Real_Time;
