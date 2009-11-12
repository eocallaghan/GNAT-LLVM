------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          A D A . R E A L _ T I M E . T I M I N G _ E V E N T S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2005-2008, Free Software Foundation, Inc.        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is a MaRTE OS version of this package

pragma Style_Checks (All_Checks);
--  Turn off mode in check for procedures since MaRTE packages do not follow
--  -gnatg style checks yet. ???

with Ada.Unchecked_Conversion;

with MaRTE.Kernel.Timers;
with MaRTE.HAL;

package body Ada.Real_Time.Timing_Events is

   package THAda renames MaRTE.Kernel.Timed_Handlers.Ada_Timing_Events;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function To_Kernel_Handler is new Ada.Unchecked_Conversion
     (Timing_Event_Handler, THAda.Base_Timing_Event_Handler);

   function To_Ada_Handler is new Ada.Unchecked_Conversion
     (THAda.Base_Timing_Event_Handler, Timing_Event_Handler);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Event : in out Timing_Event) is
   begin
      Event.Timed_Handler.Base_Clock := MaRTE.Kernel.Timers.CLOCK_MONOTONIC;
      THAda.Initialize (THAda.Base_Timing_Event (Event));
   end Initialize;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (Event   : in out Timing_Event;
      At_Time : Time;
      Handler : Timing_Event_Handler)
   is
   begin
      THAda.Set_Handler_HWTime
        (THAda.Base_Timing_Event (Event),
         MaRTE.HAL.Duration_To_HWTime (Duration (At_Time)),
         To_Kernel_Handler (Handler),
         MaRTE.Kernel.Timed_Handlers.ABSOLUTE_TIMER);
   end Set_Handler;

   procedure Set_Handler
     (Event   : in out Timing_Event;
      In_Time : in Time_Span;
      Handler : in Timing_Event_Handler)
   is
   begin
      THAda.Set_Handler_HWTime
        (THAda.Base_Timing_Event (Event),
         MaRTE.HAL.Duration_To_HWTime (Duration (In_Time)),
         To_Kernel_Handler (Handler),
         Options => 0); --  Relative time
   end Set_Handler;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler
     (Event : Timing_Event) return Timing_Event_Handler
   is
   begin
      return To_Ada_Handler
        (THAda.Current_Handler (THAda.Base_Timing_Event (Event)));
   end Current_Handler;

   --------------------
   -- Cancel_Handler --
   --------------------

   procedure Cancel_Handler
     (Event     : in out Timing_Event;
      Cancelled : out Boolean)
   is
   begin
      THAda.Cancel_Handler (THAda.Base_Timing_Event (Event), Cancelled);
   end Cancel_Handler;

   -------------------
   -- Time_Of_Event --
   -------------------

   function Time_Of_Event (Event : Timing_Event) return Time is
   begin
      if Event.Cleared then
         return Ada.Real_Time.Time_First;
      else
         return Time (MaRTE.HAL.HWTime_To_Duration (Event.Timed_Handler.T));
      end if;
   end Time_Of_Event;

end Ada.Real_Time.Timing_Events;
