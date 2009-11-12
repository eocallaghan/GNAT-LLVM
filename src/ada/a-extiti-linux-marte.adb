------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . E X E C U T I O N _ T I M E . T I M E R S            --
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

with System.Tasking;
with System.Task_Primitives.Operations;
with Ada.Unchecked_Conversion;
with Ada.Task_Identification;

with MaRTE.Kernel.Timers.Internals;
with MaRTE.HAL;

package body Ada.Execution_Time.Timers is

   package TMRI  renames MaRTE.Kernel.Timers.Internals;
   package TH    renames MaRTE.Kernel.Timed_Handlers;
   package THAda renames MaRTE.Kernel.Timed_Handlers.Ada_Timing_Events;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Get_Base_Clock (TM : Timer) return MaRTE.Kernel.Clock_Id;

   function To_Timing_Event_Handler is new Ada.Unchecked_Conversion
     (Timer_Handler, THAda.Base_Timing_Event_Handler);

   function To_Timer_Handler is new Ada.Unchecked_Conversion
     (THAda.Base_Timing_Event_Handler, Timer_Handler);

   --------------------
   -- Get_Base_Clock --
   --------------------

   function Get_Base_Clock (TM : Timer) return MaRTE.Kernel.Clock_Id is
      function To_Task_Id is new Ada.Unchecked_Conversion
        (Ada.Task_Identification.Task_Id, System.Tasking.Task_Id);
   begin
      return TMRI.To_Clock_Id
        (MaRTE.Kernel.Task_Id
           (System.Task_Primitives.Operations.Get_Thread_Id
              (To_Task_Id (TM.T.all))));
   end Get_Base_Clock;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (TM : in out Timer) is
   begin
      TM.Timed_Handler.Base_Clock := Get_Base_Clock (TM);
      THAda.Initialize (THAda.Base_Timing_Event (TM));
   end Initialize;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (TM      : in out Timer;
      In_Time : Ada.Real_Time.Time_Span;
      Handler : Timer_Handler)
   is
   begin
      THAda.Set_Handler_HWTime
        (THAda.Base_Timing_Event (TM),
         MaRTE.HAL.Duration_To_HWTime (Ada.Real_Time.To_Duration (In_Time)),
         To_Timing_Event_Handler (Handler),
         Options => 0); --  Relative time
   end Set_Handler;

   procedure Set_Handler
     (TM      : in out Timer;
      At_Time : CPU_Time;
      Handler : Timer_Handler)
   is
      function To_Duration is new Ada.Unchecked_Conversion
        (CPU_Time, Duration);
   begin
      THAda.Set_Handler_HWTime
        (THAda.Base_Timing_Event (TM),
         MaRTE.HAL.Duration_To_HWTime (To_Duration (At_Time)),
         To_Timing_Event_Handler (Handler),
         Options => TH.ABSOLUTE_TIMER);
   end Set_Handler;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler (TM : Timer) return Timer_Handler is
   begin
      return To_Timer_Handler
        (THAda.Current_Handler (THAda.Base_Timing_Event (TM)));
   end Current_Handler;

   --------------------
   -- Cancel_Handler --
   --------------------

   procedure Cancel_Handler
     (TM        : in out Timer;
      Cancelled : out Boolean)
   is
   begin
      THAda.Cancel_Handler (THAda.Base_Timing_Event (TM), Cancelled);
   end Cancel_Handler;

   --------------------
   -- Time_Remaining --
   --------------------

   function Time_Remaining (TM : Timer) return Ada.Real_Time.Time_Span is
   begin
      return Ada.Real_Time.To_Time_Span
        (MaRTE.HAL.HWTime_To_Duration
           (TMRI.Time_Remaining_To_CPU_Time_Event
              (TM.Timed_Handler'Unrestricted_Access,
               MaRTE.HAL.Get_HWTime)));
   end Time_Remaining;

end Ada.Execution_Time.Timers;
