------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                         S Y S T E M . B B . T I M E                      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2008, AdaCore                     --
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
-- The porting of GNARL to bare board  targets was initially  developed  by --
-- the Real-Time Systems Group at the Technical University of Madrid.       --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Elaboration_Code);

with Ada.Unchecked_Conversion;

with System.BB.Interrupts;
with System.BB.Threads;
with System.BB.Protection;
with System.BB.Threads.Queues;

package body System.BB.Time is

   use type Peripherals.Timer_Interval;

   --  We use two timers:
   --     A Periodic Timer for the clock
   --     An Alarm Timer for delays

   -----------------------
   -- Local definitions --
   -----------------------

   type Clock_Periods is mod 2 ** (64 - Peripherals.Timer_Interval'Size);
   for Clock_Periods'Size use (64 - Peripherals.Timer_Interval'Size);
   --  Values of this type represent number of times that the clock finishes
   --  its countdown.

   MSP_Clock : Clock_Periods := 0;
   pragma Volatile (MSP_Clock);
   --  MSP_Clock object holds the Most Significant Part of the Clock. The
   --  Lowest Significant Part of the Clock is held in the hardware clock
   --  register. The range of the Lowest Significant Part of the Clock is
   --  System.BB.Parameters.Timer_Interval'Range.
   --
   --  We need to make MSP_Clock volatile because MSP_Clock is updated by
   --  Clock_Handler. Atomic is not needed because the value is updated within
   --  a non-preemptable region (protected by Enter_Kernel/Leave_Kernel).

   Pending_Alarm : Boolean := False;
   pragma Atomic (Pending_Alarm);
   --  Alarm Timer is used to set alarms between two periodic interrupts. But
   --  it is possible than due to calculations delay an alarm could expire
   --  after the clock interrupt. If so the Clock Handler does nothing with
   --  alarms, so that this flag shows if an alarm is pending.

   type Split_Time is
      record
         MSP : Clock_Periods;
         LSP : Peripherals.Timer_Interval;
      end record;

   for Split_Time use
      record
         MSP at 0 range 0 .. (63 - Peripherals.Timer_Interval'Size);
         LSP at 0 range (64 - Peripherals.Timer_Interval'Size) .. 63;
      end record;

   for Split_Time'Size use 64;
   --  The type Split_Time represents a 64-bit time value, but it gives access
   --  to the two parts (MSP and LSP) separately.

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Alarm_Handler (Interrupt : Interrupts.Interrupt_ID);
   --  Handler for the alarm interrupt

   procedure Clock_Handler (Interrupt : Interrupts.Interrupt_ID);
   --  Handler for the clock interrupt

   function To_Time is new Ada.Unchecked_Conversion (Split_Time, Time);
   --  Function to change the view from Split_Time (a record with two unsigned
   --  32-bit fields) to Time (unsigned 64-bit).

   function To_Split_Time is new Ada.Unchecked_Conversion (Time, Split_Time);
   --  Function to change the view from Time (unsigned 64-bit) to Split_Time
   --  (a record with two unsigned fields).

   -------------------
   -- Alarm_Handler --
   -------------------

   procedure Alarm_Handler (Interrupt : Interrupts.Interrupt_ID) is
      Now             : Time;
      Time_Difference : Split_Time;
      Wakeup_Thread   : Threads.Thread_Id;

      use type Threads.Thread_States;

   begin
      --  Make sure we are handling the right interrupt and there is an alarm
      --  pending.

      pragma Assert
        (Pending_Alarm and then
         Interrupt = System.BB.Peripherals.General_Purpose_Timer);

      Peripherals.Clear_Alarm_Interrupt;

      Now := Clock;

      --  The access to the queues must be protected

      Protection.Enter_Kernel;

      --  Extract all the threads whose delay has expired

      while Threads.Queues.Get_Next_Alarm_Time <= Now loop

         --  Extract the task(s) that was waiting in the alarm queue and insert
         --  it in the ready queue.

         Wakeup_Thread := Threads.Queues.Extract_First_Alarm;

         --  We can only awake tasks that are delay statement

         pragma Assert (Wakeup_Thread.State = Threads.Delayed);

         Wakeup_Thread.State := Threads.Runnable;

         Threads.Queues.Insert (Wakeup_Thread);
      end loop;

      Time_Difference :=
        To_Split_Time (Threads.Queues.Get_Next_Alarm_Time - Now);

      --  if next alarm time is closer than a clock period then we need to
      --  program the timer. Otherwise we just need to signal that the timer
      --  has not been programed.

      if Time_Difference.MSP = 0 then

         --  The timer that we are setting is always in the future because we
         --  have previously checked that the the value of the alarm is
         --  strictly greater than the selected clock value.

         pragma Assert (Time_Difference.LSP > 0);

         Peripherals.Set_Alarm (Time_Difference.LSP);
      else
         Pending_Alarm := False;
      end if;

      --  We have finished the modifications to the queues

      Protection.Leave_Kernel;
   end Alarm_Handler;

   -----------
   -- Clock --
   -----------

   function Clock return Time is
      Result : Split_Time;

   begin
      --  The Least Significant Part of the Clock is hold in the hardware
      --  downcount register. Then, a call to this functions near to hardware
      --  clock interrupts could return a wrong Time.

      --  We take advantage of the atomicity of the interrupt handler (with
      --  respect to the caller). That is the reason why this function does not
      --  disable interrupts, and the reason why a task with a priority equal
      --  or greater than the clock interrupt may obtain erroneous clock
      --  values.

      --  Moreover, it must be note that if an interrupt occurs while time is
      --  built, the resulting time will be almost Clock_Interrupt_Period
      --  smaller than the corrected because MSP_Clock is read first.

      Result.MSP := MSP_Clock;
      Result.LSP := Peripherals.Read_Clock;

      if MSP_Clock > Result.MSP then

         --  A clock interrupt has occurred after reading Result.MSP. Hence,
         --  Result must be adjusted.

         Result.MSP := MSP_Clock;
         Result.LSP := Peripherals.Read_Clock;
      end if;

      return To_Time (Result);
   end Clock;

   -------------------
   -- Clock_Handler --
   -------------------

   procedure Clock_Handler (Interrupt : Interrupts.Interrupt_ID) is
      Time_Difference : Split_Time;

   begin
      --  Check that we are in the right handler

      pragma Assert (Interrupt = System.BB.Peripherals.Real_Time_Clock);

      Peripherals.Clear_Clock_Interrupt;

      --  The access to the queues must be protected

      Protection.Enter_Kernel;

      --  The clock timer has finished counting Timer_Interval'Last hardware
      --  clock ticks, so we increse the Most Significant Part of the Clock
      --  which is kept in memory.

      MSP_Clock := MSP_Clock + 1;

      if not Pending_Alarm then

         Time_Difference := To_Split_Time
           (Threads.Queues.Get_Next_Alarm_Time -
            To_Time (Split_Time'(MSP_Clock, Peripherals.Read_Clock)));

         --  If next alarm time is closer than a clock period then we need
         --  to program the alarm.

         if Time_Difference.MSP = 0 then

            --  Program the alarm at least one tick later

            Peripherals.Set_Alarm
              (Peripherals.Timer_Interval'Max (Time_Difference.LSP, 1));

            Pending_Alarm := True;
         end if;
      end if;

      Protection.Leave_Kernel;
   end Clock_Handler;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
      Now               : Time;
      Time_Difference   : Split_Time;
      Self              : Threads.Thread_Id;
      Inserted_As_First : Boolean;

   begin
      Now := Clock;

      Protection.Enter_Kernel;

      Self := Threads.Thread_Self;

      --  Test if the alarm time is in the future

      if T > Now then

         --  Extract the thread from the ready queue. When a thread wants
         --  to wait for an alarm it becomes blocked.

         Self.State := Threads.Delayed;

         Threads.Queues.Extract (Self);

         --  Insert Thread_Id in the alarm queue (ordered by time) and if it
         --  was inserted at head then check if Alarm Time is closer than the
         --  next clock interrupt.

         Threads.Queues.Insert_Alarm (T, Self, Inserted_As_First);

         if Inserted_As_First then
            Time_Difference := To_Split_Time (T - Now);

            --  Check whether the alarm time is within a clock period

            if Time_Difference.MSP = 0 then

               --  If so, set a new alarm and cancel the previous one if
               --  needed. The timer that we are setting is always in the
               --  future because we have previously checked that the the value
               --  of the alarm is strictly greater than the selected clock
               --  value.

               pragma Assert (Time_Difference.LSP > 0);

               if Pending_Alarm then
                  Peripherals.Cancel_And_Set_Alarm (Time_Difference.LSP);
               else
                  Peripherals.Set_Alarm (Time_Difference.LSP);
                  Pending_Alarm := True;
               end if;
            end if;
         end if;

      else
         --  If alarm time is not in the future, the thread must yield the CPU

         Threads.Queues.Yield (Self);
      end if;

      Protection.Leave_Kernel;
   end Delay_Until;

   -----------------------
   -- Initialize_Timers --
   -----------------------

   procedure Initialize_Timers is
   begin
      --  Install clock handler

      Interrupts.Attach_Handler
        (Clock_Handler'Access, System.BB.Peripherals.Real_Time_Clock);

      --  Install timer handler

      Interrupts.Attach_Handler
        (Alarm_Handler'Access, System.BB.Peripherals.General_Purpose_Timer);
   end Initialize_Timers;

end System.BB.Time;
