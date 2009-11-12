------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                  S Y S T E M . B B . P E R I P H E R A L S               --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2006 The European Space Agency            --
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

with System.BB.Peripherals.Registers;

package body System.BB.Peripherals is

   use type Registers.Scaler_12;
   use type Registers.Scaler_10;
   use type Registers.Timers_Counter;

   -----------------------
   -- Local Definitions --
   -----------------------

   Prescaler_Min : constant := 3;
   --  In order to obtain the highest granularity of the clock we set the
   --  minimum allowed prescaler division factor, which is 4, corresponding
   --  to a prescaler reload register value of 3.

   Periodic_Count : constant := Timer_Interval'Last - 1;
   --  Value to be loaded in the clock counter to accomplish the
   --  Clock_Interrupt_Period.
   --
   --  One is subtracted from Timers_Counter'Last because the timeout period
   --  will count an extra cycle for reloading the counter.

   subtype UART_Baudrate is Natural;
   --  The range of baudrates supported by the UART depends on the range of
   --  the scaler, which is a 12-bit value ranging from 0 to 2 ** 12 - 1, and
   --  on the system clock frequency.

   type UART_Parity is (None, Even, Odd);
   --  Parity values

   type UART_Flow_Control is (Off, On);
   --  Flow control using CTS/RTS

   type UART_Channel is (UART1, UART2);
   --  LEON implements two UARTs

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Initialize_Memory;
   pragma Inline (Initialize_Memory);
   --  Initialize the memory on the board

   procedure Initialize_Clock;
   --  Perform all the initialization related to the clock

   procedure Initialize_UART
     (Channel      : UART_Channel;
      Baudrate     : UART_Baudrate;
      Parity       : UART_Parity;
      Flow_Control : UART_Flow_Control);
   --  Procedure to initialize the UART port

   --------------------------
   -- Cancel_And_Set_Alarm --
   --------------------------

   procedure Cancel_And_Set_Alarm (Ticks : Timer_Interval) renames Set_Alarm;
   --  This procedure cancel a previous alarm and set a new one.
   --  Setting a new alarm cancel the previous one in this target
   --  So Cancel_And_Set_Alarm and Set_Alarm are identical.

   --------------------------
   -- Clear_Alarm_Interupt --
   --------------------------

   procedure Clear_Alarm_Interrupt is
   begin
      --  Interrupts are cleared automatically when they are acknowledged

      null;
   end Clear_Alarm_Interrupt;

   --------------------------
   -- Clear_Clock_Interupt --
   --------------------------

   procedure Clear_Clock_Interrupt is
      Watchdog_Reset : constant Registers.Timer_Register :=
        (Timer_Value => Registers.Timers_Counter'Last,
         Reserved    => (others => False));
      --  Maximum reload value to avoid watchdog timeouts

   begin
      --  Interrupts are cleared automatically when they are acknowledged.
      --  We only reset the watchdog timer to prevent timeouts.

      Registers.Watchdog_Register := Watchdog_Reset;
   end Clear_Clock_Interrupt;

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
   begin
      --  The initialization of the LEON board consists on initializing the
      --  memory, and initializing the clock in order to have the desired
      --  granularity and range.

      Initialize_Memory;
      Initialize_Clock;
   end Initialize_Board;

   ----------------------
   -- Initialize_Clock --
   ----------------------

   procedure Initialize_Clock is
      Prescaler               : constant Registers.Prescaler_Register :=
        (Value => Prescaler_Min, Reserved => (others => False));
      --  Minimum prescaler to be used to achieve best granularity

      Real_Time_Clock_Reload  : constant Registers.Timer_Register :=
        (Timer_Value => Periodic_Count, Reserved => (others => False));
      --  Periodic count to be used for the clock

      Real_Time_Clock_Control : constant Registers.Timer_Control_Register :=
        (Enable => True, Reload_Counter => True,
         Load_Counter => True, Reserved => (others => False));
      --  Program the timer in periodic mode to serve as a clock

      Watchdog_Reset : constant Registers.Timer_Register :=
        (Timer_Value => Registers.Timers_Counter'Last,
         Reserved    => (others => False));
      --  Maximum reload value to avoid watchdog timeouts

      Interrupt_Mask_Aux      : Registers.Interrupt_Mask_and_Priority_Register;

   begin
      --  Reset the watchdog timer

      Registers.Watchdog_Register := Watchdog_Reset;

      --  Set the prescaler value to achieve the required granularity

      Registers.Prescaler_Reload := Prescaler;

      --  Load the counter for the real-time clock

      Registers.Timer_2_Reload := Real_Time_Clock_Reload;

      --  Enable Timer 2

      Registers.Timer_2_Control := Real_Time_Clock_Control;

      --  Enable clock interrupts

      Interrupt_Mask_Aux := Registers.Interrupt_Mask_and_Priority;
      Interrupt_Mask_Aux.Timer_2 := True;
      Registers.Interrupt_Mask_and_Priority := Interrupt_Mask_Aux;
   end Initialize_Clock;

   ------------------------
   -- Initialize_Console --
   ------------------------

   procedure Initialize_Console is
   begin
      --  Initialize the UART1 as output console

      Initialize_UART (UART1, 115200, None, Off);
   end Initialize_Console;

   -----------------------
   -- Initialize_Memory --
   -----------------------

   procedure Initialize_Memory is
   begin
      --  Nothing to be done for LEON

      null;
   end Initialize_Memory;

   ---------------------
   -- Initialize_UART --
   ---------------------

   procedure Initialize_UART
     (Channel      : UART_Channel;
      Baudrate     : UART_Baudrate;
      Parity       : UART_Parity;
      Flow_Control : UART_Flow_Control)
   is
      Control_Aux : Registers.UART_Control_Register;

   begin
      --  Read the Control Register

      case Channel is
         when UART1 =>
            Control_Aux := Registers.UART_1_Control;

         when UART2 =>
            Control_Aux := Registers.UART_2_Control;
      end case;

      --  Check if the UART has been already initialized (for example, by the
      --  remote monitor).

      if not Control_Aux.TE and then not Control_Aux.RE then
         declare
            Scaler_Aux : Registers.UART_Scaler_Register;

         begin
            --  Set the UART scaler according to the baudrate given

            Scaler_Aux :=
              (UART_Scaler =>
                 Registers.Scaler_12
                   ((SBP.Clock_Frequency * 10 / (Baudrate * 8) - 5) / 10),
               Reserved    => (others => False));

            --  Enable TX and RX and disable interrupts

            Control_Aux.RE := True;
            Control_Aux.TE := True;

            Control_Aux.RI := False;
            Control_Aux.TI := False;

            --  Set the requested parity checking

            case Parity is
               when None =>
                  Control_Aux.PE := False;
                  Control_Aux.PS := False;

               when Even =>
                  Control_Aux.PE := True;
                  Control_Aux.PS := False;

               when Odd  =>
                  Control_Aux.PE := True;
                  Control_Aux.PS := True;
            end case;

            --  Set the requested flow control

            case Flow_Control is
               when Off =>
                  Control_Aux.FL := False;

               when On  =>
                  Control_Aux.FL := True;
            end case;

            --  Use the system clock for synchronization

            Control_Aux.EC := False;

            --  Disable loop back mode

            Control_Aux.LB := False;

            --  Set the reserved bits to False

            Control_Aux.Reserved := (others => False);

            --  Write to the Control and Scaler Registers

            case Channel is
            when UART1 =>
               Registers.UART_1_Control := Control_Aux;
               Registers.UART_1_Scaler  := Scaler_Aux;

            when UART2 =>
               Registers.UART_2_Control := Control_Aux;
               Registers.UART_2_Scaler  := Scaler_Aux;
            end case;
         end;
      end if;
   end Initialize_UART;

   ---------------------------
   -- Priority_Of_Interrupt --
   ---------------------------

   function Priority_Of_Interrupt
     (Level : SBP.Interrupt_Level) return System.Any_Priority
   is
   begin
      --  Assert that it is a real interrupt

      pragma Assert (Level /= 0);

      return (Any_Priority (Level) + Interrupt_Priority'First - 1);
   end Priority_Of_Interrupt;

   ----------------
   -- Read_Clock --
   ----------------

   function Read_Clock return Timer_Interval is
   begin
      return Timer_Interval
        (Periodic_Count - Registers.Timer_2_Counter.Timer_Value);
   end Read_Clock;

   ---------------
   -- Set_Alarm --
   ---------------

   procedure Set_Alarm (Ticks : Timer_Interval) is
      Timer_Reload_Aux   : constant Registers.Timer_Register :=
        (Timer_Value => Registers.Timers_Counter (Ticks),
         Reserved => (others => False));
      --  Load the required ticks

      Timer_Control_Aux : constant Registers.Timer_Control_Register :=
        (Enable => True, Reload_Counter => False,
         Load_Counter => True, Reserved => (others => False));
      --  Program the timer in one-shot mode

      Interrupt_Mask_Aux : Registers.Interrupt_Mask_and_Priority_Register;

   begin
      --  Alarm Clock downcount will reach 0 in Ticks. The granularity of
      --  time intervals is equal to Clock Period.

      --  Set the prescaler: already done in Initialize_Clock

      --  Load the counter

      Registers.Timer_1_Reload := Timer_Reload_Aux;

      --  Write Timer Control Register

      Registers.Timer_1_Control := Timer_Control_Aux;

      --   Enable Timer 1 Interrupts
      Interrupt_Mask_Aux := Registers.Interrupt_Mask_and_Priority;
      Interrupt_Mask_Aux.Timer_1 := True;
      Registers.Interrupt_Mask_and_Priority := Interrupt_Mask_Aux;
   end Set_Alarm;

   ----------------------
   -- Ticks_Per_Second --
   ----------------------

   function Ticks_Per_Second return Natural is
   begin
      --  The prescaler is clocked by the system clock. When it underflows, it
      --  is reloaded from the prescaler reload register and a timer tick is
      --  generated. The effective division rate is therefore equal to the
      --  prescaler reload register value plus 1.

      return System.BB.Parameters.Clock_Frequency / (Prescaler_Min + 1);
   end Ticks_Per_Second;

   ------------------
   -- To_Interrupt --
   ------------------

   function To_Interrupt
     (Vector : SBP.Range_Of_Vector) return SBP.Interrupt_Level
   is
   begin
      --  The range corresponding to asynchronous traps is between
      --  16#11# and 16#1F#.

      pragma Assert (Vector >= 16#11# and then Vector <= 16#1F#);

      return Vector - 16#10#;
   end To_Interrupt;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (Level : SBP.Interrupt_Level) return SBP.Range_Of_Vector
   is
   begin
      return Level + 16#10#;
   end To_Vector;

   ------------------
   -- Console_Send --
   ------------------

   procedure Console_Send (Char    : Character)
   is
      UART_Tx         : constant Registers.UART_Data_Register :=
        (RTD => Char, Reserved => (others => False));

      UART_Status_Aux : Registers.UART_Status_Register;
   begin
      --  Send the character through the selected channel by polling

      --  Wait until the port is ready to load data

      loop
         UART_Status_Aux := Registers.UART_1_Status;
         exit when UART_Status_Aux.TH;
      end loop;

      --  Send the character once the UART is ready

      Registers.UART_1_Data := UART_Tx;

   end Console_Send;

end System.BB.Peripherals;
