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

   use type Registers.Scaler_8;
   use type Registers.Timers_Counter;

   -----------------------
   -- Local Definitions --
   -----------------------

   Periodic_Scaler : constant := 0;
   --  In order to obtain the highest granularity of the clock we set the
   --  scaler to 0.

   Alarm_Scaler : constant := 0;
   --  In order to obtain the highest resolution of the alarm timer we set
   --  the scaler to 0.

   Periodic_Count : constant := Registers.Timers_Counter'Last - 1;
   --  Value to be loaded in the clock counter to accomplish the
   --  Clock_Interrupt_Period.
   --
   --  One is subtracted from Timers_Counter'Last because when the Scaler is
   --  set to 0, the timeout period will be the counter reload value  plus 1.

   Timer_Control_Mirror : Registers.Timer_Control_Register;
   pragma Volatile (Timer_Control_Mirror);
   --  Timer_Control register cannot be read. So the following object holds a
   --  copy of the Timer_Control register value.

   subtype UART_Baudrate is Natural;
   --  The range of baudrates supported by the UART depends on the range of
   --  the scaler, which is a 12-bit value ranging from 0 to 2 ** 12 - 1, and
   --  on the system clock frequency.

   type UART_Parity is (None, Even, Odd);
   --  Parity values

   type UART_Stop_Bits is (One, Two);
   --  Stop bits values

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Stop_Watch_Dog;
   pragma Inline (Stop_Watch_Dog);
   --  Stop the watch dog timer

   procedure Initialize_Memory;
   pragma Inline (Initialize_Memory);
   --  Initialize the memory on the board

   procedure Initialize_Clock;
   --  Perform all the initialization related to the clock

   procedure Initialize_UART
     (Baudrate  : UART_Baudrate;
      Parity    : UART_Parity;
      Stop_Bits : UART_Stop_Bits);
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
      --  From MEC Specification Document (MCD/SPC/0009/SE) page 35

      --  The MEC includes a specific register called Interrupt Pending
      --  Register, which reflects the pending interrupts.

      --  The interrupts in the IPR are cleared automatically when the
      --  interrupt is acknowledged. The MEC will sample the trap address in
      --  order to know which bit to clear. Therefore, this procedure has a
      --  null body for this target.

      null;
   end Clear_Alarm_Interrupt;

   --------------------------
   -- Clear_Clock_Interupt --
   --------------------------

   procedure Clear_Clock_Interrupt is
   begin
      --  From MEC Specification Document (MCD/SPC/0009/SE) page 35

      --  The MEC includes a specific register called Interrupt Pending
      --  Register, which reflects the pending interrupts.

      --  The interrupts in the IPR are cleared automatically when the
      --  interrupt is acknowledged. The MEC will sample the trap address in
      --  order to know which bit to clear. Therefore, this procedure has a
      --  null body for this target.

      null;
   end Clear_Clock_Interrupt;

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
   begin
      --  The initialization of the ERC32 board consists on stopping the watch
      --  dog timer, initializing the memory, and initializing the clock in
      --  order to have the desired granularity and range.

      Stop_Watch_Dog;
      Initialize_Memory;
      Initialize_Clock;
   end Initialize_Board;

   ----------------------
   -- Initialize_Clock --
   ----------------------

   procedure Initialize_Clock is
      Real_Time_Clock_Scaler_Aux : Registers.Real_Time_Clock_Scaler_Register;
      Interrupt_Mask_Aux         : Registers.Interrupt_Mask_Register;

   begin
      --  Set the scaler for the clock

      Real_Time_Clock_Scaler_Aux       := Registers.Real_Time_Clock_Scaler;
      Real_Time_Clock_Scaler_Aux.RTCS  := Periodic_Scaler;
      Registers.Real_Time_Clock_Scaler := Real_Time_Clock_Scaler_Aux;

      --  Load the counter for the clock

      Registers.Real_Time_Clock_Counter := Periodic_Count;

      --  Set the proper bits in mirrored Timer Control Register.
      --  The timer used for the clock is programmed in periodic mode.

      --  From MEC Specification Document (MCD/SPC/0009/SE) page 50.
      --  NOTE !! All reserved bits have to be written with zeros in order
      --  to avoid parity error resulting in a MEC internal error.

      Timer_Control_Mirror.Reserved4  := (others => False);
      Timer_Control_Mirror.Reserved20 := (others => False);

      Timer_Control_Mirror.RTCCR := True;
      Timer_Control_Mirror.RTCCL := True;
      Timer_Control_Mirror.RTCSL := True;
      Timer_Control_Mirror.RTCSE := True;

      --  Do not modify General Purpose Timer downcounter

      Timer_Control_Mirror.GCL := False;
      Timer_Control_Mirror.GSL := False;

      --  Write MEC Timer Control Register

      Registers.Timer_Control := Timer_Control_Mirror;

      --  Enable clock interrupts

      Interrupt_Mask_Aux := Registers.Interrupt_Mask;
      Interrupt_Mask_Aux.Real_Time_Clock := False;
      Registers.Interrupt_Mask := Interrupt_Mask_Aux;
   end Initialize_Clock;

   ------------------------
   -- Initialize_Console --
   ------------------------

   procedure Initialize_Console is
   begin
      --  Initialize the UARTs

      Initialize_UART (19200, None, One);
   end Initialize_Console;

   -----------------------
   -- Initialize_Memory --
   -----------------------

   procedure Initialize_Memory is
   begin
      --  Nothing to be done for the ERC32

      null;
   end Initialize_Memory;

   ---------------------
   -- Initialize_UART --
   ---------------------

   procedure Initialize_UART
     (Baudrate  : UART_Baudrate;
      Parity    : UART_Parity;
      Stop_Bits : UART_Stop_Bits)
   is
      Control_Aux : Registers.Control_Register;

   begin
      --  Read the MEC Control Register

      Control_Aux := Registers.Control;

      --  Check if the UART was already initialized by the remote monitor

      if Control_Aux.UART_Scaler = 0
        and then Control_Aux.UCS
        and then Control_Aux.UP
        and then Control_Aux.UPE
        and then not Control_Aux.UBR
      then
         declare
            Scaler_Aux : Registers.Scaler_8;

         begin
            --  Set the UART scaler according to the baudrate given

            Scaler_Aux :=
              Registers.Scaler_8 (SBP.Clock_Frequency / (32 * Baudrate * 2));

            if Scaler_Aux <= 1 then
               Control_Aux.UART_Scaler := Registers.Scaler_8
                 (SBP.Clock_Frequency / (32 * Baudrate) - 1);
               Control_Aux.UBR := True;
            else
               Control_Aux.UART_Scaler := Scaler_Aux - 1;
               Control_Aux.UBR := False;
            end if;

            --  Set the correct bits for parity checking

            case Parity is
               when None =>
                  Control_Aux.UPE := False;

               when Even =>
                  Control_Aux.UPE := True;
                  Control_Aux.UP  := False;

               when Odd  =>
                  Control_Aux.UPE := True;
                  Control_Aux.UP  := True;
            end case;

            --  Use one or two stop bits per character

            case Stop_Bits is
               when One =>
                  Control_Aux.USB := False;

               when Two =>
                  Control_Aux.USB := True;
            end case;

            --  Use the system clock for synchronization

            Control_Aux.UCS := True;

            --  Write to the Control Register in the MEC

            Registers.Control := Control_Aux;
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
        (Periodic_Count - Registers.Real_Time_Clock_Counter);
   end Read_Clock;

   ---------------
   -- Set_Alarm --
   ---------------

   procedure Set_Alarm (Ticks : Timer_Interval) is
      General_Purpose_Timer_Scaler_Aux :
        Registers.General_Purpose_Timer_Scaler_Register;

      Interrupt_Mask_Aux : Registers.Interrupt_Mask_Register;

   begin
      --  Alarm Clock downcount will reach 0 in Ticks. The granularity of
      --  time intervals is equal to Clock Period.

      --  Set the scaler

      General_Purpose_Timer_Scaler_Aux :=
        Registers.General_Purpose_Timer_Scaler;
      General_Purpose_Timer_Scaler_Aux.GPTS := Alarm_Scaler;
      Registers.General_Purpose_Timer_Scaler :=
        General_Purpose_Timer_Scaler_Aux;

      --  Load the counter

      Registers.General_Purpose_Timer_Counter :=
        Registers.Timers_Counter (Ticks);

      --  Set the proper bits in mirrored Timer Control Register.
      --  General Purpose Timer is used in one-shot mode.

      Timer_Control_Mirror.GCR := False;

      Timer_Control_Mirror.GCL := True;
      Timer_Control_Mirror.GSE := True;
      Timer_Control_Mirror.GSL := True;

      --  Do not modify Timer downcount

      Timer_Control_Mirror.RTCCL := False;
      Timer_Control_Mirror.RTCSL := False;

      --  From MEC Specification Document (MCD/SPC/0009/SE) page 50
      --  NOTE !! All reserved bits have to be written with zeros in order
      --  to avoid parity error resulting in a MEC internal error.

      Timer_Control_Mirror.Reserved4 := (others => False);
      Timer_Control_Mirror.Reserved20 := (others => False);

      --  Write MEC Timer Control Register

      Registers.Timer_Control := Timer_Control_Mirror;

      --  Enable GPT Interrupts

      Interrupt_Mask_Aux := Registers.Interrupt_Mask;
      Interrupt_Mask_Aux.General_Purpose_Timer := False;
      Registers.Interrupt_Mask := Interrupt_Mask_Aux;
   end Set_Alarm;

   --------------------
   -- Stop_Watch_Dog --
   --------------------

   procedure Stop_Watch_Dog is
   begin
      --  From MEC Specification Document (MCD/SPC/0009/SE) page 39

      --  After system reset or processor reset, the watch dog timer is enabled
      --  and starts running. By writing to the Trap Door Set after system
      --  reset, the timer can be disabled.

      Registers.Watchdog_Trap_Door_Set := 0;
   end Stop_Watch_Dog;

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

   procedure Console_Send (Char : Character) is
      UART_Tx         : Registers.UART_Channel_Rx_Tx_Register;
      UART_Status_Aux : Registers.UART_Status_Register;

   begin
      --  Set the Character that we want to send through the port

      UART_Tx.RTD := Char;

      --  From MEC Specification Document (MCD/SPC/0009/SE) page 50

      --  NOTE !! All reserved bits have to be written with zeros in order
      --  to avoid parity error resulting in a MEC internal error.

      UART_Tx.Reserved24 := (others => False);

      --  Send the character through the selected channel (A) by polling

      loop
         UART_Status_Aux := Registers.UART_Status;
         exit when UART_Status_Aux.THEA;
      end loop;

      Registers.UART_Channel_A := UART_Tx;
   end Console_Send;

end System.BB.Peripherals;
