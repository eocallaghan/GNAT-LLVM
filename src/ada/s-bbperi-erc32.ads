------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                  S Y S T E M . B B . P E R I P H E R A L S               --
--                                                                          --
--                                  S p e c                                 --
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

--  This package defines constants and primitives used for handling the
--  peripherals available in the target board.

--  This is the ERC32 version of this package

pragma Restrictions (No_Elaboration_Code);

with System;
with System.BB.Parameters;

package System.BB.Peripherals is
   pragma Preelaborate;

   package SBP renames System.BB.Parameters;

   -----------------------------
   -- Hardware initialization --
   -----------------------------

   procedure Initialize_Board;
   --  Procedure that performs the hardware initialization of the board.
   --  Should be called before any other operations in this package.

   ------------------------------------------------
   -- Clock and timer definitions and primitives --
   ------------------------------------------------

   type Timer_Interval is mod 2 ** 32;
   for Timer_Interval'Size use 32;
   --  This type represents any interval that we can measure within a
   --  Clock_Interrupt_Period.

   procedure Set_Alarm (Ticks : Timer_Interval);
   --  Set an alarm that will expire after the specified number of clock ticks

   procedure Cancel_And_Set_Alarm (Ticks : Timer_Interval);
   --  Set a new alarm that will expire after the specified number of clock
   --  ticks, and cancel any previous alarm set.

   function Read_Clock return Timer_Interval;
   --  Read the 32-bit value contained in the clock hardware counter, and
   --  return the number of ticks elapsed since the last clock interrupt, that
   --  is, since the clock counter was last reloaded.

   procedure Clear_Alarm_Interrupt;
   pragma Inline (Clear_Alarm_Interrupt);
   --  Acknowledge the alarm interrupt

   procedure Clear_Clock_Interrupt;
   pragma Inline (Clear_Clock_Interrupt);
   --  Acknowledge the clock interrupt

   ----------------
   -- Interrupts --
   ----------------

   function To_Vector
     (Level : SBP.Interrupt_Level) return SBP.Range_Of_Vector;
   pragma Inline (To_Vector);
   --  Function to translate interrupt levels into interrupt vector entries

   function To_Interrupt
     (Vector : SBP.Range_Of_Vector) return SBP.Interrupt_Level;
   pragma Inline (To_Interrupt);
   --  Function to translate interrupt vector entries into their
   --  corresponding interrupt level. If the trap does not correspond
   --  to an external interrupt (that is, if it is a synchronous trap)
   --  then interrupt level 0 is returned.

   function Priority_Of_Interrupt
     (Level : SBP.Interrupt_Level) return System.Any_Priority;
   pragma Inline (Priority_Of_Interrupt);
   --  Function to obtain the priority associated to an interrupt. It returns
   --  System.Any_Priority'First if Level is equal to zero (no interrupt).

   --  Constants defining the external interrupts

   Watch_Dog_Time_Out          : constant SBP.Interrupt_Level := 15;
   External_Interrupt_4        : constant SBP.Interrupt_Level := 14;
   Real_Time_Clock             : constant SBP.Interrupt_Level := 13;
   General_Purpose_Timer       : constant SBP.Interrupt_Level := 12;
   External_Interrupt_3        : constant SBP.Interrupt_Level := 11;
   External_Interrupt_2        : constant SBP.Interrupt_Level := 10;
   DMA_Time_Out                : constant SBP.Interrupt_Level := 9;
   DMA_Access_Error            : constant SBP.Interrupt_Level := 8;
   UART_Error                  : constant SBP.Interrupt_Level := 7;
   Correctable_Error_In_Memory : constant SBP.Interrupt_Level := 6;
   UART_B_Ready                : constant SBP.Interrupt_Level := 5;
   UART_A_Ready                : constant SBP.Interrupt_Level := 4;
   External_Interrupt_1        : constant SBP.Interrupt_Level := 3;
   External_Interrupt_0        : constant SBP.Interrupt_Level := 2;
   Masked_Hardware_Errors      : constant SBP.Interrupt_Level := 1;

   --------------------
   -- Output Console --
   --------------------

   procedure Initialize_Console;
   pragma Inline (Initialize_Console);
   --  Initialize the UART to be used as output console

   procedure Console_Send (Char : Character);
   pragma Inline (Console_Send);
   --  Procedure to send Characters to the UART used as output console

end System.BB.Peripherals;
