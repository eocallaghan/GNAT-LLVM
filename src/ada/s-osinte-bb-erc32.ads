------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                    S Y S T E M . O S _ I N T E R F A C E                 --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--          Copyright (C) 1995-2009, Free Software Foundation, Inc.         --
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

--  This is the Ravenscar version of this package for a bare board ERC32 target

--  This package encapsulates all direct interfaces to OS services that are
--  needed by the tasking run-time (libgnarl).

pragma Restrictions (No_Elaboration_Code);

with System.Parameters;
with System.BB.Threads;
with System.BB.Time;
with System.BB.Interrupts;
with System.BB.Peripherals;

package System.OS_Interface is
   pragma Preelaborate;

   package SBP renames System.BB.Peripherals;

   ----------------
   -- Interrupts --
   ----------------

   Max_Interrupt : constant := System.BB.Interrupts.Max_Interrupt;
   --  Number of asynchronous interrupts

   subtype Interrupt_ID is System.BB.Interrupts.Interrupt_ID;
   --  Interrupt identifiers

   No_Interrupt : constant := System.BB.Interrupts.No_Interrupt;
   --  Special value indicating no interrupt

   subtype Interrupt_Handler is System.BB.Interrupts.Interrupt_Handler;
   --  Interrupt handlers

   -------------------------
   -- External Interrupts --
   -------------------------

   External_Interrupt_4           : constant := SBP.External_Interrupt_4;
   External_Interrupt_4_Priority  : constant := Interrupt_Priority'First + 13;
   --  This interrupt has a Interrupt Level equal 14, that is
   --  System.Interrupt_Priority'Last - 1 or
   --  System.Interrupt_Priority'First + 13.

   External_Interrupt_3           : constant := SBP.External_Interrupt_3;
   External_Interrupt_3_Priority  : constant := Interrupt_Priority'First + 10;
   --  This interrupt has a Interrupt Level equal 11, that is
   --  System.Interrupt_Priority'Last - 4 or
   --  System.Interrupt_Priority'First + 10.

   External_Interrupt_2           : constant := SBP.External_Interrupt_2;
   External_Interrupt_2_Priority  : constant := Interrupt_Priority'First + 9;
   --  This interrupt has a Interrupt Level equal 10, that is
   --  System.Interrupt_Priority'Last - 5 or
   --  System.Interrupt_Priority'First + 9.

   External_Interrupt_1           : constant := SBP.External_Interrupt_1;
   External_Interrupt_1_Priority  : constant := Interrupt_Priority'First + 2;
   --  This interrupt has a Interrupt Level equal 3, that is
   --  System.Interrupt_Priority'Last - 12 or
   --  System.Interrupt_Priority'First + 2.

   External_Interrupt_0           : constant := SBP.External_Interrupt_0;
   External_Interrupt_0_Priority  : constant := Interrupt_Priority'First + 1;
   --  This interrupt has a Interrupt Level equal 2, that is
   --  System.Interrupt_Priority'Last - 13 or
   --  System.Interrupt_Priority'First + 1.

   -----------------------
   -- Timers Interrupts --
   -----------------------

   Watch_Dog_Time_Out             : constant := SBP.Watch_Dog_Time_Out;
   Watch_Dog_Time_Out_Priority    : constant := Interrupt_Priority'First + 14;
   --  This interrupt has a Interrupt Level equal 15, that is
   --  System.Interrupt_Priority'Last or
   --  System.Interrupt_Priority'First + 14.

   Real_Time_Clock                : constant := SBP.Real_Time_Clock;
   Real_Time_Clock_Priority       : constant := Interrupt_Priority'First + 12;
   --  This interrupt has a Interrupt Level equal 13, that is
   --  System.Interrupt_Priority'Last - 2 or
   --  System.Interrupt_Priority'First + 12.

   General_Purpose_Timer          : constant := SBP.General_Purpose_Timer;
   General_Purpose_Timer_Priority : constant := Interrupt_Priority'First + 11;
   --  This interrupt has a Interrupt Level equal 12, that is
   --  System.Interrupt_Priority'Last - 3 or
   --  System.Interrupt_Priority'First + 11.

   --------------------
   -- DMA Interrupts --
   --------------------

   DMA_Time_Out                   : constant := SBP.DMA_Time_Out;
   DMA_Time_Out_Priority          : constant := Interrupt_Priority'First + 8;
   --  This interrupt has a Interrupt Level equal 9, that is
   --  System.Interrupt_Priority'Last - 6 or
   --  System.Interrupt_Priority'First + 8.

   DMA_Access_Error               : constant := SBP.DMA_Access_Error;
   DMA_Access_Error_Priority      : constant := Interrupt_Priority'First + 7;
   --  This interrupt has a Interrupt Level equal 8, that is
   --  System.Interrupt_Priority'Last - 7 or
   --  System.Interrupt_Priority'First + 7.

   ---------------------
   -- UART Interrupts --
   ---------------------

   UART_Error                     : constant := SBP.UART_Error;
   UART_Error_Priority            : constant := Interrupt_Priority'First + 6;
   --  This interrupt has a Interrupt Level equal 7, that is
   --  System.Interrupt_Priority'Last - 8 or
   --  System.Interrupt_Priority'First + 6.

   UART_B_Ready                   : constant := SBP.UART_B_Ready;
   UART_B_Ready_Priority          : constant := Interrupt_Priority'First + 4;
   --  This interrupt has a Interrupt Level equal 5, that is
   --  System.Interrupt_Priority'Last - 10 or
   --  System.Interrupt_Priority'First + 4.

   UART_A_Ready                   : constant := SBP.UART_A_Ready;
   UART_A_Ready_Priority          : constant := Interrupt_Priority'First + 3;
   --  This interrupt has a Interrupt Level equal 4, that is
   --  System.Interrupt_Priority'Last - 11 or
   --  System.Interrupt_Priority'First + 3.

   -----------------------------
   -- Miscelaneous Interrupts --
   -----------------------------

   Correctable_Error_In_Memory  : constant := SBP.Correctable_Error_In_Memory;
   Correctable_Error_In_Memory_Priority : constant :=
     Interrupt_Priority'First + 5;
   --  This interrupt has a Interrupt Level equal 6, that is
   --  System.Interrupt_Priority'Last - 9 or
   --  System.Interrupt_Priority'First + 5.

   Masked_Hardware_Errors         : constant := SBP.Masked_Hardware_Errors;
   Masked_Hardware_Errors_Priority : constant := Interrupt_Priority'First;
   --  This interrupt has a Interrupt Level equal 1, that is
   --  System.Interrupt_Priority'Last - 14 or
   --  System.Interrupt_Priority'First.

   --------------------------
   -- Interrupt processing --
   --------------------------

   function Current_Interrupt return Interrupt_ID
     renames System.BB.Interrupts.Current_Interrupt;
   --  Function that returns the hardware interrupt currently being
   --  handled (if any). In case no hardware interrupt is being handled
   --  the returned value is No_Interrupt.

   function Priority_Of_Interrupt (Id : Interrupt_ID) return Any_Priority
     renames System.BB.Interrupts.Priority_Of_Interrupt;
   --  Obtain the software priority of any hardware interrupt. This makes
   --  easier the selection of the priority of the protected handler
   --  attached to interrupts.

   procedure Attach_Handler
     (Handler : Interrupt_Handler;
      Id      : Interrupt_ID) renames System.BB.Interrupts.Attach_Handler;
   --  Attach a handler to a hardware interrupt

   ----------
   -- Time --
   ----------

   subtype Time is System.BB.Time.Time;
   --  Representation of the time in the underlying tasking system

   subtype Time_Span is System.BB.Time.Time_Span;
   --  Represents the length of time intervals in the underlying tasking
   --  system.

   Ticks_Per_Second : constant Natural;
   pragma Import (Asm, Ticks_Per_Second, "clock_frequency");
   --  Number of ticks (or clock interrupts) per second

   function Clock return Time renames System.BB.Time.Clock;
   --  Get the number of ticks elapsed since startup

   procedure Delay_Until (T : Time) renames System.BB.Time.Delay_Until;
   --  Suspend the calling task until the absolute time specified by T

   -------------
   -- Threads --
   -------------

   subtype Thread_Descriptor is System.BB.Threads.Thread_Descriptor;
   --  Type that contains the information about a thread (registers,
   --  priority, etc.).

   subtype Thread_Id is System.BB.Threads.Thread_Id;
   --  Identifiers for the underlying threads

   Null_Thread_Id : constant Thread_Id := null;
   --  Identifier for a non valid thread

   Lwp_Self : constant System.Address := Null_Address;
   --  LWP is not used by gdb on ERC32

   procedure Initialize
     (Environment_Thread : Thread_Id;
      Main_Priority      : System.Any_Priority)
     renames System.BB.Threads.Initialize;
   --  Procedure for initializing the underlying tasking system

   procedure Thread_Create
     (Id            : Thread_Id;
      Code          : System.Address;
      Arg           : System.Address;
      Priority      : System.Any_Priority;
      Stack_Address : System.Address;
      Stack_Size    : System.Parameters.Size_Type)
     renames System.BB.Threads.Thread_Create;
   --  Create a new thread

   function Thread_Self return Thread_Id renames System.BB.Threads.Thread_Self;
   --  Return the thread identifier for the calling task

   ----------
   -- ATCB --
   ----------

   procedure Set_ATCB (ATCB : System.Address)
     renames System.BB.Threads.Set_ATCB;
   --  Associate the specified ATCB to the currently running thread

   function Get_ATCB return System.Address renames System.BB.Threads.Get_ATCB;
   --  Get the ATCB associated to the currently running thread

   ----------------
   -- Scheduling --
   ----------------

   procedure Set_Priority (Priority  : System.Any_Priority)
     renames System.BB.Threads.Set_Priority;
   --  Set the active priority of the executing thread to the given value

   function Get_Priority  (Id : Thread_Id) return System.Any_Priority
     renames System.BB.Threads.Get_Priority;
   --  Get the current base priority of a thread

   procedure Sleep renames System.BB.Threads.Sleep;
   --  The calling thread is unconditionally suspended

   procedure Wakeup (Id : Thread_Id) renames System.BB.Threads.Wakeup;
   --  The referred thread becomes ready (the thread must be suspended)

end System.OS_Interface;
