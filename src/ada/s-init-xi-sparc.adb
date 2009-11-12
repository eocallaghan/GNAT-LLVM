------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                             S Y S T E M . I N I T                        --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--          Copyright (C) 2003-2008, Free Software Foundation, Inc.         --
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

--  This is a bare board implementation of this body for SPARC (V7/V8) targets

--  This file should be kept synchronized with 2sinit.ads, 2sinit.adb,
--  s-init-ae653-cert.adb, and init.c. All these files implement the required
--  functionality for different targets.

with System.BB.Parameters;
with System.BB.CPU_Primitives;
with System.Tasking;

package body System.Init is

   use System.BB.Parameters;

   ----------------------
   -- Trap definitions --
   ----------------------

   Instruction_Access_Exception : constant := 16#01#;
   Illegal_Instruction          : constant := 16#02#;
   Address_Not_Aligned          : constant := 16#07#;
   FP_Exception                 : constant := 16#08#;
   Data_Access_Exception        : constant := 16#09#;
   Instruction_Access_Error     : constant := 16#21#;
   Data_Access_Error            : constant := 16#29#;
   Division_By_Zero             : constant := 16#2A#;
   Data_Store_Error             : constant := 16#2B#;

   -------------------------------
   --  Binder Generated Values  --
   -------------------------------

   Gl_Time_Slice_Val : Integer := -1;
   pragma Export (C, Gl_Time_Slice_Val, "__gl_time_slice_val");

   Gl_Wc_Encoding : Character := 'n';
   pragma Export (C, Gl_Wc_Encoding, "__gl_wc_encoding");

   Gl_Locking_Policy : Character := ' ';
   pragma Export (C, Gl_Locking_Policy, "__gl_locking_policy");

   Gl_Queuing_Policy : Character := ' ';
   pragma Export (C, Gl_Queuing_Policy, "__gl_queuing_policy");

   Gl_Task_Dispatching_Policy : Character := ' ';
   pragma Export (C, Gl_Task_Dispatching_Policy,
                  "__gl_task_dispatching_policy");

   Gl_Priority_Specific_Dispatching : Address := Null_Address;
   pragma Export (C, Gl_Priority_Specific_Dispatching,
                     "__gl_priority_specific_dispatching");

   Gl_Num_Specific_Dispatching : Integer := 0;
   pragma Export (C, Gl_Num_Specific_Dispatching,
                  "__gl_num_specific_dispatching");

   Gl_Restrictions : Address := Null_Address;
   pragma Export (C, Gl_Restrictions, "__gl_restrictions");

   Gl_Interrupt_States : Address := Null_Address;
   pragma Export (C, Gl_Interrupt_States, "__gl_interrupt_states");

   Gl_Num_Interrupt_States : Integer := 0;
   pragma Export (C, Gl_Num_Interrupt_States, "__gl_num_interrupt_states");

   Gl_Unreserve_All_Interrupts : Integer := 0;
   pragma Export (C, Gl_Unreserve_All_Interrupts,
                  "__gl_unreserve_all_interrupts");

   Gl_Exception_Tracebacks : Integer := 0;
   pragma Export (C, Gl_Exception_Tracebacks, "__gl_exception_tracebacks");

   Gl_Zero_Cost_Exceptions : Integer := 0;
   pragma Export (C, Gl_Zero_Cost_Exceptions, "__gl_zero_cost_exceptions");

   Gl_Detect_Blocking : Integer := 0;
   pragma Export (C, Gl_Detect_Blocking, "__gl_detect_blocking");

   Gl_Default_Stack_Size : Integer := 0;
   pragma Export (C, Gl_Default_Stack_Size, "__gl_default_stack_size");

   Gl_Leap_Seconds_Support : Integer := 0;
   pragma Export (C, Gl_Leap_Seconds_Support, "__gl_leap_seconds_support");

   Handler_Installed : Integer := 0;
   pragma Export (C, Handler_Installed, "__gnat_handler_installed");
   --  Indication of whether synchronous signal handlers have already been
   --  installed by a previous call to Install_Handler.

   ------------------------
   --  Local procedures  --
   ------------------------

   procedure GNAT_Error_Handler (Trap : Range_Of_Vector);
   --  Common procedure that is executed when a trap that needs to be
   --  translated into an exception is captured.

   procedure Initialize;
   pragma Export (C, Initialize, "__gnat_initialize");

   procedure Finalize;
   pragma Export (C, Finalize, "__gnat_finalize");

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      null;
   end Finalize;

   ------------------------
   -- GNAT_Error_Handler --
   ------------------------

   procedure GNAT_Error_Handler (Trap : Range_Of_Vector) is
   begin
      case Trap is
         when Instruction_Access_Exception =>
            raise Storage_Error with "instruction access exception";
         when Illegal_Instruction =>
            raise Constraint_Error with "illegal instruction";
         when Address_Not_Aligned =>
            raise Constraint_Error with "address not aligned";
         when FP_Exception =>
            raise Constraint_Error with "floating point exception";
         when Data_Access_Exception =>
            raise Constraint_Error with "data access exception";
         when Instruction_Access_Error =>
            raise Constraint_Error with "instruction access exception";
         when Data_Access_Error =>
            raise Constraint_Error with "data access error";
         when Division_By_Zero =>
            raise Constraint_Error with "division by zero";
         when Data_Store_Error =>
            raise Constraint_Error with "data store error";
         when others =>
            raise Program_Error with "unhandled trap";
      end case;
   end GNAT_Error_Handler;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Handler is
   begin
      --  Ensure that the tasking run time is initialized when using this run
      --  time. This initialization is required by the support for exceptions
      --  (which uses thread local storage). The initialization routine has the
      --  required machinery to prevent multiple calls to Initialize.

      System.Tasking.Initialize;

      --  Set up trap handler to map synchronous signals to appropriate
      --  exceptions. Make sure that the handler isn't interrupted by
      --  another signal that might cause a scheduling event!

      System.BB.CPU_Primitives.Install_Handler
        (GNAT_Error_Handler'Address, Instruction_Access_Exception);

      System.BB.CPU_Primitives.Install_Handler
        (GNAT_Error_Handler'Address, Illegal_Instruction);

      System.BB.CPU_Primitives.Install_Handler
        (GNAT_Error_Handler'Address, Address_Not_Aligned);

      System.BB.CPU_Primitives.Install_Handler
        (GNAT_Error_Handler'Address, FP_Exception);

      System.BB.CPU_Primitives.Install_Handler
        (GNAT_Error_Handler'Address, Data_Access_Exception);

      System.BB.CPU_Primitives.Install_Handler
        (GNAT_Error_Handler'Address, Instruction_Access_Error);

      System.BB.CPU_Primitives.Install_Handler
        (GNAT_Error_Handler'Address, Data_Access_Error);

      System.BB.CPU_Primitives.Install_Handler
        (GNAT_Error_Handler'Address, Division_By_Zero);

      System.BB.CPU_Primitives.Install_Handler
        (GNAT_Error_Handler'Address, Data_Store_Error);

      Handler_Installed := 1;
   end Install_Handler;

end System.Init;
