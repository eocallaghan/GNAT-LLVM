------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                             S Y S T E M . I N I T                        --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--          Copyright (C) 2003-2006, Free Software Foundation, Inc.         --
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

--  This is a simplified version of this package, for use with a configurable
--  run-time library. It is not currently included in any product runtime.

--  This file should be kept synchronized with 2sinit.ads, s-init-xi-sparc.adb,
--  s-init-ae653-cert.adb, and init.c. All these files implement the required
--  functionality for different targets.

package body System.Init is

   --  Copies of global values computed by the binder

   Gl_Main_Priority : Integer := -1;
   pragma Export (C, Gl_Main_Priority, "__gl_main_priority");

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
   pragma Export (C, Gl_Zero_Cost_Exceptions, "__gl_detect_blocking");

   Gl_Default_Stack_Size : Integer := 0;
   pragma Export (C, Gl_Default_Stack_Size, "__gl_default_stack_size");

   Handler_Installed : Integer := 0;
   pragma Export (C, Handler_Installed, "__gnat_handler_installed");
   --  Indication of whether synchronous signal handlers have already been
   --  installed by a previous call to Install_Handler.

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Handler is
   begin
      Handler_Installed := 1;
   end Install_Handler;

end System.Init;
