------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . B B . P R O T E C T I O N                --
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

with System.BB.CPU_Primitives;
with System.BB.Threads;
with System.BB.Interrupts;

with System.BB.Threads.Queues;
pragma Elaborate (System.BB.Threads.Queues);

package body System.BB.Protection is

   -----------------------
   -- Local subprograms --
   -----------------------

   function Context_Switch_Needed return Boolean;
   pragma Inline (Context_Switch_Needed);
   pragma Export (Asm, Context_Switch_Needed, "context_switch_needed");
   --  This function returns True if the task (or interrupt handler) that is
   --  executing is no longer the highest priority one. This function can also
   --  be called by the interrupt handlers' epilogue.

   ------------------
   -- Enter_Kernel --
   ------------------

   procedure Enter_Kernel is
   begin
      --  Interrupts are disabled to avoid concurrency problems when modifying
      --  kernel data. This way, external interrupts cannot be raised.

      CPU_Primitives.Disable_Interrupts;
   end Enter_Kernel;

   ---------------------------
   -- Context_Switch_Needed --
   ---------------------------

   function Context_Switch_Needed return Boolean is
      use type System.BB.Threads.Thread_Id;

   begin
      --  A context switch is needed when there is a higher priority task ready
      --  to execute. It means that First_Thread is not null and it is not
      --  equal to the task currently executing (Running_Thread).

      pragma Assert (Threads.Queues.First_Thread   /= Threads.Null_Thread_Id
                       and then
                     Threads.Queues.Running_Thread /= Threads.Null_Thread_Id);

      return Threads.Queues.First_Thread /= Threads.Queues.Running_Thread;
   end Context_Switch_Needed;

   ------------------
   -- Leave_Kernel --
   ------------------

   procedure Leave_Kernel is
      use type System.BB.Threads.Thread_Id;
      use type System.BB.Threads.Thread_States;

   begin
      --  Interrupts are always disabled when entering here

      --  If there is nothing to execute (no tasks or interrupt handlers) then
      --  we just wait until there is something to do. It means that we need to
      --  wait until there is any thread ready to execute. Interrupts are
      --  handled just after enabling interrupts.

      if Threads.Queues.First_Thread = Threads.Null_Thread_Id then
         --  There is no task ready to execute so we need to wait until there
         --  is one, unless we are currently handling an interrupt.

         --  In the meantime, we put the task temporarily in the ready queue
         --  so interrupt handling is performed normally. Note that the task
         --  is inserted in the queue but its state is not Runnable.

         Threads.Queues.Insert (Threads.Queues.Running_Thread);

         --  Wait until a task has been made ready to execute (including the
         --  one that has been temporarily added to the ready queue).

         while Threads.Queues.First_Thread = Threads.Queues.Running_Thread
           and then Threads.Queues.Running_Thread.State /= Threads.Runnable
           and then Threads.Queues.Running_Thread.Next = Threads.Null_Thread_Id
         loop
            --  Allow all external interrupts for a while

            CPU_Primitives.Enable_Interrupts (0);
            CPU_Primitives.Disable_Interrupts;
         end loop;

         --  A task has been made ready to execute. We remove the one that was
         --  temporarily inserted in the ready queue, if needed.

         if Threads.Queues.Running_Thread.State /= Threads.Runnable then
            Threads.Queues.Extract (Threads.Queues.Running_Thread);
         end if;
      end if;

      --  We need to check whether a context switch is needed

      if Context_Switch_Needed then
         --  Perform a context switch because the currently executing thread
         --  is blocked or it is no longer the one with the highest priority.

         CPU_Primitives.Context_Switch;
      end if;

      --  Now we need to set the hardware interrupt masking level equal to the
      --  software priority of the task that is executing.

      if Threads.Queues.Running_Thread.Active_Priority in
        Interrupt_Priority'Range
      then
         --  We need to mask some interrupts because we are executing at a
         --  hardware interrupt priority.

         System.BB.CPU_Primitives.Enable_Interrupts
           (Threads.Queues.Running_Thread.Active_Priority -
            System.Interrupt_Priority'First + 1);

      else
         --  We are neither within an interrupt handler nor within task that
         --  has a priority in the range of Interrupt_Priority, so that no
         --  interrupt should be masked.

         System.BB.CPU_Primitives.Enable_Interrupts (0);
      end if;
   end Leave_Kernel;

end System.BB.Protection;
