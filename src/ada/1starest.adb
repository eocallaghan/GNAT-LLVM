------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . T A S K I N G . R E S T R I C T E D . S T A G E S      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                     Copyright (C) 1999-2006, AdaCore                     --
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

--  This is a simplified version of the System.Tasking.Stages package, for use
--  with the ravenscar/HI-E profile.

--  This package represents the high level tasking interface used by the
--  compiler to expand Ada 95 tasking constructs into simpler run time calls.

pragma Style_Checks (All_Checks);
--  Turn off subprogram alpha order check, since we group soft link bodies and
--  also separate off subprograms for restricted GNARLI.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with System.Task_Primitives.Operations;
--  used for Enter_Task
--           Wakeup
--           Get_Priority
--           Set_Priority
--           Sleep

with System.Secondary_Stack;
--  used for SS_Init
--           Default_Secondary_Stack_Size

with System.Storage_Elements;
--  used for Storage_Array

package body System.Tasking.Restricted.Stages is

   use System.Secondary_Stack;
   use System.Task_Primitives.Operations;
   use System.Task_Info;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Task_Wrapper (Self_ID : Task_Id);
   --  This is the procedure that is called by the GNULL from the new context
   --  when a task is created. It waits for activation and then calls the task
   --  body procedure. When the task body procedure completes, it terminates
   --  the task.

   function Get_Sec_Stack return Address;
   --  Return the address of the task specific secondary stack, as expected by
   --  System.Secondary_Stack
   pragma Export (C, Get_Sec_Stack, "__gnat_get_secondary_stack");

   -------------------
   -- Get_Sec_Stack --
   -------------------

   function Get_Sec_Stack return Address is
   begin
      return Self.Common.Compiler_Data.Sec_Stack_Addr;
   end Get_Sec_Stack;

   ------------------
   -- Task_Wrapper --
   ------------------

   --  The task wrapper is a procedure that is called first for each task
   --  task body, and which in turn calls the compiler-generated task body
   --  procedure. The wrapper's main job is to do initialization for the task.

   --  The variable ID in the task wrapper is used to implement the Self
   --  function on targets where there is a fast way to find the stack
   --  base of the current thread, since it should be at a fixed offset
   --  from the stack base.

   procedure Task_Wrapper (Self_ID : Task_Id) is
      Secondary_Stack : aliased Storage_Elements.Storage_Array
                          (1 .. Storage_Elements.Storage_Offset
                                  (Default_Secondary_Stack_Size));

      TH : Termination_Handler := null;

   begin
      Self_ID.Common.Compiler_Data.Sec_Stack_Addr := Secondary_Stack'Address;
      SS_Init (Secondary_Stack'Address, Default_Secondary_Stack_Size);

      --  Initialize low-level TCB components, that cannot be initialized by
      --  the creator.

      Enter_Task (Self_ID);

      --  Call the task body procedure

      Self_ID.Common.Task_Entry_Point (Self_ID.Common.Task_Arg);

      --  Look for a fall-back handler. There is a single task termination
      --  procedure for all the tasks in the partition.

      --  This package is part of the restricted run time which supports
      --  neither task hierarchies (No_Task_Hierarchy) nor specific task
      --  termination handlers (No_Specific_Termination_Handlers).

      --  Raise the priority to prevent race conditions when using
      --  System.Tasking.Fall_Back_Handler.

      Set_Priority (Self_ID, Any_Priority'Last);

      TH := System.Tasking.Fall_Back_Handler;

      --  Restore original priority after retrieving shared data

      Set_Priority (Self_ID, Self_ID.Common.Base_Priority);

      --  Execute the task termination handler if we found it

      if TH /= null then
         TH.all (Self_ID);
      end if;

      --  We used to raise a Program_Error here to signal the task termination
      --  event in order to avoid silent task death. It has been removed
      --  because the Ada.Task_Termination functionality serves the same
      --  purpose in a more flexible (and standard) way. In addition, this
      --  exception triggered a second execution of the termination handler
      --  (if any was installed). We simply ensure that the task does not
      --  execute any more.

      Sleep (Self_ID, Terminated);

   end Task_Wrapper;

   -----------------------
   -- Restricted GNARLI --
   -----------------------

   -------------------------------
   -- Activate_Restricted_Tasks --
   -------------------------------

   procedure Activate_Restricted_Tasks
     (Chain_Access : Activation_Chain_Access)
   is
      Self_ID : constant Task_Id := Task_Primitives.Operations.Self;
      C       : Task_Id;
      Success : Boolean;

   begin
      --  Raise the priority to prevent activated tasks from racing ahead
      --  before we finish activating the chain.

      Set_Priority (Self_ID, System.Any_Priority'Last);

      --  Activate all the tasks in the chain

      --  Creation of the thread of control was deferred until activation.
      --  So create it now.

      --  Note that since all created tasks will be blocked trying to get our
      --  (environment task) lock, there is no need to lock C here.

      C := Chain_Access.T_ID;
      while C /= null loop
         Task_Primitives.Operations.Create_Task
           (C, Task_Wrapper'Address,
            Parameters.Size_Type (C.Common.Compiler_Data.Pri_Stack_Info.Size),
            C.Common.Base_Priority, Success);

         if Success then
            C.Common.State := Runnable;
         else
            raise Program_Error;
         end if;

         C := C.Common.Activation_Link;
      end loop;

      Self_ID.Common.State := Runnable;

      --  Restore the original priority

      Set_Priority (Self_ID, Self_ID.Common.Base_Priority);

      --  Remove the tasks from the chain

      Chain_Access.T_ID := null;
   end Activate_Restricted_Tasks;

   ------------------------------------
   -- Complete_Restricted_Activation --
   ------------------------------------

   procedure Complete_Restricted_Activation is
   begin
      --  Nothing to be done

      null;
   end Complete_Restricted_Activation;

   ------------------------------
   -- Complete_Restricted_Task --
   ------------------------------

   procedure Complete_Restricted_Task is
   begin
      Task_Primitives.Operations.Self.Common.State := Terminated;
   end Complete_Restricted_Task;

   ----------------------------
   -- Create_Restricted_Task --
   ----------------------------

   procedure Create_Restricted_Task
     (Priority      : Integer;
      Stack_Address : System.Address;
      Size          : System.Parameters.Size_Type;
      Task_Info     : System.Task_Info.Task_Info_Type;
      State         : Task_Procedure_Access;
      Discriminants : System.Address;
      Elaborated    : Access_Boolean;
      Chain         : in out Activation_Chain;
      Task_Image    : String;
      Created_Task  : Task_Id)
   is
      pragma Unreferenced (Task_Image, Elaborated);

      Base_Priority : System.Any_Priority;
      Success       : Boolean;

   begin
      if Priority = Unspecified_Priority then
         Base_Priority := System.Default_Priority;
      else
         Base_Priority := System.Any_Priority (Priority);
      end if;

      --  No need to lock Self_ID here, since only environment task is running

      Initialize_ATCB
        (State, Discriminants, Base_Priority,
         Task_Info, Stack_Address, Size, Created_Task, Success);

      if not Success then
         raise Program_Error;
      end if;

      Created_Task.Entry_Call.Self   := Created_Task;
      Created_Task.Common.Activation_Link := Chain.T_ID;
      Chain.T_ID := Created_Task;
   end Create_Restricted_Task;

   ---------------------------
   -- Finalize_Global_Tasks --
   ---------------------------

   --  Dummy version since this procedure is not used in true ravenscar mode

   procedure Finalize_Global_Tasks is
   begin
      raise Program_Error;
   end Finalize_Global_Tasks;

   ---------------------------
   -- Restricted_Terminated --
   ---------------------------

   function Restricted_Terminated (T : Task_Id) return Boolean is
   begin
      return T.Common.State = Terminated;
   end Restricted_Terminated;

begin
   Tasking.Initialize;
end System.Tasking.Restricted.Stages;
