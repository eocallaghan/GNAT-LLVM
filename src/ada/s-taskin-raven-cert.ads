------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                        S Y S T E M . T A S K I N G                       --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1992-2008, Free Software Foundation, Inc.         --
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

--  This is the Ravenscar/cert version of this package

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes.

pragma Restrictions (No_Elaboration_Code);

with Ada.Exceptions;
--  used for Exception_ID

with System.Soft_Links;
--  used for TSD

with System.Storage_Elements;
--  used for Storage_Offset

with System.Parameters;
--  used for Size_Type

with System.Task_Info;
--  used for Task_Info_Type

with System.Task_Primitives;
--  used for Private_Data
--           Lock (in System.Tasking.Protected_Objects)

with Unchecked_Conversion;

package System.Tasking is
   pragma Preelaborate;

   ---------------------------------
   -- Task_Id related definitions --
   ---------------------------------

   type Ada_Task_Control_Block;

   type Task_Id is access all Ada_Task_Control_Block;
   function To_Task_Id is new Unchecked_Conversion (System.Address, Task_Id);
   function To_Address is new Unchecked_Conversion (Task_Id, System.Address);

   Null_Task : constant Task_Id := null;

   type Task_List is array (Positive range <>) of Task_Id;
   pragma Suppress_Initialization (Task_List);

   function Self return Task_Id;
   --  This is the compiler interface version of this function. Do not call
   --  from the run-time system.

   -----------------------
   -- Enumeration types --
   -----------------------

   type Task_States is
     (Unactivated,
      --  Task has been created but has not been activated.
      --  It cannot be executing.

      --  Active states

      --  For all states from here down, the task has been activated.
      --  For all states from here down, except for Terminated, the task
      --  may be executing.

      --  For all states from here down, the task has been activated,
      --  and may be executing.

      Runnable,
      --  Task is not blocked for any reason known to Ada.
      --  (It may be waiting for a mutex, though.)
      --  It is conceptually "executing" in normal mode.

      Terminated,
      --  The task is terminated, in the sense of ARM 9.3 (5)

      Activator_Sleep,
      --  Task is waiting for created tasks to complete activation

      Acceptor_Sleep,
      --  Task is waiting on an accept or selective wait statement

      Entry_Caller_Sleep,
      --  Task is waiting on an entry call

      Async_Select_Sleep,
      --  Task is waiting to start the abortable part of an
      --  asynchronous select statement.

      Delay_Sleep,
      --  Task is waiting on a delay statement

      Master_Completion_Sleep,
      --  Master completion has two phases.
      --  In Phase 1 the task is sleeping in Complete_Master
      --  having completed a master within itself,
      --  and is waiting for the tasks dependent on that master to become
      --  terminated or waiting on a terminate Phase.

      Master_Phase_2_Sleep,
      --  In Phase 2 the task is sleeping in Complete_Master
      --  waiting for tasks on terminate alternatives to finish
      --  terminating.

      --  The following are special uses of sleep, for server tasks
      --  within the run-time system.

      Interrupt_Server_Idle_Sleep,
      Interrupt_Server_Blocked_Interrupt_Sleep,
      Timer_Server_Sleep,
      AST_Server_Sleep,

      Asynchronous_Hold,
      --  The task has been held by Asynchronous_Task_Control.Hold_Task

      Interrupt_Server_Blocked_On_Event_Flag
      --  The task has been blocked on a system call waiting for the
      --  completion event.
     );

   --  The following status are never used in a Ravenscar run time. They are
   --  defined for debugging purposes: The same code in GDB to get the current
   --  status of a task in a full run-time environment and in a Ravenscar
   --  environment.
   pragma Unreferenced (Activator_Sleep);
   pragma Unreferenced (Acceptor_Sleep);
   pragma Unreferenced (Async_Select_Sleep);
   pragma Unreferenced (Master_Completion_Sleep);
   pragma Unreferenced (Master_Phase_2_Sleep);
   pragma Unreferenced (Interrupt_Server_Idle_Sleep);
   pragma Unreferenced (Interrupt_Server_Blocked_Interrupt_Sleep);
   pragma Unreferenced (Timer_Server_Sleep);
   pragma Unreferenced (AST_Server_Sleep);
   pragma Unreferenced (Asynchronous_Hold);
   pragma Unreferenced (Interrupt_Server_Blocked_On_Event_Flag);

   type Call_Modes is (Simple_Call);

   -------------------------------
   -- Entry related definitions --
   -------------------------------

   Null_Entry : constant := 0;

   Max_Entry : constant := Integer'Last;

   Interrupt_Entry : constant := -2;

   Cancelled_Entry : constant := -1;

   type Entry_Index is range Interrupt_Entry .. Max_Entry;

   Null_Task_Entry : constant := Null_Entry;

   Max_Task_Entry : constant := Max_Entry;

   type Task_Entry_Index is new Entry_Index
     range Null_Task_Entry .. Max_Task_Entry;

   type Entry_Call_Record;

   type Entry_Call_Link is access all Entry_Call_Record;

   ----------------------------------
   -- Entry_Call_Record definition --
   ----------------------------------

   type Entry_Call_Record is record
      Self : Task_Id;
      --  ID of the caller

      Uninterpreted_Data : System.Address;
      --  Data passed by the compiler

      Exception_To_Raise : Ada.Exceptions.Exception_Id;
      --  The exception to raise once this call has been completed without
      --  being aborted.
   end record;
   pragma Suppress_Initialization (Entry_Call_Record);

   ------------------------------------
   -- Other Task-Related Definitions --
   ------------------------------------

   type Activation_Chain is limited private;

   type Activation_Chain_Access is access all Activation_Chain;

   type Task_Procedure_Access is access procedure (Arg : System.Address);

   type Access_Boolean is access all Boolean;

   ----------------------------------------------
   -- Ada_Task_Control_Block (ATCB) definition --
   ----------------------------------------------

   --  Notes on protection (synchronization) of TRTS data structures

   --  Any field of the TCB can be written by the activator of a task when the
   --  task is created, since no other task can access the new task's
   --  state until creation is complete.

   --  The protection for each field is described in a comment starting with
   --  "Protection:".

   --  When a lock is used to protect an ATCB field, this lock is simply named

   --  Some protection is described in terms of tasks related to the
   --  ATCB being protected. These are:

   --    Self: The task which is controlled by this ATCB.
   --    Activator: The task that created Self and initiated its activation.
   --    Created: A task created and activated by Self.

   type Common_ATCB is record
      State : Task_States;
      pragma Atomic (State);
      --  Encodes some basic information about the state of a task,
      --  including whether it has been activated, whether it is sleeping,
      --  and whether it is terminated.
      --
      --  Protection: Only accessed by Self.

      Base_Priority : System.Any_Priority;
      --  Base priority.
      --
      --  Protection: Only written by Self, accessed by anyone.

      Protected_Action_Nesting : Natural;
      pragma Atomic (Protected_Action_Nesting);
      --  The dynamic level of protected action nesting for this task.
      --  This field is needed for checking whether potentially
      --  blocking operations are invoked from protected actions.
      --  pragma Atomic is used because it can be read/written from
      --  protected interrupt handlers.

      LL : aliased Task_Primitives.Private_Data;
      --  Control block used by the underlying low-level tasking
      --  service (GNULLI).
      --
      --  Protection: This is used only by the GNULLI implementation, which
      --  takes care of all of its synchronization.

      Task_Arg : System.Address;
      --  The argument to task procedure. Currently unused; this will
      --  provide a handle for discriminant information.
      --
      --  Protection: Part of the synchronization between Self and
      --  Activator. Activator writes it, once, before Self starts
      --  executing. Thereafter, Self only reads it.

      Task_Entry_Point : Task_Procedure_Access;
      --  Information needed to call the procedure containing the code for
      --  the body of this task.
      --
      --  Protection: Part of the synchronization between Self and
      --  Activator. Activator writes it, once, before Self starts
      --  executing. Self reads it, once, as part of its execution.

      Compiler_Data : System.Soft_Links.TSD;
      --  Task-specific data needed by the compiler to store
      --  per-task structures.
      --  Protection: Only accessed by Self.

      Activation_Link : Task_Id;
      --  Used to link this task to a list of tasks to be activated.
      --  Protection: Only used by Activator.

      Task_Info : System.Task_Info.Task_Info_Type;
      --  System-specific attributes of the task as specified by the
      --  Task_Info pragma.
   end record;
   pragma Suppress_Initialization (Common_ATCB);

   type Ada_Task_Control_Block (Entry_Num : Task_Entry_Index) is record
      Common     : Common_ATCB;
      Entry_Call : aliased Entry_Call_Record;
      --  Protection: This field is used on entry call queues associated with
      --  protected objects, and is protected by the protected object lock.
   end record;
   pragma Suppress_Initialization (Ada_Task_Control_Block);
   --  The discriminant Entry_Num is not needed, but we keep it here for
   --  compatibility reasons with the rest of the run times, so that the
   --  expander does not need to know which run time is being used.

   --------------------------------
   -- Master Related Definitions --
   --------------------------------

   subtype Master_Level is Integer;
   subtype Master_ID is Master_Level;

   Library_Task_Level : constant Master_Level := 3;

   ------------------------------
   -- Task size, priority info --
   ------------------------------

   function Storage_Size (T : Task_Id) return System.Parameters.Size_Type;
   --  Retrieve from the TCB of the task the allocated size of its stack,
   --  either the system default or the size specified by a pragma. This
   --  is in general a non-static value that can depend on discriminants
   --  of the task.

   Unspecified_Priority : constant Integer := System.Priority'First - 1;

   --------------------
   -- Initialization --
   --------------------

   procedure Initialize;
   --  This procedure constitutes the first part of the initialization of the
   --  GNARL. This includes creating data structures to make the initial thread
   --  into the environment task. The last part of the initialization is done
   --  in System.Tasking.Initialization or System.Tasking.Restricted.Stages.
   --  All the initializations used to be in Tasking.Initialization, but this
   --  is no longer possible with the run time simplification (including
   --  optimized PO and the restricted run time) since one cannot rely on
   --  System.Tasking.Initialization being present, as was done before.

   procedure Initialize_ATCB
     (Task_Entry_Point : Task_Procedure_Access;
      Task_Arg         : System.Address;
      Base_Priority    : System.Any_Priority;
      Task_Info        : System.Task_Info.Task_Info_Type;
      Stack_Address    : System.Address;
      Stack_Size       : System.Parameters.Size_Type;
      T                : Task_Id;
      Success          : out Boolean);
   --  Initialize fields of a TCB and link into global TCB structures
   --  Call this only with abort deferred and holding All_Tasks_L.

private

   type Activation_Chain is record
      T_ID : Task_Id;
   end record;
   pragma Volatile (Activation_Chain);

   Tasking_Error_Def : System.Address;
   pragma Export (C, Tasking_Error_Def, "tasking_error");
   --  Definition of exception Tasking_Error. There is no information
   --  that needs to be stored here when using this run time.

end System.Tasking;
