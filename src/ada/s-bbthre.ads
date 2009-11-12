------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                       S Y S T E M . B B . T H R E A D S                  --
--                                                                          --
--                                  S p e c                                 --
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

--  Package that implements basic tasking functionalities

pragma Restrictions (No_Elaboration_Code);

with System;
with System.Parameters;
with System.BB.CPU_Primitives;
with System.BB.Time;
with System.BB.Interrupts;

package System.BB.Threads is
   pragma Preelaborate;

   --------------------------
   -- Basic thread support --
   --------------------------

   Initialized : Boolean := False;
   --  Boolean that indicates whether the tasking executive has finished its
   --  initialization.

   type Thread_Descriptor;
   --  This type contains the information about a thread

   type Thread_Id is access all Thread_Descriptor;
   --  Type used as thread identifier

   Null_Thread_Id : constant Thread_Id := null;
   --  Identifier used to define an invalid value for a thread identifier

   type Thread_States is (Runnable, Suspended, Delayed);
   --  These are the three possible states for a thread under the Ravenscar
   --  profile restrictions: Runnable (not blocked, and it may also be
   --  executing), Suspended (waiting on an entry call), and Delayed (waiting
   --  on a delay until statement).

   type Thread_Descriptor is record
      Context : aliased System.BB.CPU_Primitives.Context_Buffer;
      --  Location where the hardware registers (stack pointer, program
      --  counter, ...) are stored. This field supports context switches among
      --  threads.

      ATCB : System.Address;
      --  Address of the Ada Task Control Block corresponding to the Ada task
      --  that executes on this thread.

      Base_Priority : System.Any_Priority;
      --  Base priority of the thread

      Active_Priority : System.Any_Priority;
      pragma Volatile (Active_Priority);
      --  Active priority that differs from the base priority due to dynamic
      --  priority changes required by the Ceiling Priority Protocol. This
      --  field is marked as Volatile for a fast implementation of
      --  Get_Priority.

      Top_Of_Stack : System.Address;
      --  Address of the top of the stack that is used by the thread

      Bottom_Of_Stack : System.Address;
      --  Address of the bottom of the stack that is used by the thread

      Next : Thread_Id;
      --  Points to the ready thread that is in the next position for
      --  execution.

      Alarm_Time : System.BB.Time.Time;
      --  Time (absolute) when the alarm for this thread expires

      Next_Alarm : Thread_Id;
      --  Next thread in the alarm queue. The queue is ordered by expiration
      --  times. The first place is occupied by the thread which must be
      --  first awaken.

      State : Thread_States;
      --  Encodes some basic information about the state of a thread

      Wakeup_Signaled : Boolean;
      --  Variable which reflects whether another thread has performed a
      --  Wakeup operation on the thread.
   end record;

   for Thread_Descriptor use
      record
         Context at 0 range 0 ..
           (System.BB.CPU_Primitives.Context_Buffer_Size - 1);
      end record;
   --  It is important that the Context field is placed at the beginning of
   --  the record, because this assumption is using for implementing context
   --  switching.

   procedure Initialize
     (Environment_Thread : Thread_Id;
      Main_Priority      : System.Any_Priority);
   --  Procedure to initialize the board and the data structures related to
   --  the low level tasking system. This procedure must be called before any
   --  other tasking operation.

   procedure Thread_Create
     (Id            : Thread_Id;
      Code          : System.Address;
      Arg           : System.Address;
      Priority      : System.Any_Priority;
      Stack_Address : System.Address;
      Stack_Size    : System.Parameters.Size_Type);
   --  Create a new thread
   --
   --  The new thread executes the code at address Code and using Args
   --  as argument. Priority is the base priority of the new
   --  thread. The new thread is provided with a stack of size
   --  Stack_Size that has been preallocated at Stack_Address.
   --
   --  A procedure to destroy threads is not available because that is not
   --  allowed by the Ravenscar profile.

   function Thread_Self return Thread_Id;
   pragma Inline (Thread_Self);
   --  Return the thread identifier of the calling thread

   ----------------
   -- Scheduling --
   ----------------

   procedure Set_Priority (Priority : System.Any_Priority);
   pragma Inline (Set_Priority);
   --  Set the active priority of the executing thread to the given value

   function Get_Priority  (Id : Thread_Id) return System.Any_Priority;
   pragma Inline (Get_Priority);
   --  Get the current active priority of any thread

   procedure Sleep;
   --  The calling thread is unconditionally suspended

   procedure Wakeup (Id : Thread_Id);
   --  Thread Id becomes ready (the thread must be previously suspended)

   ----------
   -- ATCB --
   ----------

   procedure Set_ATCB (ATCB : System.Address);
   pragma Inline (Set_ATCB);
   --  This procedure sets the ATCB passed as argument for the
   --  currently running thread.

   function Get_ATCB return System.Address;
   pragma Inline (Get_ATCB);
   --  Returns the ATCB of the currently executing thread

end System.BB.Threads;
