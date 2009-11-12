------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . T H R E A D S . Q U E U E S            --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2007, AdaCore                     --
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

package body System.BB.Threads.Queues is

   use type System.BB.Time.Time;

   ----------------
   -- Local data --
   ----------------

   Head_Alarm_Id : Thread_Id := Null_Thread_Id;
   pragma Volatile (Head_Alarm_Id);
   --  Identifier of the thread that is in the first place of the alarm queue

   ---------------------
   -- Change_Priority --
   ---------------------

   procedure Change_Priority
     (Thread   : Thread_Id;
      Priority : System.Any_Priority)
   is
      Aux_Pointer : Thread_Id;

   begin
      --  We can only change the priority of the thread that is
      --  currently executing.

      pragma Assert (Thread = Running_Thread);

      --  Change the active priority. The base priority does not change

      Thread.Active_Priority := Priority;

      --  When raising the priority, it is not possible that there is
      --  another task with a higher priority (otherwise the other task
      --  would be running). Hence, there is no displacement required within
      --  the queue, because the thread is already in the first position.

      if Thread.Next /= Null_Thread_Id
        and then Priority < Thread.Next.Active_Priority
      then
         --  If we are here it is because the currently executing
         --  thread is lowering its priority, and there is a thread
         --  with a higher priority ready to execute.

         --  The running thread is no longer the highest priority thread

         First_Thread := Thread.Next;

         Aux_Pointer := First_Thread;

         --  FIFO_Within_Priorities dispatching policy. In ALRM D.2.2 it
         --  is said that when the active priority is lowered due to the
         --  loss of inherited priority (the only possible case within the
         --  Ravenscar profile) the task is added at the head of the ready
         --  queue for its new active priority. Next loop will look
         --  for the value of Aux_Pointer that contains the last thread with
         --  a higher priority (so that we insert the thread just after it).

         while Aux_Pointer.Next /= Null_Thread_Id
           and then Priority < Aux_Pointer.Next.Active_Priority
         loop
            Aux_Pointer := Aux_Pointer.Next;
         end loop;

         --  Insert the thread just after the Aux_Pointer

         Thread.Next := Aux_Pointer.Next;
         Aux_Pointer.Next := Thread;
      end if;
   end Change_Priority;

   -------------
   -- Extract --
   -------------

   procedure Extract (Thread : Thread_Id) is
   begin
      --  The only thread that can be extracted from the ready list is
      --  the one that is currently executing (as a result of a delay
      --  or a protected operation).

      pragma Assert
        (Thread = Running_Thread
          and then Thread = First_Thread
          and then Thread.State /= Runnable);

      First_Thread := Thread.Next;
      Thread.Next := Null_Thread_Id;
   end Extract;

   -------------------------
   -- Extract_First_Alarm --
   -------------------------

   function Extract_First_Alarm return Thread_Id is
      Result : constant Thread_Id := Head_Alarm_Id;
   begin
      pragma Assert (Result.State = Delayed);

      Head_Alarm_Id := Head_Alarm_Id.Next_Alarm;
      Result.Alarm_Time := System.BB.Time.Time'Last;
      Result.Next_Alarm := Null_Thread_Id;
      return Result;
   end Extract_First_Alarm;

   -------------------------
   -- Get_Next_Alarm_Time --
   -------------------------

   function Get_Next_Alarm_Time return System.BB.Time.Time is
   begin
      if Head_Alarm_Id = Null_Thread_Id then

         --  If alarm queue is empty then next alarm to raise will be Time'Last

         return System.BB.Time.Time'Last;
      else
         return Head_Alarm_Id.Alarm_Time;
      end if;

   end Get_Next_Alarm_Time;

   ------------
   -- Insert --
   ------------

   procedure Insert (Thread : Thread_Id) is
      Aux_Pointer : Thread_Id;

   begin
      --  If may be the case that we try to insert a task that is already in
      --  the queue. It can only happen if the task was not runnable and its
      --  context was being used for handling an interrupt. Hence, if the task
      --  is already in the queue we do nothing.

      --  Insert at the head of queue if there is no other thread with a higher
      --  priority.

      if First_Thread = Null_Thread_Id
        or else Thread.Active_Priority > First_Thread.Active_Priority
      then
         Thread.Next := First_Thread;
         First_Thread := Thread;

         --  Middle or tail insertion

      else
         Aux_Pointer := First_Thread;

         --  Look for the Aux_Pointer to insert the thread just after it

         while Aux_Pointer /= Thread
           and then Aux_Pointer.Next /= Null_Thread_Id
           and then Aux_Pointer.Next.Active_Priority >= Thread.Active_Priority
         loop
            Aux_Pointer := Aux_Pointer.Next;
         end loop;

         --  Insert the thread after the Aux_Pointer, if needed

         if Aux_Pointer /= Thread then
            Thread.Next := Aux_Pointer.Next;
            Aux_Pointer.Next := Thread;
         end if;
      end if;
   end Insert;

   ------------------
   -- Insert_Alarm --
   ------------------

   procedure Insert_Alarm
     (T        : System.BB.Time.Time;
      Thread   : Thread_Id;
      Is_First : out Boolean)
   is
      Alarm_Id_Aux : Thread_Id;

   begin
      --  We can only insert in the alarm queue threads whose state is Delayed

      pragma Assert (Thread.State = Delayed);

      --  Set the Alarm_Time within the thread descriptor

      Thread.Alarm_Time := T;

      if Head_Alarm_Id = Null_Thread_Id or else
        T < Head_Alarm_Id.Alarm_Time
      then
         --  The thread is inserted as first because either the queue is empty
         --  or the new alarm expires earlier.

         Thread.Next_Alarm := Head_Alarm_Id;
         Head_Alarm_Id := Thread;
         Is_First := True;

      else
         --  Place in the middle

         Alarm_Id_Aux := Head_Alarm_Id;

         --  Find the minimum greater than T alarm within the alarm queue

         while Alarm_Id_Aux.Next_Alarm /= Null_Thread_Id and then
           Alarm_Id_Aux.Next_Alarm.Alarm_Time < T
         loop
            Alarm_Id_Aux := Alarm_Id_Aux.Next_Alarm;
         end loop;

         Thread.Next_Alarm := Alarm_Id_Aux.Next_Alarm;
         Alarm_Id_Aux.Next_Alarm := Thread;

         Is_First := False;
      end if;
   end Insert_Alarm;

   -----------
   -- Yield --
   -----------

   procedure Yield (Thread : Thread_Id) is
      Prio        : constant Integer := Thread.Active_Priority;
      Aux_Pointer : Thread_Id;

   begin
      pragma Assert (Thread = Running_Thread
                       and then Thread = First_Thread
                       and then Thread.State = Runnable);

      if Thread.Next /= Null_Thread_Id
        and then Thread.Next.Active_Priority = Prio
      then
         First_Thread := Thread.Next;

         --  Look for the Aux_Pointer to insert the thread just after it

         Aux_Pointer  := First_Thread;
         while Aux_Pointer.Next /= Null_Thread_Id and then
           Prio = Aux_Pointer.Next.Active_Priority
         loop
            Aux_Pointer := Aux_Pointer.Next;
         end loop;

         --  Insert the thread after the Aux_Pointer

         Thread.Next := Aux_Pointer.Next;
         Aux_Pointer.Next := Thread;
      end if;
   end Yield;

end System.BB.Threads.Queues;
