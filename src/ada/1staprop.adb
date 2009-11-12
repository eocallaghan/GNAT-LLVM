------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                     Copyright (C) 2001-2008, AdaCore                     --
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
------------------------------------------------------------------------------

--  This is the VxWorks/Cert version of this package

--  This package contains all the GNULL primitives that interface directly
--  with the underlying OS.

with System.OS_Interface;
with System.VxWorks.Ext;

with Interfaces.C;
with Interfaces.VxWorks;

package body System.Task_Primitives.Operations is

   use System.Tasking;
   use System.OS_Interface;
   use System.Parameters;
   use type System.VxWorks.Ext.t_id;
   use type Interfaces.C.int;

   ----------------
   -- Local Data --
   ----------------

   Low_Priority : constant := 255;
   --  VxWorks native (default) lowest scheduling priority

   Current_Task : aliased System.Address := System.Null_Address;
   pragma Export (Ada, Current_Task);
   --  Task specific value used to store the Ada Task_Id

   function To_VxWorks_Priority
     (Priority : System.OS_Interface.int) return System.OS_Interface.int;
   pragma Inline (To_VxWorks_Priority);
   --  Convert between VxWorks and Ada priority

   function To_Ada_Priority
     (Priority : System.OS_Interface.int) return System.Any_Priority;
   pragma Inline (To_Ada_Priority);
   --  Convert between Ada priority and VxWorks priority

   -------------------------
   -- To_VxWorks_Priority --
   -------------------------

   function To_VxWorks_Priority
     (Priority : System.OS_Interface.int) return System.OS_Interface.int is
   begin
      return Low_Priority - Priority;
   end To_VxWorks_Priority;

   ---------------------
   -- To_Ada_Priority --
   ---------------------

   function To_Ada_Priority
     (Priority : System.OS_Interface.int) return System.Any_Priority is
   begin
      return System.Any_Priority (Low_Priority - Priority);
   end To_Ada_Priority;

   ----------
   -- Self --
   ----------

   function Self return Task_Id is
   begin
      pragma Assert (Current_Task /= System.Null_Address);
      return To_Task_Id (Current_Task);
   end Self;

   -----------
   -- Sleep --
   -----------

   procedure Sleep (Self_ID : Task_Id; Reason : System.Tasking.Task_States) is
      pragma Warnings (Off, Reason);

      Result : System.OS_Interface.int;

   begin
      --  Perform a blocking operation to take the CV semaphore

      Result := semTake (Self_ID.Common.LL.CV, WAIT_FOREVER);
      pragma Assert (Result = 0);
   end Sleep;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (Abs_Time : Time) is
      Current_Time : constant Time := Monotonic_Clock;

      Result : System.OS_Interface.int;
      pragma Unreferenced (Result);

   begin
      if Current_Time < Abs_Time then
         Result := taskDelay
           (System.OS_Interface.int (Abs_Time - Current_Time));
      else
         Result := taskDelay (0);
      end if;
   end Delay_Until;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Time is
   begin
      return Time (System.VxWorks.Ext.tickGet);
   end Monotonic_Clock;

   -------------------
   -- RT_Resolution --
   -------------------

   function RT_Resolution return Time is
   begin
      return Time (sysClkRateGet);
   end RT_Resolution;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup (T : Task_Id; Reason : System.Tasking.Task_States) is
      pragma Warnings (Off, Reason);
      Result : System.OS_Interface.int;
   begin
      Result := semGive (T.Common.LL.CV);
      pragma Assert (Result = 0);
   end Wakeup;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (T    : Task_Id;
      Prio : System.Any_Priority)
   is
      Result : System.OS_Interface.int;
   begin
      Result := taskPrioritySet
        (T.Common.LL.Thread, To_VxWorks_Priority
           (System.OS_Interface.int (Prio)));
      pragma Assert (Result = 0);
   end Set_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (T : Task_Id) return System.Any_Priority is
      Result           : System.OS_Interface.int;
      VxWorks_Priority : aliased System.OS_Interface.int;
   begin
      Result := taskPriorityGet (T.Common.LL.Thread, VxWorks_Priority'Access);
      pragma Assert (Result = 0);
      return To_Ada_Priority (VxWorks_Priority);
   end Get_Priority;

   ----------------
   -- Enter_Task --
   ----------------

   procedure Enter_Task (Self_ID : Task_Id) is
      Result : System.OS_Interface.int;
      pragma Unreferenced (Result);
   begin
      Self_ID.Common.LL.Thread := taskIdSelf;
      Result := taskVarAdd (0, Current_Task'Access);
      Current_Task := To_Address (Self_ID);
   end Enter_Task;

   --------------------
   -- Initialize_TCB --
   --------------------

   procedure Initialize_TCB (Self_ID : Task_Id; Succeeded : out Boolean) is
   begin
      Self_ID.Common.LL.CV := semBCreate (SEM_Q_PRIORITY, SEM_EMPTY);

      if Self_ID.Common.LL.CV = 0 then
         Succeeded := False;
      else
         Succeeded := True;
      end if;
   end Initialize_TCB;

   -----------------
   -- Create_Task --
   -----------------

   procedure Create_Task
     (T          : Task_Id;
      Wrapper    : System.Address;
      Stack_Size : System.Parameters.Size_Type;
      Priority   : System.Any_Priority;
      Succeeded  : out Boolean)
   is
      Adjusted_Stack_Size : System.OS_Interface.size_t;

      VX_FP_TASK : constant := 16#0008#;
      --  Option required to create a task in VxWorks/Cert that can use the
      --  floating point unit.

   begin
      if Stack_Size = Unspecified_Size then
         Adjusted_Stack_Size := System.OS_Interface.size_t
           (Default_Stack_Size);
      elsif Stack_Size < Minimum_Stack_Size then
         Adjusted_Stack_Size := System.OS_Interface.size_t
           (Minimum_Stack_Size);
      else
         Adjusted_Stack_Size := System.OS_Interface.size_t (Stack_Size);
      end if;

      T.Common.LL.Thread := taskSpawn
        (System.Null_Address,
         To_VxWorks_Priority (System.OS_Interface.int (Priority)),
         VX_FP_TASK,
         Adjusted_Stack_Size,
         Wrapper,
         To_Address (T));
      Succeeded := T.Common.LL.Thread /= -1;
   end Create_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : System.Tasking.Task_Id) is
   begin
      Enter_Task (Environment_Task);
   end Initialize;

   ---------------------
   -- Is_Task_Context --
   ---------------------

   function Is_Task_Context return Boolean is
   begin
      --  Function intContext returns 1 only if the current execution state
      --  is in interrupt context.

      return Interfaces.VxWorks.intContext /= 1;
   end Is_Task_Context;

end System.Task_Primitives.Operations;
