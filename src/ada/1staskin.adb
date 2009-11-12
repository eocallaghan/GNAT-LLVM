------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                        S Y S T E M . T A S K I N G                       --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                     Copyright (C) 2001-2009, AdaCore                     --
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

--  This is the Ravenscar/HI-E version of this package

pragma Restrictions (No_Elaboration_Code);

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during tasking
--  operations. It causes infinite loops and other problems.

with System.Task_Primitives.Operations;
--  used for Self

with System.Secondary_Stack;
--  used for SS_Init
--           Default_Secondary_Stack_Size

package body System.Tasking is

   use System.Secondary_Stack;

   ------------------------
   -- Local Declarations --
   ------------------------

   Main_Priority : Integer := Unspecified_Priority;
   pragma Export (C, Main_Priority, "__gl_main_priority");
   --  Priority associated with the environment task. By default, its value is
   --  undefined, and can be set by using pragma Priority in the main program.

   Environment : aliased Ada_Task_Control_Block (Entry_Num => 0);
   --  ATCB for the environment task

   ---------------------
   -- Initialize_ATCB --
   ---------------------

   procedure Initialize_ATCB
     (Task_Entry_Point : Task_Procedure_Access;
      Task_Arg         : System.Address;
      Base_Priority    : System.Any_Priority;
      Task_Info        : System.Task_Info.Task_Info_Type;
      Stack_Address    : System.Address;
      Stack_Size       : System.Parameters.Size_Type;
      T                : Task_Id;
      Success          : out Boolean)
   is
   begin
      T.Common.State := Unactivated;

      --  Initialize T.Common.LL

      Task_Primitives.Operations.Initialize_TCB (T, Success);

      if not Success then
         return;
      end if;

      T.Common.Base_Priority := Base_Priority;
      T.Common.Protected_Action_Nesting := 0;
      T.Common.Task_Arg := Task_Arg;
      T.Common.Task_Entry_Point := Task_Entry_Point;
      T.Common.Task_Info := Task_Info;

      T.Common.Compiler_Data.Pri_Stack_Info.Start_Address :=
        Stack_Address;

      T.Common.Compiler_Data.Pri_Stack_Info.Size :=
        Storage_Elements.Storage_Offset
          (Parameters.Adjust_Storage_Size (Stack_Size));
   end Initialize_ATCB;

   ----------------
   -- Initialize --
   ----------------

   Secondary_Stack : aliased Storage_Elements.Storage_Array
                       (1 .. Storage_Elements.Storage_Offset
                               (Default_Secondary_Stack_Size));
   for Secondary_Stack'Alignment use Standard'Maximum_Alignment;
   pragma Warnings (Off, Secondary_Stack);

   Initialized : Boolean := False;
   --  Used to prevent multiple calls to Initialize

   procedure Initialize is
      Base_Priority : Any_Priority;

      Success : Boolean;
      pragma Warnings (Off, Success);

   begin
      if Initialized then
         return;
      end if;

      Initialized := True;

      if Main_Priority = Unspecified_Priority then
         Base_Priority := Default_Priority;
      else
         Base_Priority := Main_Priority;
      end if;

      Initialize_ATCB
        (null, Null_Address, Base_Priority, Task_Info.Unspecified_Task_Info,
         Null_Address, 0, Environment'Access, Success);

      Task_Primitives.Operations.Initialize (Environment'Access);

      Task_Primitives.Operations.Set_Priority
        (Environment'Access, Base_Priority);

      Environment.Common.State := Runnable;
      Environment.Entry_Call.Self := Environment'Access;

      --  Initialize the secondary stack

      Environment.Common.Compiler_Data.Sec_Stack_Addr :=
        Secondary_Stack'Address;
      SS_Init (Secondary_Stack'Address, Default_Secondary_Stack_Size);

      --  No fall back handler by default

      Fall_Back_Handler := null;
   end Initialize;

   ----------
   -- Self --
   ----------

   function Self return Task_Id renames System.Task_Primitives.Operations.Self;

   ------------------
   -- Storage_Size --
   ------------------

   function Storage_Size (T : Task_Id) return System.Parameters.Size_Type is
   begin
      return
         System.Parameters.Size_Type
           (T.Common.Compiler_Data.Pri_Stack_Info.Size);
   end Storage_Size;

end System.Tasking;
