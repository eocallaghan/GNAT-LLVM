------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--      S Y S T E M . T A S K I N G . P R O T E C T E D _ O B J E C T S     --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Fundation, Inc.          --
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

--  This is the Ravenscar version of this package

with System.Task_Primitives.Operations;
--  Used for Set_Priority
--           Get_Priority
--           Self

package body System.Tasking.Protected_Objects is

   use System.Task_Primitives.Operations;

   ---------------------------
   -- Initialize_Protection --
   ---------------------------

   procedure Initialize_Protection
     (Object           : Protection_Access;
      Ceiling_Priority : Integer)
   is
      Init_Priority : Integer := Ceiling_Priority;

   begin
      if Init_Priority = Unspecified_Priority then
         Init_Priority := System.Priority'Last;
      end if;

      Object.Ceiling := System.Any_Priority (Init_Priority);
      Object.Caller_Priority := System.Any_Priority'First;
      Object.Owner := Null_Task;
   end Initialize_Protection;

   ----------
   -- Lock --
   ----------

   procedure Lock (Object : Protection_Access) is
      Self_Id         : constant Task_Id := Self;
      Caller_Priority : constant Any_Priority := Get_Priority (Self_Id);

   begin
      --  For this run time, pragma Detect_Blocking is always active. As
      --  described in ARM 9.5.1, par. 15, an external call on a protected
      --  subprogram with the same target object as that of the protected
      --  action that is currently in progress (i.e., if the caller is
      --  already the protected object's owner) is a potentially blocking
      --  operation, and hence Program_Error must be raised.

      if Object.Owner = Self_Id then
         raise Program_Error;
      end if;

      --  Check ceiling locking violation

      if Caller_Priority > Object.Ceiling then
         raise Program_Error;
      end if;

      Set_Priority (Self_Id, Object.Ceiling);

      --  Update the protected object's owner

      Object.Owner := Self_Id;

      --  Store caller's active priority so that it can be later
      --  restored when finishing the protected action.

      Object.Caller_Priority := Caller_Priority;

      --  We are entering in a protected action, so that we increase the
      --  protected object nesting level.

      Self_Id.Common.Protected_Action_Nesting :=
        Self_Id.Common.Protected_Action_Nesting + 1;
   end Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Object : Protection_Access) is
      Self_Id : constant Task_Id := Self;

   begin
      --  Calls to this procedure can only take place when being within a
      --  protected action and when the caller is the protected object's
      --  owner.

      pragma Assert (Self_Id.Common.Protected_Action_Nesting > 0
                     and then Object.Owner = Self_Id);

      --  Remove ownership of the protected object

      Object.Owner := Null_Task;

      --  We are exiting from a protected action, so that we decrease the
      --  protected object nesting level.

      Self_Id.Common.Protected_Action_Nesting :=
        Self_Id.Common.Protected_Action_Nesting - 1;

      Set_Priority (Self_Id, Object.Caller_Priority);
   end Unlock;

begin
   --  Ensure that tasking is initialized when using protected objects

   Tasking.Initialize;
end System.Tasking.Protected_Objects;
