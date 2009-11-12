------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T H R E A D S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the VxWorks AE653 version of this package

pragma Restrictions (No_Tasking);
--  The VxWorks AE653 version of this package is intended only for programs
--  which do not use Ada tasking. This restriction ensures that this
--  will be checked by the binder.

with System.Secondary_Stack;
pragma Elaborate_All (System.Secondary_Stack);

package body System.Threads is

   use Interfaces.C;

   package SSS renames System.Secondary_Stack;

   package SSL renames System.Soft_Links;

   Current_ATSD : aliased System.Address := System.Null_Address;
   pragma Export (C, Current_ATSD, "__gnat_current_atsd");

   subtype EOA is Ada.Exceptions.Exception_Occurrence_Access;

   Main_ATSD : aliased ATSD;
   --  TSD for environment task

   --------------------------
   -- VxWorks specific API --
   --------------------------

   ERROR : constant STATUS := Interfaces.C.int (-1);

   function taskIdVerify (tid : t_id)  return STATUS;
   pragma Import (C, taskIdVerify, "taskIdVerify");

   function taskIdSelf return t_id;
   pragma Import (C, taskIdSelf, "taskIdSelf");

   function taskVarAdd
     (tid : t_id; pVar : not null access System.Address) return int;
   pragma Import (C, taskVarAdd, "taskVarAdd");

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Init_RTS;
   --  This procedure performs the initialization of the run-time lib.
   --  It installs System.Threads versions of certain operations of the
   --  run-time lib.

   procedure Install_Handler;
   pragma Import (C, Install_Handler, "__gnat_install_handler");

   function  Get_Jmpbuf_Address return  Address;
   pragma Inline (Get_Jmpbuf_Address);

   procedure Set_Jmpbuf_Address (Addr : Address);
   pragma Inline (Set_Jmpbuf_Address);

   function  Get_Sec_Stack_Addr return  Address;
   pragma Inline (Get_Sec_Stack_Addr);

   procedure Set_Sec_Stack_Addr (Addr : Address);
   pragma Inline (Set_Sec_Stack_Addr);

   function Get_Current_Excep return EOA;
   pragma Inline (Get_Current_Excep);

   -----------------------
   -- Thread_Body_Enter --
   -----------------------

   procedure Thread_Body_Enter
     (Sec_Stack_Address    : System.Address;
      Sec_Stack_Size       : Natural;
      Process_ATSD_Address : System.Address)
   is
      --  Current_ATSD must already be a taskVar of taskIdSelf.
      --  No assertion because taskVarGet is not available on VxWorks/CERT

      TSD : constant ATSD_Access := From_Address (Process_ATSD_Address);

   begin
      TSD.Sec_Stack_Addr := Sec_Stack_Address;
      SSS.SS_Init (TSD.Sec_Stack_Addr, Sec_Stack_Size);
      Current_ATSD := Process_ATSD_Address;

      Install_Handler;
   end Thread_Body_Enter;

   ----------------------------------
   -- Thread_Body_Exceptional_Exit --
   ----------------------------------

   procedure Thread_Body_Exceptional_Exit
     (EO : Ada.Exceptions.Exception_Occurrence)
   is
      pragma Unreferenced (EO);

   begin
      --  No action for this target

      null;
   end Thread_Body_Exceptional_Exit;

   -----------------------
   -- Thread_Body_Leave --
   -----------------------

   procedure Thread_Body_Leave is
   begin
      --  No action for this target

      null;
   end Thread_Body_Leave;

   --------------
   -- Init_RTS --
   --------------

   procedure Init_RTS is
      --  Register environment task
      Result : constant Interfaces.C.int := Register (taskIdSelf);
      pragma Assert (Result /= ERROR);
   begin
      Main_ATSD.Sec_Stack_Addr := SSL.Get_Sec_Stack_Addr_NT;
      Current_ATSD := Main_ATSD'Address;
      Install_Handler;
      SSL.Get_Jmpbuf_Address := Get_Jmpbuf_Address'Access;
      SSL.Get_Sec_Stack_Addr := Get_Sec_Stack_Addr'Access;
      SSL.Get_Current_Excep  := Get_Current_Excep'Access;
      SSL.Set_Jmpbuf_Address := Set_Jmpbuf_Address'Access;
      SSL.Set_Sec_Stack_Addr := Set_Sec_Stack_Addr'Access;
   end Init_RTS;

   -----------------------
   -- Get_Current_Excep --
   -----------------------

   function Get_Current_Excep return EOA is
      CTSD : ATSD_Access := From_Address (Current_ATSD);
   begin
      pragma Assert (Current_ATSD /= System.Null_Address);
      return CTSD.Current_Excep'Access;
   end Get_Current_Excep;

   ------------------------
   -- Get_Jmpbuf_Address --
   ------------------------

   function  Get_Jmpbuf_Address return  Address is
      CTSD : constant ATSD_Access := From_Address (Current_ATSD);
   begin
      pragma Assert (Current_ATSD /= System.Null_Address);
      return CTSD.Jmpbuf_Address;
   end Get_Jmpbuf_Address;

   ------------------------
   -- Get_Sec_Stack_Addr --
   ------------------------

   function  Get_Sec_Stack_Addr return  Address is
      CTSD : constant ATSD_Access := From_Address (Current_ATSD);
   begin
      pragma Assert (Current_ATSD /= System.Null_Address);
      return CTSD.Sec_Stack_Addr;
   end Get_Sec_Stack_Addr;

   --------------
   -- Register --
   --------------

   function Register (T : Thread_Id) return STATUS is
         Result : STATUS;

   begin
      --  It cannot be assumed that the caller of this routine has a ATSD;
      --  so neither this procedure nor the procedures that it calls should
      --  raise or handle exceptions, or make use of a secondary stack.

      --  This routine is only necessary because taskVarAdd cannot be
      --  executed once an AE653 partition has entered normal mode
      --  (depending on configRecord.c, allocation could be disabled).
      --  Otherwise, everything could have been done in Thread_Body_Enter.

      if taskIdVerify (T) = ERROR then
         return ERROR;
      end if;

      Result := taskVarAdd (T, Current_ATSD'Access);
      pragma Assert (Result /= ERROR);

      return Result;
   end Register;

   ------------------------
   -- Set_Jmpbuf_Address --
   ------------------------

   procedure Set_Jmpbuf_Address (Addr : Address) is
      CTSD : constant ATSD_Access := From_Address (Current_ATSD);
   begin
      pragma Assert (Current_ATSD /= System.Null_Address);
      CTSD.Jmpbuf_Address := Addr;
   end Set_Jmpbuf_Address;

   ------------------------
   -- Set_Sec_Stack_Addr --
   ------------------------

   procedure Set_Sec_Stack_Addr (Addr : Address) is
      CTSD : constant ATSD_Access := From_Address (Current_ATSD);
   begin
      pragma Assert (Current_ATSD /= System.Null_Address);
      CTSD.Sec_Stack_Addr := Addr;
   end Set_Sec_Stack_Addr;

begin
   --  Initialize run-time library

   Init_RTS;
end System.Threads;
