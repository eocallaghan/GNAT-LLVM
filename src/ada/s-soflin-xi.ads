------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . S O F T _ L I N K S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

--  This package contains a set of subprogram access variables that access
--  some low-level primitives that are different depending whether tasking is
--  involved or not (e.g. the Get/Set_Jmpbuf_Address that needs to provide a
--  different value for each task). To avoid dragging in the tasking runtimes
--  all the time, we use a system of soft links where the links are
--  initialized to non-tasking versions, and then if the tasking support is
--  initialized, they are set to the real tasking versions.

--  This is a Ravenscar bare board version of this package. Tasking versions
--  of the primitives are always used.

with Ada.Exceptions;

package System.Soft_Links is
   pragma Preelaborate;

   subtype EOA is Ada.Exceptions.Exception_Occurrence_Access;
   subtype EO is Ada.Exceptions.Exception_Occurrence;

   --  First we have the access subprogram types used to establish the links.
   --  The approach is to establish variables containing access subprogram
   --  values, which by default point to dummy no tasking versions of routines.

   type No_Param_Proc     is access procedure;

   type Get_Address_Call  is access function return Address;
   type Set_Address_Call  is access procedure (Addr : Address);

   type Get_EOA_Call      is access function return EOA;
   type Set_EOA_Call      is access procedure (Excep : EOA);
   type Set_EO_Call       is access procedure (Excep : EO);

   --  Suppress checks on all these types, since we know the corrresponding
   --  values can never be null (the soft links are always initialized).

   pragma Suppress (Access_Check, No_Param_Proc);
   pragma Suppress (Access_Check, Get_Address_Call);
   pragma Suppress (Access_Check, Set_Address_Call);
   pragma Suppress (Access_Check, Get_EOA_Call);
   pragma Suppress (Access_Check, Set_EOA_Call);
   pragma Suppress (Access_Check, Set_EO_Call);

   procedure Task_Lock_Soft;
   --  Lock out other tasks

   procedure Task_Unlock_Soft;
   --  Release lock set by Task_Lock

   Lock_Task : No_Param_Proc := Task_Lock_Soft'Access;
   --  Locks out other tasks. Preceding a section of code by Task_Lock and
   --  following it by Task_Unlock creates a critical region. This is used
   --  for ensuring that a region of non-tasking code (such as code used to
   --  allocate memory) is tasking safe. Note that it is valid for calls to
   --  Task_Lock/Task_Unlock to be nested, and this must work properly, i.e.
   --  only the corresponding outer level Task_Unlock will actually unlock.

   Unlock_Task : No_Param_Proc := Task_Unlock_Soft'Access;
   --  Releases lock previously set by call to Lock_Task. In the nested case,
   --  all nested locks must be released before other tasks competing for the
   --  tasking lock are released.
   --
   --  Note: the recommended protocol for using Lock_Task and Unlock_Task
   --  is as follows:
   --
   --    Locked_Processing : begin
   --       System.Soft_Links.Lock_Task.all;
   --       ...
   --       System.Soft_Links.Unlock_Task.all;
   --
   --    exception
   --       when others =>
   --          System.Soft_Links.Unlock_Task.all;
   --          raise;
   --    end Locked_Processing;
   --
   --  This ensures that the lock is not left set if an exception is raised
   --  explicitly or implicitly during the critical locked region.

   procedure Adafinal_Soft;
   --  Programs do not terminate in Ravenscar

   Adafinal : No_Param_Proc := Adafinal_Soft'Access;
   --  Performs the finalization of the Ada Runtime

   --  Declarations for the no tasking versions of the required routines

   function  Get_Jmpbuf_Address_Soft return  Address;
   procedure Set_Jmpbuf_Address_Soft (Addr : Address);
   pragma Inline (Get_Jmpbuf_Address_Soft);
   pragma Inline (Set_Jmpbuf_Address_Soft);

   Get_Jmpbuf_Address : constant Get_Address_Call :=
     Get_Jmpbuf_Address_Soft'Access;

   Set_Jmpbuf_Address : constant Set_Address_Call :=
     Set_Jmpbuf_Address_Soft'Access;

   function Get_Current_Excep_Soft return EOA;
   pragma Inline (Get_Current_Excep_Soft);

   Get_Current_Excep : constant Get_EOA_Call := Get_Current_Excep_Soft'Access;

   function Get_GNAT_Exception return Ada.Exceptions.Exception_Id;
   pragma Inline (Get_GNAT_Exception);
   --  This function obtains the Exception_Id from the Exception_Occurrence
   --  referenced by the Current_Excep field of the task specific data, i.e.
   --  the call is equivalent to
   --  Exception_Identity (Get_Current_Exception.all)

   procedure Task_Termination_Soft;
   --  Handle task termination routines for the environment task (non-tasking
   --  case, does nothing).

   Task_Termination_Handler : No_Param_Proc := Task_Termination_Soft'Access;
   --  Handle task termination routines (task/non-task case as appropriate)

   procedure Finalize_Global_List_Soft;
   pragma Inline (Finalize_Global_List_Soft);
   --  Finalize global list for controlled objects (does nothing)

   Finalize_Global_List : No_Param_Proc := Finalize_Global_List_Soft'Access;
   --  Performs finalization of global list for controlled objects

end System.Soft_Links;
