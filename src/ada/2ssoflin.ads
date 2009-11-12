------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . S O F T _ L I N K S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2008, Free Software Foundation, Inc.         --
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

--  Ravenscar / CERT

--  This package contains a set of subprogram access variables that access some
--  low-level primitives that are called different depending wether tasking is
--  involved or not (e.g. the Get/Set_Jmpbuf_Address that needs to provide a
--  different value for each task). To avoid dragging in the tasking all the
--  time, we use a system of soft links where the links are initialized to
--  non-tasking versions, and then if the tasking is initialized, they are
--  reset to the real tasking versions.

with Ada.Exceptions;

with System.Storage_Elements;

package System.Soft_Links is
   pragma Preelaborate;

   subtype EOA is Ada.Exceptions.Exception_Occurrence_Access;
   subtype EO is Ada.Exceptions.Exception_Occurrence;

   --  First we have the access subprogram types used to establish the links.
   --  The approach is to establish variables containing access subprogram
   --  values which by default point to dummy no tasking versions of routines.

   type No_Param_Proc     is access procedure;
   type EO_Param_Proc     is access procedure (Excep : EO);

   type Get_Address_Call  is access function return Address;
   type Set_Address_Call  is access procedure (Addr : Address);

   type Get_Integer_Call  is access function return Integer;
   type Set_Integer_Call  is access procedure (Len : Integer);

   type Get_EOA_Call      is access function return EOA;

   --  Suppress checks on all these types, since we know corrresponding values
   --  can never be null (the soft links are always initialized).

   pragma Suppress (Access_Check, No_Param_Proc);
   pragma Suppress (Access_Check, Get_Address_Call);
   pragma Suppress (Access_Check, Set_Address_Call);
   pragma Suppress (Access_Check, Get_Integer_Call);
   pragma Suppress (Access_Check, Set_Integer_Call);
   pragma Suppress (Access_Check, Get_EOA_Call);

   procedure Null_Adafinal;
   --  Shuts down the runtime system (non-tasking no-finalization case,
   --  does nothing)

   Adafinal : No_Param_Proc := Null_Adafinal'Access;
   --  Performs the finalization of the Ada Runtime

   function  Get_Jmpbuf_Address_NT return  Address;
   procedure Set_Jmpbuf_Address_NT (Addr : Address);

   Get_Jmpbuf_Address : Get_Address_Call := Get_Jmpbuf_Address_NT'Access;
   Set_Jmpbuf_Address : Set_Address_Call := Set_Jmpbuf_Address_NT'Access;

   function  Get_Sec_Stack_Addr_NT return  Address;
   procedure Set_Sec_Stack_Addr_NT (Addr : Address);

   Get_Sec_Stack_Addr : Get_Address_Call := Get_Sec_Stack_Addr_NT'Access;
   Set_Sec_Stack_Addr : Set_Address_Call := Set_Sec_Stack_Addr_NT'Access;

   function  Get_Current_Excep_NT return EOA;

   Get_Current_Excep : Get_EOA_Call := Get_Current_Excep_NT'Access;

   function Get_GNAT_Exception return Ada.Exceptions.Exception_Id;
   pragma Inline (Get_GNAT_Exception);
   --  This function obtains the Exception_Id from the Exception_Occurrence
   --  referenced by the Current_Excep field of the task specific data.

   --  Export the Get/Set routines for the various Task Specific Data (TSD)
   --  elements as callable subprograms instead of objects of access to
   --  subprogram types.

   function  Get_Jmpbuf_Address_Soft return  Address;
   procedure Set_Jmpbuf_Address_Soft (Addr : Address);
   pragma Inline (Get_Jmpbuf_Address_Soft);
   pragma Inline (Set_Jmpbuf_Address_Soft);

   function  Get_Sec_Stack_Addr_Soft return  Address;
   procedure Set_Sec_Stack_Addr_Soft (Addr : Address);
   pragma Inline (Get_Sec_Stack_Addr_Soft);
   pragma Inline (Set_Sec_Stack_Addr_Soft);

   type Stack_Info is record
      Start_Address : System.Address := System.Null_Address;
      Size          : System.Storage_Elements.Storage_Offset;
   end record;
   pragma Suppress_Initialization (Stack_Info);

   type TSD is record
      Pri_Stack_Info : aliased Stack_Info;
      --  Information on stack (Base/Limit/Size) that is used
      --  by System.Stack_Checking. If this TSD does not belong to
      --  the environment task, the Size field must be initialized
      --  to the tasks requested stack size before the task can do
      --  its first stack check.

      pragma Warnings (Off);

      Jmpbuf_Address : Address := Null_Address;
      --  Address of jump buffer used to store the address of the current
      --  longjmp/setjmp buffer for exception management. These buffers are
      --  threaded into a stack, and the address here is the top of the stack.
      --  A null address means that no exception handler is currently active.

      Sec_Stack_Addr : Address := Null_Address;
      --  Address of currently allocated secondary stack

      pragma Warnings (On);

      Current_Excep : aliased EO;
      --  Exception occurrence that contains the information for the current
      --  exception. Note that any exception in the same task destroys this
      --  information, so the data in this variable must be copied out before
      --  another exception can occur.
      --
      --  Also act as a list of the active exceptions in the case of the GCC
      --  exception mechanism, organized as a stack with the most recent first.

   end record;

end System.Soft_Links;
