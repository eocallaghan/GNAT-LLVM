------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       A D A . E X C E P T I O N S                        --
--                                                                          --
--                                 B o d y                                  --
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

--  This body is part of the bare board Ravenscar run time. It implements
--  Ada 83 exception handling, plus a subset of the operations available
--  in Ada 95 for Exception_Occurrences and Exception_Ids (Exception_Name,
--  Exception_Identity ...).

with System;                  use System;
with System.Standard_Library; use System.Standard_Library;
with System.Soft_Links;       use System.Soft_Links;

package body Ada.Exceptions is

   procedure builtin_longjmp (buffer : Address);
   pragma No_Return (builtin_longjmp);
   pragma Import (C, builtin_longjmp, "_gnat_builtin_longjmp");

   procedure Last_Chance_Handler (Except :  Exception_Occurrence);
   pragma Import (C, Last_Chance_Handler, "__gnat_last_chance_handler");
   pragma No_Return (Last_Chance_Handler);

   pragma Suppress (All_Checks);
   --  We definitely do not want exceptions occurring within this unit, or
   --  we are in big trouble. If an exceptional situation does occur, better
   --  that it not be raised, since raising it can cause confusing chaos.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Code_Address_For_AAA return System.Address;
   function Code_Address_For_ZZZ return System.Address;
   --  Return start and end of procedures in this package
   --
   --  These procedures are used to provide exclusion bounds in
   --  calls to Call_Chain at exception raise points from this unit. The
   --  purpose is to arrange for the exception tracebacks not to include
   --  frames from routines involved in the raise process, as these are
   --  meaningless from the user's standpoint.
   --
   --  For these bounds to be meaningful, we need to ensure that the object
   --  code for the routines involved in processing a raise is located after
   --  the object code Code_Address_For_AAA and before the object code
   --  Code_Address_For_ZZZ. This will indeed be the case as long as the
   --  following rules are respected:
   --
   --  1) The bodies of the subprograms involved in processing a raise
   --     are located after the body of Code_Address_For_AAA and before the
   --     body of Code_Address_For_ZZZ.
   --
   --  2) No pragma Inline applies to any of these subprograms, as this
   --     could delay the corresponding assembly output until the end of
   --     the unit.

   procedure Call_Chain (Excep : EOA);
   --  Generate traceback if enabled

   procedure Process_Exception
     (E          : Exception_Id;
      Is_Reraise : Boolean := False);
   --  Shared exception processing for raise / reraise
   pragma No_Return (Process_Exception);
   pragma Export (Ada, Process_Exception, "__gnat_raise_nodefer_with_msg");

   procedure Raise_Constraint_Error
     (File : System.Address;
      Line : Integer);
   pragma No_Return (Raise_Constraint_Error);
   pragma Export
     (C, Raise_Constraint_Error, "__gnat_raise_constraint_error");
   --  Raise constraint error

   procedure Raise_Program_Error
     (File : System.Address;
      Line : Integer);
   pragma No_Return (Raise_Program_Error);
   pragma Export
     (C, Raise_Program_Error, "__gnat_raise_program_error");
   --  Raise program error

   procedure Raise_Storage_Error
     (File : System.Address;
      Line : Integer);
   pragma No_Return (Raise_Storage_Error);
   pragma Export
     (C, Raise_Storage_Error, "__gnat_raise_storage_error");
   --  Raise storage error

   procedure Unhandled_Exception;
   pragma Export (C, Unhandled_Exception, "__gnat_unhandled_exception");
   --  Hook for GDB to support "break exception unhandled". This is a null
   --  routine that has no effect. It is provided for the debugger to place
   --  breakpoints on their entry points to get control on an exception.

   -----------------------------
   -- Run-Time Check Routines --
   -----------------------------

   --  These routines raise a specific exception with a reason message
   --  attached. The parameters are the file name and line number in each
   --  case. The names are keyed to the codes defined in types.ads and
   --  a-types.h (for example, the name Rcheck_05 refers to the Reason
   --  RT_Exception_Code'Val (5)).

   procedure Rcheck_00 (File : System.Address; Line : Integer);
   procedure Rcheck_01 (File : System.Address; Line : Integer);
   procedure Rcheck_02 (File : System.Address; Line : Integer);
   procedure Rcheck_03 (File : System.Address; Line : Integer);
   procedure Rcheck_04 (File : System.Address; Line : Integer);
   procedure Rcheck_05 (File : System.Address; Line : Integer);
   procedure Rcheck_06 (File : System.Address; Line : Integer);
   procedure Rcheck_07 (File : System.Address; Line : Integer);
   procedure Rcheck_08 (File : System.Address; Line : Integer);
   procedure Rcheck_09 (File : System.Address; Line : Integer);
   procedure Rcheck_10 (File : System.Address; Line : Integer);
   procedure Rcheck_11 (File : System.Address; Line : Integer);
   procedure Rcheck_12 (File : System.Address; Line : Integer);
   procedure Rcheck_13 (File : System.Address; Line : Integer);
   procedure Rcheck_14 (File : System.Address; Line : Integer);
   procedure Rcheck_15 (File : System.Address; Line : Integer);
   procedure Rcheck_16 (File : System.Address; Line : Integer);
   procedure Rcheck_17 (File : System.Address; Line : Integer);
   procedure Rcheck_18 (File : System.Address; Line : Integer);
   procedure Rcheck_19 (File : System.Address; Line : Integer);
   procedure Rcheck_20 (File : System.Address; Line : Integer);
   procedure Rcheck_21 (File : System.Address; Line : Integer);
   procedure Rcheck_22 (File : System.Address; Line : Integer);
   procedure Rcheck_23 (File : System.Address; Line : Integer);
   procedure Rcheck_24 (File : System.Address; Line : Integer);
   procedure Rcheck_25 (File : System.Address; Line : Integer);
   procedure Rcheck_26 (File : System.Address; Line : Integer);
   procedure Rcheck_27 (File : System.Address; Line : Integer);
   procedure Rcheck_28 (File : System.Address; Line : Integer);
   procedure Rcheck_29 (File : System.Address; Line : Integer);
   procedure Rcheck_30 (File : System.Address; Line : Integer);
   procedure Rcheck_31 (File : System.Address; Line : Integer);
   procedure Rcheck_32 (File : System.Address; Line : Integer);
   procedure Rcheck_33 (File : System.Address; Line : Integer);

   pragma Export (C, Rcheck_00, "__gnat_rcheck_00");
   pragma Export (C, Rcheck_01, "__gnat_rcheck_01");
   pragma Export (C, Rcheck_02, "__gnat_rcheck_02");
   pragma Export (C, Rcheck_03, "__gnat_rcheck_03");
   pragma Export (C, Rcheck_04, "__gnat_rcheck_04");
   pragma Export (C, Rcheck_05, "__gnat_rcheck_05");
   pragma Export (C, Rcheck_06, "__gnat_rcheck_06");
   pragma Export (C, Rcheck_07, "__gnat_rcheck_07");
   pragma Export (C, Rcheck_08, "__gnat_rcheck_08");
   pragma Export (C, Rcheck_09, "__gnat_rcheck_09");
   pragma Export (C, Rcheck_10, "__gnat_rcheck_10");
   pragma Export (C, Rcheck_11, "__gnat_rcheck_11");
   pragma Export (C, Rcheck_12, "__gnat_rcheck_12");
   pragma Export (C, Rcheck_13, "__gnat_rcheck_13");
   pragma Export (C, Rcheck_14, "__gnat_rcheck_14");
   pragma Export (C, Rcheck_15, "__gnat_rcheck_15");
   pragma Export (C, Rcheck_16, "__gnat_rcheck_16");
   pragma Export (C, Rcheck_17, "__gnat_rcheck_17");
   pragma Export (C, Rcheck_18, "__gnat_rcheck_18");
   pragma Export (C, Rcheck_19, "__gnat_rcheck_19");
   pragma Export (C, Rcheck_20, "__gnat_rcheck_20");
   pragma Export (C, Rcheck_21, "__gnat_rcheck_21");
   pragma Export (C, Rcheck_22, "__gnat_rcheck_22");
   pragma Export (C, Rcheck_23, "__gnat_rcheck_23");
   pragma Export (C, Rcheck_24, "__gnat_rcheck_24");
   pragma Export (C, Rcheck_25, "__gnat_rcheck_25");
   pragma Export (C, Rcheck_26, "__gnat_rcheck_26");
   pragma Export (C, Rcheck_27, "__gnat_rcheck_27");
   pragma Export (C, Rcheck_28, "__gnat_rcheck_28");
   pragma Export (C, Rcheck_29, "__gnat_rcheck_29");
   pragma Export (C, Rcheck_30, "__gnat_rcheck_30");
   pragma Export (C, Rcheck_31, "__gnat_rcheck_31");
   pragma Export (C, Rcheck_32, "__gnat_rcheck_32");
   pragma Export (C, Rcheck_33, "__gnat_rcheck_33");

   --  None of these procedures ever returns (they raise an exception!). By
   --  using pragma No_Return, we ensure that any junk code after the call,
   --  such as normal return epilog stuff, can be eliminated).

   pragma No_Return (Rcheck_00);
   pragma No_Return (Rcheck_01);
   pragma No_Return (Rcheck_02);
   pragma No_Return (Rcheck_03);
   pragma No_Return (Rcheck_04);
   pragma No_Return (Rcheck_05);
   pragma No_Return (Rcheck_06);
   pragma No_Return (Rcheck_07);
   pragma No_Return (Rcheck_08);
   pragma No_Return (Rcheck_09);
   pragma No_Return (Rcheck_10);
   pragma No_Return (Rcheck_11);
   pragma No_Return (Rcheck_12);
   pragma No_Return (Rcheck_13);
   pragma No_Return (Rcheck_14);
   pragma No_Return (Rcheck_15);
   pragma No_Return (Rcheck_16);
   pragma No_Return (Rcheck_17);
   pragma No_Return (Rcheck_18);
   pragma No_Return (Rcheck_19);
   pragma No_Return (Rcheck_20);
   pragma No_Return (Rcheck_21);
   pragma No_Return (Rcheck_22);
   pragma No_Return (Rcheck_23);
   pragma No_Return (Rcheck_24);
   pragma No_Return (Rcheck_25);
   pragma No_Return (Rcheck_26);
   pragma No_Return (Rcheck_27);
   pragma No_Return (Rcheck_28);
   pragma No_Return (Rcheck_29);
   pragma No_Return (Rcheck_30);
   pragma No_Return (Rcheck_32);
   pragma No_Return (Rcheck_33);

   --------------------------
   -- Code_Address_For_AAA --
   --------------------------

   --  This function gives us the start of the PC range for addresses
   --  within the exception unit itself. We hope that gigi/gcc keep all the
   --  procedures in their original order!

   function Code_Address_For_AAA return System.Address is
   begin
      --  We are using a label instead of merely using
      --  Code_Address_For_AAA'Address because on some platforms the latter
      --  does not yield the address we want, but the address of a stub or of
      --  a descriptor instead. This is the case at least on Alpha-VMS and
      --  PA-HPUX.

      <<Start_Of_AAA>>
      return Start_Of_AAA'Address;
   end Code_Address_For_AAA;

   ----------------
   -- Call_Chain --
   ----------------

   procedure Call_Chain (Excep : EOA) is separate;

   ------------------------
   -- Exception_Identity --
   ------------------------

   function Exception_Identity
     (X    : Exception_Occurrence)
      return Exception_Id
   is
   begin
      return X.Id;
   end Exception_Identity;

   --------------------
   -- Exception_Name --
   --------------------

   function Exception_Name (X : Exception_Occurrence) return String is
   begin
      return Exception_Name (X.Id);
   end Exception_Name;

   function Exception_Name (Id : Exception_Id) return String is
   begin
      return To_Ptr (Id.Full_Name) (1 .. Id.Name_Length);
   end Exception_Name;

   --------------------------------------
   -- Calls to Run-Time Check Routines --
   --------------------------------------

   procedure Rcheck_00 (File : System.Address; Line : Integer) is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_00;

   procedure Rcheck_01 (File : System.Address; Line : Integer) is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_01;

   procedure Rcheck_02 (File : System.Address; Line : Integer) is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_02;

   procedure Rcheck_03 (File : System.Address; Line : Integer) is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_03;

   procedure Rcheck_04 (File : System.Address; Line : Integer) is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_04;

   procedure Rcheck_05 (File : System.Address; Line : Integer) is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_05;

   procedure Rcheck_06 (File : System.Address; Line : Integer) is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_06;

   procedure Rcheck_07 (File : System.Address; Line : Integer) is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_07;

   procedure Rcheck_08 (File : System.Address; Line : Integer) is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_08;

   procedure Rcheck_09 (File : System.Address; Line : Integer) is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_09;

   procedure Rcheck_10 (File : System.Address; Line : Integer) is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_10;

   procedure Rcheck_11 (File : System.Address; Line : Integer) is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_11;

   procedure Rcheck_12 (File : System.Address; Line : Integer) is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_12;

   procedure Rcheck_13 (File : System.Address; Line : Integer) is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_13;

   procedure Rcheck_14 (File : System.Address; Line : Integer) is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_14;

   procedure Rcheck_15 (File : System.Address; Line : Integer) is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_15;

   procedure Rcheck_16 (File : System.Address; Line : Integer) is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_16;

   procedure Rcheck_17 (File : System.Address; Line : Integer) is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_17;

   procedure Rcheck_18 (File : System.Address; Line : Integer) is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_18;

   procedure Rcheck_19 (File : System.Address; Line : Integer) is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_19;

   procedure Rcheck_20 (File : System.Address; Line : Integer) is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_20;

   procedure Rcheck_21 (File : System.Address; Line : Integer) is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_21;

   procedure Rcheck_22 (File : System.Address; Line : Integer) is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_22;

   procedure Rcheck_23 (File : System.Address; Line : Integer) is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_23;

   procedure Rcheck_24 (File : System.Address; Line : Integer) is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_24;

   procedure Rcheck_25 (File : System.Address; Line : Integer) is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_25;

   procedure Rcheck_26 (File : System.Address; Line : Integer) is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_26;

   procedure Rcheck_27 (File : System.Address; Line : Integer) is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_27;

   procedure Rcheck_28 (File : System.Address; Line : Integer) is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_28;

   procedure Rcheck_29 (File : System.Address; Line : Integer) is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_29;

   procedure Rcheck_30 (File : System.Address; Line : Integer) is
   begin
      Raise_Storage_Error (File, Line);
   end Rcheck_30;

   procedure Rcheck_31 (File : System.Address; Line : Integer) is
   begin
      Raise_Storage_Error (File, Line);
   end Rcheck_31;

   procedure Rcheck_32 (File : System.Address; Line : Integer) is
   begin
      Raise_Storage_Error (File, Line);
   end Rcheck_32;

   procedure Rcheck_33 (File : System.Address; Line : Integer) is
   begin
      Raise_Storage_Error (File, Line);
   end Rcheck_33;

   ----------------------------
   -- Raise_Constraint_Error --
   ----------------------------

   procedure Raise_Constraint_Error (File : System.Address; Line : Integer) is
      pragma Unreferenced (File, Line);
   begin
      Raise_Exception (Constraint_Error_Def'Access);
   end Raise_Constraint_Error;

   -----------------------
   -- Process_Exception --
   -----------------------

   procedure Process_Exception
     (E          : Exception_Id;
      Is_Reraise : Boolean := False)
   is
      pragma Inspection_Point (E);
      --  The pragma Inspection point here ensures that the debugger
      --  can inspect the parameter.

      Jumpbuf_Ptr : constant Address := Get_Jmpbuf_Address.all;
      Excep       : constant EOA     := Get_Current_Excep.all;

      Id : Exception_Id := E;
      pragma Volatile (Id);
      pragma Warnings (Off, Id);
      --  In order to provide support for breakpoints on unhandled exceptions,
      --  the debugger will also need to be able to inspect the value of E from
      --  another (inner) frame. So we need to make sure that if E is passed in
      --  a register, its value is also spilled on stack. For this, we store
      --  the parameter value in a local variable, and add a pragma Volatile to
      --  make sure it is spilled. The pragma Warnings (Off) is needed because
      --  the compiler knows that Id is not referenced and that this use of
      --  pragma Volatile is peculiar!

   begin
      --  Store the identifier for this exception because it may be
      --  needed by a reraise.

      Excep.Id := E;

      --  Generate traceback if enabled

      if not Is_Reraise then
         Excep.Num_Tracebacks := 0;
         Call_Chain (Excep);
      end if;

      --  WARNING : There should be no exception handler for this body
      --  because this would cause gigi to prepend a setup for a new
      --  jmpbuf to the sequence of statements. We would then always get
      --  this new buf in Jumpbuf_Ptr instead of the one for the exception
      --  we are handling, which would completely break the whole design
      --  of this procedure.

      --  If the jump buffer pointer is non-null, transfer control using it

      if Jumpbuf_Ptr /= Null_Address then
         builtin_longjmp (Jumpbuf_Ptr);

      --  Otherwise this is an unhandled exception

      else
         --  Call this hook so that GDB can insert a breakpoint on unhandled
         --  exceptions. This procedure has no other effect.

         Unhandled_Exception;

         --  Check whether there is any termination handler to be executed for
         --  the environment task, and execute it if needed.

         Task_Termination_Handler.all;

         --  Code to be executed for unhandled exceptions

         Last_Chance_Handler (Excep.all);
      end if;
   end Process_Exception;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception (E : Exception_Id; Message : String := "") is
      pragma Unreferenced (Message);
      --  This appears to be as early as we can start ignoring the "Message"
      --  parameter, since "Raise_Exception" is externally callable.
   begin
      Process_Exception (E);
   end Raise_Exception;

   ----------------------------
   -- Raise_Exception_Always --
   ----------------------------

   procedure Raise_Exception_Always
     (E       : Exception_Id;
      Message : String := "") renames Raise_Exception;

   -------------------------
   -- Raise_Program_Error --
   -------------------------

   procedure Raise_Program_Error (File : System.Address; Line : Integer) is
      pragma Unreferenced (File, Line);
   begin
      Process_Exception (Program_Error_Def'Access);
   end Raise_Program_Error;

   -------------------------
   -- Raise_Storage_Error --
   -------------------------

   procedure Raise_Storage_Error (File : System.Address; Line : Integer) is
      pragma Unreferenced (File, Line);
   begin
      Process_Exception (Storage_Error_Def'Access);
   end Raise_Storage_Error;

   ------------------------
   -- Reraise_Occurrence --
   ------------------------

   procedure Reraise_Occurrence (X : Exception_Occurrence) is
   begin
      Process_Exception (X.Id, Is_Reraise => True);
   end Reraise_Occurrence;

   -------------------------------
   -- Reraise_Occurrence_Always --
   -------------------------------

   procedure Reraise_Occurrence_Always (X : Exception_Occurrence)
     renames Reraise_Occurrence;

   ---------------------------------
   -- Reraise_Occurrence_No_Defer --
   ---------------------------------

   procedure Reraise_Occurrence_No_Defer (X : Exception_Occurrence)
     renames Reraise_Occurrence;

   ---------------------
   -- Save_Occurrence --
   ---------------------

   procedure Save_Occurrence
     (Target : out Exception_Occurrence;
      Source : Exception_Occurrence)
   is
   begin
      Target.Id             := Source.Id;
      Target.Num_Tracebacks := Source.Num_Tracebacks;

      Target.Tracebacks (1 .. Target.Num_Tracebacks) :=
        Source.Tracebacks (1 .. Target.Num_Tracebacks);
   end Save_Occurrence;

   -------------------------
   -- Unhandled_Exception --
   -------------------------

   procedure Unhandled_Exception is
   begin
      null;
   end Unhandled_Exception;

   --------------------------
   -- Code_Address_For_ZZZ --
   --------------------------

   --  This function gives us the end of the PC range for addresses
   --  within the exception unit itself. We hope that gigi/gcc keeps all the
   --  procedures in their original order!

   function Code_Address_For_ZZZ return System.Address is
   begin
      <<Start_Of_ZZZ>>
      return Start_Of_ZZZ'Address;
   end Code_Address_For_ZZZ;

end Ada.Exceptions;
