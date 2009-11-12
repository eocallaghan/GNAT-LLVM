------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              S Y S T E M . S T A N D A R D _ L I B R A R Y               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2003-2009, Free Software Foundation, Inc.         --
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

--  This package is included in all programs. It contains declarations that
--  are required to be part of every Ada program. A special mechanism is
--  required to ensure that these are loaded, since it may be the case in
--  some programs that the only references to these required packages are
--  from C code or from code generated directly by Gigi, an in both cases
--  the binder is not aware of such references.

--  System.Standard_Library also includes data that must be present in every
--  program, in particular the definitions of all the standard and also some
--  subprograms that must be present in every program.

--  The binder unconditionally includes s-stalib.ali, which ensures that this
--  package and the packages it references are included in all Ada programs,
--  together with the included data.

pragma Polling (Off);
--  We must turn polling off for this unit, because otherwise we get
--  elaboration circularities with Ada.Exceptions if polling is on.

with Unchecked_Conversion;

package System.Standard_Library is
   pragma Preelaborate;

   type Big_String_Ptr is access all String (Positive);
   for Big_String_Ptr'Storage_Size use 0;
   --  A non-fat pointer type for null terminated strings

   function To_Ptr is
     new Unchecked_Conversion (System.Address, Big_String_Ptr);

   -------------------------------------
   -- Exception Declarations and Data --
   -------------------------------------

   type Raise_Action is access procedure;
   --  A pointer to a procedure used in the Raise_Hook field

   type Exception_Data;
   type Exception_Data_Ptr is access all Exception_Data;
   --  An equivalent of Exception_Id that is public

   --  The following record defines the underlying representation of exceptions

   --  WARNING! Any changes to this may need to be reflectd in the following
   --  locations in the compiler and runtime code:

   --    1. The Internal_Exception routine in s-exctab.adb
   --    2. The processing in gigi that tests Not_Handled_By_Others
   --    3. Expand_N_Exception_Declaration in Exp_Ch11
   --    4. The construction of the exception type in Cstand

   type Exception_Data is record
      Not_Handled_By_Others : Boolean;
      --  Normally set False, indicating that the exception is handled in the
      --  usual way by others (i.e. an others handler handles the exception).
      --  Set True to indicate that this exception is not caught by others
      --  handlers, but must be explicitly named in a handler. This latter
      --  setting is currently used by the Abort_Signal.

      Lang : Character;
      --  A character indicating the language raising the exception.
      --  Set to "A" for exceptions defined by an Ada program.
      --  Set to "V" for imported VMS exceptions.

      Name_Length : Natural;
      --  Length of fully expanded name of exception

      Full_Name : System.Address;
      --  Fully expanded name of exception, null terminated
      --  You can use To_Ptr to convert this to a string.

      HTable_Ptr : Exception_Data_Ptr;
      --  Hash table pointer used to link entries together in the hash table
      --  built (by Register_Exception in s-exctab.adb) for converting between
      --  identities and names.

      Import_Code : Integer;
      --  Value for imported exceptions. Needed only for the handling of
      --  Import/Export_Exception for the VMS case, but present in all
      --  implementations (we might well extend this mechanism for other
      --  systems in the future).

      Raise_Hook : Raise_Action;
      --  This field can be used to place a "hook" on an exception. If the
      --  value is non-null, then it points to a procedure which is called
      --  whenever the exception is raised. This call occurs immediately,
      --  before any other actions taken by the raise (and in particular
      --  before any unwinding of the stack occurs).
   end record;

   --  Definitions for standard predefined exceptions defined in Standard,

   --  Why are the Nul's necessary here, seems like they should not be
   --  required, since Gigi is supposed to add a Nul to each name ???

   Constraint_Error_Name : constant String := "CONSTRAINT_ERROR" & ASCII.NUL;
   Program_Error_Name    : constant String := "PROGRAM_ERROR"    & ASCII.NUL;
   Storage_Error_Name    : constant String := "STORAGE_ERROR"    & ASCII.NUL;

   Numeric_Error_Name    : constant String := "NUMERIC_ERROR"    & ASCII.NUL;
   --  This is used only in the Ada 83 case, but it is not worth having a
   --  separate version of s-stalib.ads for use in Ada 83 mode.

   Constraint_Error_Def : aliased Exception_Data :=
     (Not_Handled_By_Others => False,
      Lang                  => 'A',
      Name_Length           => Constraint_Error_Name'Length,
      Full_Name             => Constraint_Error_Name'Address,
      HTable_Ptr            => null,
      Import_Code           => 0,
      Raise_Hook            => null);

   Numeric_Error_Def : aliased Exception_Data :=
     (Not_Handled_By_Others => False,
      Lang                  => 'A',
      Name_Length           => Numeric_Error_Name'Length,
      Full_Name             => Numeric_Error_Name'Address,
      HTable_Ptr            => null,
      Import_Code           => 0,
      Raise_Hook            => null);

   Program_Error_Def : aliased Exception_Data :=
     (Not_Handled_By_Others => False,
      Lang                  => 'A',
      Name_Length           => Program_Error_Name'Length,
      Full_Name             => Program_Error_Name'Address,
      HTable_Ptr            => null,
      Import_Code           => 0,
      Raise_Hook            => null);

   Storage_Error_Def : aliased Exception_Data :=
     (Not_Handled_By_Others => False,
      Lang                  => 'A',
      Name_Length           => Storage_Error_Name'Length,
      Full_Name             => Storage_Error_Name'Address,
      HTable_Ptr            => null,
      Import_Code           => 0,
      Raise_Hook            => null);

   pragma Export (C, Constraint_Error_Def, "constraint_error");
   pragma Export (C, Numeric_Error_Def,    "numeric_error");
   pragma Export (C, Program_Error_Def,    "program_error");
   pragma Export (C, Storage_Error_Def,    "storage_error");

   -----------------
   -- Subprograms --
   -----------------

   procedure Adafinal;
   --  Performs the Ada Runtime finalization the first time it is invoked.
   --  All subsequent calls are ignored.

   procedure Break_Start;
   pragma Export (C, Break_Start, "__gnat_break_start");
   --  This is a dummy procedure that is called at the start of execution.
   --  Its sole purpose is to provide a well defined point for the placement
   --  of a main program breakpoint.

end System.Standard_Library;
