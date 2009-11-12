------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       A D A . E X C E P T I O N S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  This version is part of the VxWorks AE653 Level A runtime. It implements
--  Ada 83 exception handling, plus a subset of the opertations available
--  in Ada 95 for Exception_Occurrences and Exception_Ids (Exception_Name,
--  Exception_Identity ...).

with System;
with System.Standard_Library;

package Ada.Exceptions is
   pragma Preelaborate_05;
   --  In accordance with Ada 2005 AI-362

   type Exception_Id is private;
   pragma Preelaborable_Initialization (Exception_Id);

   Null_Id : constant Exception_Id;

   type Exception_Occurrence is limited private;
   pragma Preelaborable_Initialization (Exception_Occurrence);

   type Exception_Occurrence_Access is access all Exception_Occurrence;

   Null_Occurrence : constant Exception_Occurrence;

   function Exception_Name (X : Exception_Occurrence) return String;
   --  Same as Exception_Name (Exception_Identity (X))

   function Exception_Name (Id : Exception_Id) return String;

   procedure Raise_Exception (E : Exception_Id; Message : String := "");
   pragma No_Return (Raise_Exception);

   procedure Reraise_Occurrence (X : Exception_Occurrence);
   pragma No_Return (Reraise_Occurrence);

   function Exception_Identity (X : Exception_Occurrence) return Exception_Id;

   procedure Save_Occurrence
     (Target : out Exception_Occurrence;
      Source : Exception_Occurrence);

private
   package SSL renames System.Standard_Library;

   subtype EOA is Exception_Occurrence_Access;

   ------------------
   -- Exception_Id --
   ------------------

   type Exception_Id is new SSL.Exception_Data_Ptr;

   Null_Id : constant Exception_Id := null;

   -------------------------
   -- Private Subprograms --
   -------------------------

   procedure Raise_Exception_Always (E : Exception_Id; Message : String := "");
   pragma No_Return (Raise_Exception_Always);
   pragma Export (Ada, Raise_Exception_Always, "__gnat_raise_exception");

   procedure Reraise_Occurrence_Always (X : Exception_Occurrence);
   pragma No_Return (Reraise_Occurrence_Always);

   procedure Reraise_Occurrence_No_Defer (X : Exception_Occurrence);
   pragma No_Return (Reraise_Occurrence_No_Defer);

   --------------------------
   -- Exception_Occurrence --
   --------------------------

   subtype Code_Loc is System.Address;
   --  Code location used for the traceback table

   Null_Loc : constant System.Address := System.Null_Address;

   Max_Tracebacks : constant := 50;
   --  Maximum number of trace backs stored in exception occurrence

   type Tracebacks_Array is array (1 .. Max_Tracebacks) of Code_Loc;
   --  Traceback array stored in exception occurrence

   type Exception_Occurrence is record
      Id : Exception_Id;
      --  Exception_Identity for this exception occurrence

      Num_Tracebacks : Natural range 0 .. Max_Tracebacks := 0;
      --  Number of traceback entries stored

      Tracebacks : Tracebacks_Array;
      --  Stored tracebacks (in Tracebacks (1 .. Num_Tracebacks))

   end record;

   pragma Warnings (Off);
   --  Allow non-static constants in Ada 2005 mode where this package will be
   --  implicitly categorized as Preelaborate. See AI-362 for details. It is
   --  safe in the context of the run-time to violate the rules!

   Null_Occurrence : constant Exception_Occurrence := (
     Id               => Null_Id,
     Num_Tracebacks   => 0,
     Tracebacks       => (others => Null_Loc));

   pragma Warnings (On);

end Ada.Exceptions;
