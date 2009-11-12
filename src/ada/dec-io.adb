------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                               D E C . I O                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

--  This is an AlphaVMS package that provides the interface between
--  GNAT, DECLib IO packages and the DECLib Bliss library.

pragma Extend_System (Aux_DEC);

with System;                            use  System;
with System.Task_Primitives;            use  System.Task_Primitives;
with System.Task_Primitives.Operations; use  System.Task_Primitives.Operations;
with IO_Exceptions;                     use  IO_Exceptions;
with Aux_IO_Exceptions;                 use  Aux_IO_Exceptions;

package body DEC.IO is

   type File_Type is record
      FCB : Integer   := 0;   -- Temporary
      SEQ : Integer   := 0;
   end record;

   for File_Type'Size use 64;
   for File_Type'Alignment use 8;

   for File_Type use record
      FCB at 0 range 0 .. 31;
      SEQ at 4 range 0 .. 31;
   end record;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function GNAT_Name_64 (File : File_Type) return String;
   pragma Export_Function (GNAT_Name_64, "GNAT$NAME_64");
   --  ??? comment

   function GNAT_Form_64 (File : File_Type) return String;
   pragma Export_Function (GNAT_Form_64, "GNAT$FORM_64");
   --  ??? comment

   procedure Init_IO;
   pragma Interface (C, Init_IO);
   pragma Import_Procedure (Init_IO, "GNAT$$INIT_IO");
   --  ??? comment

   ----------------
   -- IO_Locking --
   ----------------

   package body IO_Locking is

      ------------------
      -- Create_Mutex --
      ------------------

      function Create_Mutex return Access_Mutex is
         M : constant Access_Mutex := new RTS_Lock;

      begin
         Initialize_Lock (M, Global_Task_Level);
         return M;
      end Create_Mutex;

      -------------
      -- Acquire --
      -------------

      procedure Acquire (M : Access_Mutex) is
      begin
         Write_Lock (M, Global_Lock => True);
      end Acquire;

      -------------
      -- Release --
      -------------

      procedure Release (M : Access_Mutex) is
      begin
         Unlock (M, Global_Lock => True);
      end Release;

   end IO_Locking;

   ------------------
   -- GNAT_Name_64 --
   ------------------

   function GNAT_Name_64 (File : File_Type) return String is
      subtype Buffer_Subtype is String (1 .. 8192);

      Buffer : Buffer_Subtype;
      Length : System.Integer_32;

      procedure Get_Name
        (File    : System.Address;
         MaxLen  : System.Integer_32;
         Buffer  : out Buffer_Subtype;
         Length  : out System.Integer_32);
      pragma Interface (C, Get_Name);
      pragma Import_Procedure
        (Get_Name, "GNAT$FILE_NAME",
         Mechanism => (Value, Value, Reference, Reference));

   begin
      Get_Name (File'Address, Buffer'Length, Buffer, Length);
      return Buffer (1 .. Integer (Length));
   end GNAT_Name_64;

   ------------------
   -- GNAT_Form_64 --
   ------------------

   function GNAT_Form_64 (File : File_Type) return String is
      subtype Buffer_Subtype is String (1 .. 8192);

      Buffer : Buffer_Subtype;
      Length : System.Integer_32;

      procedure Get_Form
        (File    : System.Address;
         MaxLen  : System.Integer_32;
         Buffer  : out Buffer_Subtype;
         Length  : out System.Integer_32);
      pragma Interface (C, Get_Form);
      pragma Import_Procedure
        (Get_Form, "GNAT$FILE_FORM",
         Mechanism => (Value, Value, Reference, Reference));

   begin
      Get_Form (File'Address, Buffer'Length, Buffer, Length);
      return Buffer (1 .. Integer (Length));
   end GNAT_Form_64;

-------------------------
-- Package Elaboration --
-------------------------

begin
   Init_IO;
end DEC.IO;
