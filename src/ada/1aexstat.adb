------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--     A D A . E X C E P T I O N S . S T R E A M _ A T T R I B U T E S      --
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

--  This is the dummy version of this package when we do not implement
--  streaming attributes for Exception_Ids and Exception_Occurrences.

separate (Ada.Exceptions)
package body Stream_Attributes is

   -------------------
   -- EId_To_String --
   -------------------

   --  Call not allowed

   function EId_To_String (X : Exception_Id) return String is
      pragma Unreferenced (X);
   begin
      raise Program_Error;
      return "";
   end EId_To_String;

   ------------------
   -- EO_To_String --
   ------------------

   --  Call not allowed

   function EO_To_String (X : Exception_Occurrence) return String is
      pragma Unreferenced (X);
   begin
      raise Program_Error;
      return "";
   end EO_To_String;

   -------------------
   -- String_To_EId --
   -------------------

   --  Call not allowed

   function String_To_EId (S : String) return Exception_Id is
      pragma Unreferenced (S);
   begin
      raise Program_Error;
      return Null_Id;
   end String_To_EId;

   ------------------
   -- String_To_EO --
   ------------------

   --  Call not allowed

   function String_To_EO (S : String) return Exception_Occurrence is
      pragma Unreferenced (S);
   begin
      raise Program_Error;
      return Null_Occurrence;
   end String_To_EO;

end Stream_Attributes;
