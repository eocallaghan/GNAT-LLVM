------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . D I R E C T O R I E S . V A L I D I T Y              --
--                                                                          --
--                                 B o d y                                  --
--                             (POSIX Version)                              --
--                                                                          --
--          Copyright (C) 2004-2007, Free Software Foundation, Inc.         --
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

--  This is the POSIX version of this package

package body Ada.Directories.Validity is

   ---------------------------------
   -- Is_Path_Name_Case_Sensitive --
   ---------------------------------

   function Is_Path_Name_Case_Sensitive return Boolean is
   begin
      return True;
   end Is_Path_Name_Case_Sensitive;

   ------------------------
   -- Is_Valid_Path_Name --
   ------------------------

   function Is_Valid_Path_Name   (Name : String) return Boolean is
   begin
      --  A path name cannot be empty and cannot contain any NUL character

      if Name'Length = 0 then
         return False;

      else
         for J in Name'Range loop
            if Name (J) = ASCII.NUL then
               return False;
            end if;
         end loop;
      end if;

      --  If Name does not contain any NUL character, it is valid

      return True;
   end Is_Valid_Path_Name;

   --------------------------
   -- Is_Valid_Simple_Name --
   --------------------------

   function Is_Valid_Simple_Name (Name : String) return Boolean is
   begin
      --  A file name cannot be empty and cannot contain a slash ('/') or
      --  the NUL character.

      if Name'Length = 0 then
         return False;

      else
         for J in Name'Range loop
            if Name (J) = '/' or else Name (J) = ASCII.NUL then
               return False;
            end if;
         end loop;
      end if;

      --  If Name does not contain any slash or NUL, it is valid

      return True;
   end Is_Valid_Simple_Name;

   -------------
   -- OpenVMS --
   -------------

   function OpenVMS return Boolean is
   begin
      return False;
   end OpenVMS;

   -------------
   -- Windows --
   -------------

   function Windows return Boolean is
   begin
      return False;
   end Windows;

end Ada.Directories.Validity;
