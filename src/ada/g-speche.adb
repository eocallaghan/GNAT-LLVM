------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 G N A T . S P E L L I N G _ C H E C K E R                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1998-2008, AdaCore                     --
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

pragma Compiler_Unit;

with GNAT.Spelling_Checker_Generic;

package body GNAT.Spelling_Checker is

   function IBS is new
     GNAT.Spelling_Checker_Generic.Is_Bad_Spelling_Of
       (Character, String);

   ------------------------
   -- Is_Bad_Spelling_Of --
   ------------------------

   function Is_Bad_Spelling_Of
     (Found  : String;
      Expect : String) return Boolean
   renames IBS;

end GNAT.Spelling_Checker;
