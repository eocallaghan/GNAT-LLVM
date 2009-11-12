------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--   A D A . S T R I N G S . E Q U A L _ C A S E _ I N S E N S I T I V E    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2006, Free Software Foundation, Inc.         --
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
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

with Ada.Characters.Handling;  use Ada.Characters.Handling;

function Ada.Strings.Equal_Case_Insensitive
  (Left, Right : String) return Boolean
is
   LI : Integer := Left'First;
   RI : Integer := Right'First;

begin
   if Left'Length /= Right'Length then
      return False;
   end if;

   if Left'Length = 0 then
      return True;
   end if;

   loop
      if To_Lower (Left (LI)) /= To_Lower (Right (RI)) then
         return False;
      end if;

      if LI = Left'Last then
         return True;
      end if;

      LI := LI + 1;
      RI := RI + 1;
   end loop;
end Ada.Strings.Equal_Case_Insensitive;
