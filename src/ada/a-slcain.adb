------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                    ADA.STRINGS.LESS_CASE_INSENSITIVE                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;

function Ada.Strings.Less_Case_Insensitive
  (Left, Right : String) return Boolean
is
   LI : Integer := Left'First;
   RI : Integer := Right'First;

   LC, RC : Character;

begin
   if LI > Left'Last then
      return RI <= Right'Last;
   end if;

   if RI > Right'Last then
      return False;
   end if;

   loop
      LC := To_Lower (Left (LI));
      RC := To_Lower (Right (RI));

      if LC < RC then
         return True;
      end if;

      if LC > RC then
         return False;
      end if;

      if LI = Left'Last then
         return RI < Right'Last;
      end if;

      if RI = Right'Last then
         return False;
      end if;

      LI := LI + 1;
      RI := RI + 1;
   end loop;
end Ada.Strings.Less_Case_Insensitive;
