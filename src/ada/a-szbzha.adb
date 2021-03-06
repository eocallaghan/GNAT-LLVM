------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--              ADA.STRINGS.WIDE_WIDE_BOUNDED.WIDE_WIDE_HASH                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--          Copyright (C) 2004-2009, Free Software Foundation, Inc.         --
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

with System.String_Hash;

function Ada.Strings.Wide_Wide_Bounded.Wide_Wide_Hash
  (Key : Bounded.Bounded_Wide_Wide_String)
  return Containers.Hash_Type
is
   use Ada.Containers;
   function Hash is new System.String_Hash.Hash
     (Wide_Wide_Character, Wide_Wide_String, Hash_Type);
begin
   return Hash (Bounded.To_Wide_Wide_String (Key));
end Ada.Strings.Wide_Wide_Bounded.Wide_Wide_Hash;
