------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           A D A . C H A R A C T E R S . C O N V E R S I O N S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2005-2006, Free Software Foundation, Inc.         --
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

package Ada.Characters.Conversions is
   pragma Pure;

   function Is_Character (Item : Wide_Character)      return Boolean;
   function Is_String    (Item : Wide_String)         return Boolean;
   function Is_Character (Item : Wide_Wide_Character) return Boolean;
   function Is_String    (Item : Wide_Wide_String)    return Boolean;

   function Is_Wide_Character (Item : Wide_Wide_Character) return Boolean;
   function Is_Wide_String    (Item : Wide_Wide_String)    return Boolean;

   function To_Wide_Character (Item : Character) return Wide_Character;
   function To_Wide_String    (Item : String)    return Wide_String;

   function To_Wide_Wide_Character
     (Item : Character) return Wide_Wide_Character;

   function To_Wide_Wide_String
     (Item : String) return Wide_Wide_String;

   function To_Wide_Wide_Character
     (Item : Wide_Character) return Wide_Wide_Character;

   function To_Wide_Wide_String
     (Item : Wide_String) return Wide_Wide_String;

   function To_Character
     (Item       : Wide_Character;
      Substitute : Character := ' ') return Character;

   function To_String
     (Item       : Wide_String;
      Substitute : Character := ' ') return String;

   function To_Character
     (Item       : Wide_Wide_Character;
      Substitute : Character := ' ') return Character;

   function To_String
     (Item       : Wide_Wide_String;
      Substitute : Character := ' ') return String;

   function To_Wide_Character
     (Item       : Wide_Wide_Character;
      Substitute : Wide_Character := ' ') return Wide_Character;

   function To_Wide_String
     (Item       : Wide_Wide_String;
      Substitute : Wide_Character := ' ') return Wide_String;

end Ada.Characters.Conversions;
