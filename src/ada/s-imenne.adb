------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . I M G _ E N U M _ N E W                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2000-2008, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Conversion;

package body System.Img_Enum_New is

   -------------------------
   -- Image_Enumeration_8 --
   -------------------------

   procedure Image_Enumeration_8
     (Pos     : Natural;
      S       : in out String;
      P       : out Natural;
      Names   : String;
      Indexes : System.Address)
   is
      pragma Assert (S'First = 1);

      type Natural_8 is range 0 .. 2 ** 7 - 1;
      type Index_Table is array (Natural) of Natural_8;
      type Index_Table_Ptr is access Index_Table;

      function To_Index_Table_Ptr is
        new Ada.Unchecked_Conversion (System.Address, Index_Table_Ptr);

      IndexesT : constant Index_Table_Ptr := To_Index_Table_Ptr (Indexes);

      Start : constant Natural := Natural (IndexesT (Pos));
      Next  : constant Natural := Natural (IndexesT (Pos + 1));

   begin
      S (1 .. Next - Start) := Names (Start .. Next - 1);
      P := Next - Start;
   end Image_Enumeration_8;

   --------------------------
   -- Image_Enumeration_16 --
   --------------------------

   procedure Image_Enumeration_16
     (Pos     : Natural;
      S       : in out String;
      P       : out Natural;
      Names   : String;
      Indexes : System.Address)
   is
      pragma Assert (S'First = 1);

      type Natural_16 is range 0 .. 2 ** 15 - 1;
      type Index_Table is array (Natural) of Natural_16;
      type Index_Table_Ptr is access Index_Table;

      function To_Index_Table_Ptr is
        new Ada.Unchecked_Conversion (System.Address, Index_Table_Ptr);

      IndexesT : constant Index_Table_Ptr := To_Index_Table_Ptr (Indexes);

      Start : constant Natural := Natural (IndexesT (Pos));
      Next  : constant Natural := Natural (IndexesT (Pos + 1));

   begin
      S (1 .. Next - Start) := Names (Start .. Next - 1);
      P := Next - Start;
   end Image_Enumeration_16;

   --------------------------
   -- Image_Enumeration_32 --
   --------------------------

   procedure Image_Enumeration_32
     (Pos     : Natural;
      S       : in out String;
      P       : out Natural;
      Names   : String;
      Indexes : System.Address)
   is
      pragma Assert (S'First = 1);

      type Natural_32 is range 0 .. 2 ** 31 - 1;
      type Index_Table is array (Natural) of Natural_32;
      type Index_Table_Ptr is access Index_Table;

      function To_Index_Table_Ptr is
        new Ada.Unchecked_Conversion (System.Address, Index_Table_Ptr);

      IndexesT : constant Index_Table_Ptr := To_Index_Table_Ptr (Indexes);

      Start : constant Natural := Natural (IndexesT (Pos));
      Next  : constant Natural := Natural (IndexesT (Pos + 1));

   begin
      S (1 .. Next - Start) := Names (Start .. Next - 1);
      P := Next - Start;
   end Image_Enumeration_32;

end System.Img_Enum_New;
