------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                      S Y S T E M . C O N C A T _ 5                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2008, Free Software Foundation, Inc.            --
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

with System.Concat_4;

package body System.Concat_5 is

   pragma Suppress (All_Checks);

   ------------------
   -- Str_Concat_5 --
   ------------------

   procedure Str_Concat_5 (R : out String; S1, S2, S3, S4, S5 : String) is
      F, L : Natural;

   begin
      F := R'First;
      L := F + S1'Length - 1;
      R (F .. L) := S1;

      F := L + 1;
      L := F + S2'Length - 1;
      R (F .. L) := S2;

      F := L + 1;
      L := F + S3'Length - 1;
      R (F .. L) := S3;

      F := L + 1;
      L := F + S4'Length - 1;
      R (F .. L) := S4;

      F := L + 1;
      L := R'Last;
      R (F .. L) := S5;
   end Str_Concat_5;

   -------------------------
   -- Str_Concat_Bounds_5 --
   -------------------------

   procedure Str_Concat_Bounds_5
     (Lo, Hi             : out Natural;
      S1, S2, S3, S4, S5 : String)
   is
   begin
      System.Concat_4.Str_Concat_Bounds_4 (Lo, Hi, S2, S3, S4, S5);

      if S1 /= "" then
         Hi := S1'Last + Hi - Lo + 1;
         Lo := S1'First;
      end if;
   end Str_Concat_Bounds_5;

end System.Concat_5;
