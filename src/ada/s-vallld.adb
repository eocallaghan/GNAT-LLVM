------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L _ L L D                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
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

with System.Val_Real; use System.Val_Real;

package body System.Val_LLD is

   ----------------------------
   -- Scan_Long_Long_Decimal --
   ----------------------------

   --  We use the floating-point circuit for now, this will be OK on a PC,
   --  but definitely does NOT have the required precision if the longest
   --  float type is IEEE double. This must be fixed in the future ???

   function Scan_Long_Long_Decimal
     (Str   : String;
      Ptr   : not null access Integer;
      Max   : Integer;
      Scale : Integer) return Long_Long_Integer
   is
      Val : Long_Long_Float;
   begin
      Val := Scan_Real (Str, Ptr, Max);
      return Long_Long_Integer (Val * 10.0 ** Scale);
   end Scan_Long_Long_Decimal;

   -----------------------------
   -- Value_Long_Long_Decimal --
   -----------------------------

   --  Again we cheat and use floating-point ???

   function Value_Long_Long_Decimal
     (Str   : String;
      Scale : Integer) return Long_Long_Integer
   is
   begin
      return Long_Long_Integer (Value_Real (Str) * 10.0 ** Scale);
   end Value_Long_Long_Decimal;

end System.Val_LLD;
