------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . V A L _ W C H A R                      --
--                                                                          --
--                                 S p e c                                  --
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

--  Processing for Wide_[Wide_]Value attribute

with System.WCh_Con;

package System.Val_WChar is
   pragma Pure;

   function Value_Wide_Character
     (Str : String;
      EM  : System.WCh_Con.WC_Encoding_Method) return Wide_Character;
   --  Computes Wide_Character'Value (Str). The parameter EM is the encoding
   --  method used for any Wide_Character sequences in Str. Note that brackets
   --  notation is always permitted.

   function Value_Wide_Wide_Character
     (Str : String;
      EM  : System.WCh_Con.WC_Encoding_Method) return Wide_Wide_Character;
   --  Computes Wide_Character'Value (Str). The parameter EM is the encoding
   --  method used for any wide_character sequences in Str. Note that brackets
   --  notation is always permitted.

end System.Val_WChar;
