------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                      S Y S T E M . C O N C A T _ 3                       --
--                                                                          --
--                                 S p e c                                  --
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

--  This package contains a procedure for runtime concatenation of three string
--  operands. It is used when we want to save space in the generated code.

pragma Compiler_Unit;

package System.Concat_3 is

   procedure Str_Concat_3 (R : out String; S1, S2, S3 : String);
   --  Performs the operation R := S1 & S2 & S3. The bounds of R are known to
   --  be correct (usually set by a call to the Str_Concat_Bounds_3 procedure
   --  below), so no bounds checks are required, and it is known that none of
   --  the input operands overlaps R. No assumptions can be made about the
   --  lower bounds of any of the operands.

   procedure Str_Concat_Bounds_3
     (Lo, Hi     : out Natural;
      S1, S2, S3 : String);
   --  Assigns to Lo..Hi the bounds of the result of concatenating the three
   --  given strings, following the rules in the RM regarding null operands.

end System.Concat_3;