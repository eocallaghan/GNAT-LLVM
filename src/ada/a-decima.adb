------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          A D A . D E C I M A L                           --
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

package body Ada.Decimal is

   ------------
   -- Divide --
   ------------

   procedure Divide
     (Dividend  : Dividend_Type;
      Divisor   : Divisor_Type;
      Quotient  : out Quotient_Type;
      Remainder : out Remainder_Type)
   is
      --  We have a nested procedure that is the actual intrinsic divide.
      --  This is required because in the current RM, Divide itself does
      --  not have convention Intrinsic.

      procedure Divide
        (Dividend  : Dividend_Type;
         Divisor   : Divisor_Type;
         Quotient  : out Quotient_Type;
         Remainder : out Remainder_Type);

      pragma Import (Intrinsic, Divide);

   begin
      Divide (Dividend, Divisor, Quotient, Remainder);
   end Divide;

end Ada.Decimal;
