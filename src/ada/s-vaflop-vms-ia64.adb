------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           S Y S T E M . V A X _ F L O A T _ O P E R A T I O N S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2008, Free Software Foundation, Inc.         --
--                       (Version for IA64 OpenVMS)                         --
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

with System.IO;

package body System.Vax_Float_Operations is

   --  Declare functions that do conversions between floating-point formats

   function FS (X : F) return S;
   function GT (X : G) return T;
   function DT (X : D) return T;

   function SF (X : S) return F;
   function TG (X : T) return G;
   function TD (X : T) return D;

   pragma Import (C, FS, "OTS$CVT_FLOAT_F_S");
   pragma Import (C, GT, "OTS$CVT_FLOAT_G_T");
   pragma Import (C, DT, "OTS$CVT_FLOAT_D_T");

   pragma Import (C, SF, "OTS$CVT_FLOAT_S_F");
   pragma Import (C, TG, "OTS$CVT_FLOAT_T_G");
   pragma Import (C, TD, "OTS$CVT_FLOAT_T_D");

   ------------
   -- D_To_G --
   ------------

   function D_To_G (X : D) return G is
   begin
      return TG (DT (X));
   end D_To_G;

   ------------
   -- F_To_G --
   ------------

   function F_To_G (X : F) return G is
   begin
      return TG (T (FS (X)));
   end F_To_G;

   ------------
   -- F_To_S --
   ------------

   function F_To_S (X : F) return S is
   begin
      return FS (X);
   end F_To_S;

   ------------
   -- G_To_D --
   ------------

   function G_To_D (X : G) return D is
   begin
      return TD (GT (X));
   end G_To_D;

   ------------
   -- G_To_F --
   ------------

   function G_To_F (X : G) return F is
   begin
      return SF (S (GT (X)));
   end G_To_F;

   ------------
   -- G_To_Q --
   ------------

   function G_To_Q (X : G) return Q is
   begin
      return Q (GT (X));
   end G_To_Q;

   ------------
   -- G_To_T --
   ------------

   function G_To_T (X : G) return T is
   begin
      return GT (X);
   end G_To_T;

   ------------
   -- F_To_Q --
   ------------

   function F_To_Q (X : F) return Q is
   begin
      return Q (FS (X));
   end F_To_Q;

   ------------
   -- Q_To_F --
   ------------

   function Q_To_F (X : Q) return F is
   begin
      return SF (S (X));
   end Q_To_F;

   ------------
   -- Q_To_G --
   ------------

   function Q_To_G (X : Q) return G is
   begin
      return TG (T (X));
   end Q_To_G;

   ------------
   -- S_To_F --
   ------------

   function S_To_F (X : S) return F is
   begin
      return SF (X);
   end S_To_F;

   ------------
   -- T_To_D --
   ------------

   function T_To_D (X : T) return D is
   begin
      return TD (X);
   end T_To_D;

   ------------
   -- T_To_G --
   ------------

   function T_To_G (X : T) return G is
   begin
      return TG (X);
   end T_To_G;

   -----------
   -- Abs_F --
   -----------

   function Abs_F (X : F) return F is
   begin
      return SF (abs (FS (X)));
   end Abs_F;

   -----------
   -- Abs_G --
   -----------

   function Abs_G (X : G) return G is
   begin
      return TG (abs (GT (X)));
   end Abs_G;

   -----------
   -- Add_F --
   -----------

   function Add_F (X, Y : F) return F is
   begin
      return SF (FS (X) + FS (Y));
   end Add_F;

   -----------
   -- Add_G --
   -----------

   function Add_G (X, Y : G) return G is
   begin
      return TG (GT (X) + GT (Y));
   end Add_G;

   --------------------
   -- Debug_Output_D --
   --------------------

   procedure Debug_Output_D (Arg : D) is
   begin
      System.IO.Put (T'Image (DT (Arg)));
   end Debug_Output_D;

   --------------------
   -- Debug_Output_F --
   --------------------

   procedure Debug_Output_F (Arg : F) is
   begin
      System.IO.Put (S'Image (FS (Arg)));
   end Debug_Output_F;

   --------------------
   -- Debug_Output_G --
   --------------------

   procedure Debug_Output_G (Arg : G) is
   begin
      System.IO.Put (T'Image (GT (Arg)));
   end Debug_Output_G;

   --------------------
   -- Debug_String_D --
   --------------------

   Debug_String_Buffer : String (1 .. 32);
   --  Buffer used by all Debug_String_x routines for returning result

   function Debug_String_D (Arg : D) return System.Address is
      Image_String : constant String := T'Image (DT (Arg)) & ASCII.NUL;
      Image_Size   : constant Integer := Image_String'Length;
   begin
      Debug_String_Buffer (1 .. Image_Size) := Image_String;
      return Debug_String_Buffer (1)'Address;
   end Debug_String_D;

   --------------------
   -- Debug_String_F --
   --------------------

   function Debug_String_F (Arg : F) return System.Address is
      Image_String : constant String := S'Image (FS (Arg)) & ASCII.NUL;
      Image_Size   : constant Integer := Image_String'Length;
   begin
      Debug_String_Buffer (1 .. Image_Size) := Image_String;
      return Debug_String_Buffer (1)'Address;
   end Debug_String_F;

   --------------------
   -- Debug_String_G --
   --------------------

   function Debug_String_G (Arg : G) return System.Address is
      Image_String : constant String := T'Image (GT (Arg)) & ASCII.NUL;
      Image_Size   : constant Integer := Image_String'Length;
   begin
      Debug_String_Buffer (1 .. Image_Size) := Image_String;
      return Debug_String_Buffer (1)'Address;
   end Debug_String_G;

   -----------
   -- Div_F --
   -----------

   function Div_F (X, Y : F) return F is
   begin
      return SF (FS (X) / FS (Y));
   end Div_F;

   -----------
   -- Div_G --
   -----------

   function Div_G (X, Y : G) return G is
   begin
      return TG (GT (X) / GT (Y));
   end Div_G;

   ----------
   -- Eq_F --
   ----------

   function Eq_F (X, Y : F) return Boolean is
   begin
      return FS (X) = FS (Y);
   end Eq_F;

   ----------
   -- Eq_G --
   ----------

   function Eq_G (X, Y : G) return Boolean is
   begin
      return GT (X) = GT (Y);
   end Eq_G;

   ----------
   -- Le_F --
   ----------

   function Le_F (X, Y : F) return Boolean is
   begin
      return FS (X) <= FS (Y);
   end Le_F;

   ----------
   -- Le_G --
   ----------

   function Le_G (X, Y : G) return Boolean is
   begin
      return GT (X) <= GT (Y);
   end Le_G;

   ----------
   -- Lt_F --
   ----------

   function Lt_F (X, Y : F) return Boolean is
   begin
      return FS (X) < FS (Y);
   end Lt_F;

   ----------
   -- Lt_G --
   ----------

   function Lt_G (X, Y : G) return Boolean is
   begin
      return GT (X) < GT (Y);
   end Lt_G;

   -----------
   -- Mul_F --
   -----------

   function Mul_F (X, Y : F) return F is
   begin
      return SF (FS (X) * FS (Y));
   end Mul_F;

   -----------
   -- Mul_G --
   -----------

   function Mul_G (X, Y : G) return G is
   begin
      return TG (GT (X) * GT (Y));
   end Mul_G;

   ----------
   -- Ne_F --
   ----------

   function Ne_F (X, Y : F) return Boolean is
   begin
      return FS (X) /= FS (Y);
   end Ne_F;

   ----------
   -- Ne_G --
   ----------

   function Ne_G (X, Y : G) return Boolean is
   begin
      return GT (X) /= GT (Y);
   end Ne_G;

   -----------
   -- Neg_F --
   -----------

   function Neg_F (X : F) return F is
   begin
      return SF (-FS (X));
   end Neg_F;

   -----------
   -- Neg_G --
   -----------

   function Neg_G (X : G) return G is
   begin
      return TG (-GT (X));
   end Neg_G;

   --------
   -- pd --
   --------

   procedure pd (Arg : D) is
   begin
      System.IO.Put_Line (T'Image (DT (Arg)));
   end pd;

   --------
   -- pf --
   --------

   procedure pf (Arg : F) is
   begin
      System.IO.Put_Line (S'Image (FS (Arg)));
   end pf;

   --------
   -- pg --
   --------

   procedure pg (Arg : G) is
   begin
      System.IO.Put_Line (T'Image (GT (Arg)));
   end pg;

   --------------
   -- Return_D --
   --------------

   function Return_D (X : D) return D is
   begin
      return X;
   end Return_D;

   --------------
   -- Return_F --
   --------------

   function Return_F (X : F) return F is
   begin
      return X;
   end Return_F;

   --------------
   -- Return_G --
   --------------

   function Return_G (X : G) return G is
   begin
      return X;
   end Return_G;

   -----------
   -- Sub_F --
   -----------

   function Sub_F (X, Y : F) return F is
   begin
      return SF (FS (X) - FS (Y));
   end Sub_F;

   -----------
   -- Sub_G --
   -----------

   function Sub_G (X, Y : G) return G is
   begin
      return TG (GT (X) - GT (Y));
   end Sub_G;

   -------------
   -- Valid_D --
   -------------

   --  For now, convert to IEEE and do Valid test on result. This is not quite
   --  accurate, but is good enough in practice.

   function Valid_D (Arg : D) return Boolean is
      Val : constant T := G_To_T (D_To_G (Arg));
   begin
      return Val'Valid;
   end Valid_D;

   -------------
   -- Valid_F --
   -------------

   --  For now, convert to IEEE and do Valid test on result. This is not quite
   --  accurate, but is good enough in practice.

   function Valid_F (Arg : F) return Boolean is
      Val : constant S := F_To_S (Arg);
   begin
      return Val'Valid;
   end Valid_F;

   -------------
   -- Valid_G --
   -------------

   --  For now, convert to IEEE and do Valid test on result. This is not quite
   --  accurate, but is good enough in practice.

   function Valid_G (Arg : G) return Boolean is
      Val : constant T := G_To_T (Arg);
   begin
      return Val'Valid;
   end Valid_G;

end System.Vax_Float_Operations;
