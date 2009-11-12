------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--     G E N E R I C _ F A S T _ E L E M E N T A R Y _ F U N C T I O N S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

--  This body is specifically for using an Ada interface to C math.h to get
--  the computation engine. Many special cases are handled locally to avoid
--  unnecessary calls. This is not a "strict" implementation, but takes full
--  advantage of the C functions, e.g. in providing interface to hardware
--  provided versions of the elementary functions.

--  A known weakness is that on the x86, all computation is done in Double,
--  which means that a lot of accuracy is lost for the Long_Long_Float case.

--  Uses functions sqrt, exp, log, pow, sin, asin, cos, acos, tan, atan,
--  sinh, cosh, tanh from C library via math.h
--  This is an adaptation of Ada.Numerics.Generic_Elementary_Functions that
--  provides a compatible body for the DEC Math_Lib package.

pragma Warnings (Off);
--  Allow with's of non-standard units

with Ada.Numerics.Aux;
use type Ada.Numerics.Aux.Double;
with Ada.Numerics; use Ada.Numerics;

pragma Warnings (On);

package body Generic_Fast_Elementary_Functions is

   Log_Two : constant := 0.69314_71805_59945_30941_72321_21458_17656_80755;

   Two_Pi     : constant Float_Type := 2.0 * Pi;
   Half_Pi    : constant Float_Type := Pi / 2.0;
   Fourth_Pi  : constant Float_Type := Pi / 4.0;
   Epsilon    : constant Float_Type := Float_Type'Epsilon;
   IEpsilon   : constant Float_Type := 1.0 / Epsilon;

   subtype Double is Aux.Double;

   DEpsilon    : constant Double := Double (Epsilon);
   DIEpsilon   : constant Double := Double (IEpsilon);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Arctan
     (Y    : Float_Type;
      A    : Float_Type := 1.0)
      return Float_Type;

   function Arctan
     (Y     : Float_Type;
      A     : Float_Type := 1.0;
      Cycle : Float_Type)
      return  Float_Type;

   function Exact_Remainder
     (A    : Float_Type;
      Y    : Float_Type)
      return Float_Type;
   --  Computes exact remainder of A divided by Y

   function Half_Log_Epsilon return Float_Type;
   --  Function to provide constant: 0.5 * Log (Epsilon)

   function Local_Atan
     (Y    : Float_Type;
      A    : Float_Type := 1.0)
      return Float_Type;
   --  Common code for arc tangent after cyele reduction

   function Log_Inverse_Epsilon return Float_Type;
   --  Function to provide constant: Log (1.0 / Epsilon)

   function Square_Root_Epsilon return Float_Type;
   --  Function to provide constant: Sqrt (Epsilon)

   ----------
   -- "**" --
   ----------

   function "**" (X, Y : Float_Type) return Float_Type is

   begin
      if X = 0.0
        and then Y = 0.0
      then
         raise Argument_Error;

      elsif X < 0.0 then
         raise Argument_Error;

      elsif Y = 0.0 then
         return 1.0;

      elsif X = 0.0 then
         if Y < 0.0 then
            raise Constraint_Error;
         else
            return 0.0;
         end if;

      elsif X = 1.0 then
         return 1.0;

      elsif Y = 1.0 then
         return X;

      else
         begin
            if Y = 2.0 then
               return X * X;
            else
               return
                 Float_Type (Aux.pow (Double (X), Double (Y)));
            end if;

         exception
            when others =>
               raise Constraint_Error;
         end;
      end if;
   end "**";

   ------------
   -- Arccos --
   ------------

   --  Natural cycle

   function Arccos (X : Float_Type) return Float_Type is
      Temp : Float_Type;

   begin
      if abs X > 1.0 then
         raise Argument_Error;

      elsif abs X < Square_Root_Epsilon then
         return Pi / 2.0 - X;

      elsif X = 1.0 then
         return 0.0;

      elsif X = -1.0 then
         return Pi;
      end if;

      Temp := Float_Type (Aux.acos (Double (X)));

      if Temp < 0.0 then
         Temp := Pi + Temp;
      end if;

      return Temp;
   end Arccos;

   --  Arbitrary cycle

   function Arccos (X, Cycle : Float_Type) return Float_Type is
      Temp : Float_Type;

   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif abs X > 1.0 then
         raise Argument_Error;

      elsif abs X < Square_Root_Epsilon then
         return Cycle / 4.0;

      elsif X = 1.0 then
         return 0.0;

      elsif X = -1.0 then
         return Cycle / 2.0;
      end if;

      Temp := Arctan (Sqrt (1.0 - X * X) / X, 1.0, Cycle);

      if Temp < 0.0 then
         Temp := Cycle / 2.0 + Temp;
      end if;

      return Temp;
   end Arccos;

   -------------
   -- Arccosh --
   -------------

   function Arccosh (X : Float_Type) return Float_Type is
   begin
      --  Return Log (X - Sqrt (X * X - 1.0));  double valued,
      --    only positive value returned
      --  What is this comment ???

      if X < 1.0 then
         raise Argument_Error;

      elsif X < 1.0 + Square_Root_Epsilon then
         return X - 1.0;

      elsif abs X > 1.0 / Square_Root_Epsilon then
         return Log (X) + Log_Two;

      else
         return Log (X + Sqrt (X * X - 1.0));
      end if;
   end Arccosh;

   ------------
   -- Arccot --
   ------------

   --  Natural cycle

   function Arccot
     (X    : Float_Type;
      Y    : Float_Type := 1.0)
      return Float_Type
   is
   begin
      --  Just reverse arguments

      return Arctan (Y, X);
   end Arccot;

   --  Arbitrary cycle

   function Arccot
     (X     : Float_Type;
      Y     : Float_Type := 1.0;
      Cycle : Float_Type)
      return  Float_Type
   is
   begin
      --  Just reverse arguments

      return Arctan (Y, X, Cycle);
   end Arccot;

   -------------
   -- Arccoth --
   -------------

   function Arccoth (X : Float_Type) return Float_Type is
   begin
      if abs X = 1.0 then
         raise Constraint_Error;

      elsif abs X < 1.0 then
         raise Argument_Error;

      elsif abs X > 1.0 / Epsilon then
         return 0.0;

      else
         return 0.5 * Log ((1.0 + X) / (X - 1.0));
      end if;
   end Arccoth;

   ------------
   -- Arcsin --
   ------------

   --  Natural cycle

   function Arcsin (X : Float_Type) return Float_Type is
   begin
      if abs X > 1.0 then
         raise Argument_Error;

      elsif abs X < Square_Root_Epsilon then
         return X;

      elsif X = 1.0 then
         return Pi / 2.0;

      elsif X = -1.0 then
         return -Pi / 2.0;
      end if;

      return Float_Type (Aux.asin (Double (X)));
   end Arcsin;

   --  Arbitrary cycle

   function Arcsin (X, Cycle : Float_Type) return Float_Type is
   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif abs X > 1.0 then
         raise Argument_Error;

      elsif X = 0.0 then
         return X;

      elsif X = 1.0 then
         return Cycle / 4.0;

      elsif X = -1.0 then
         return -Cycle / 4.0;
      end if;

      return Arctan (X / Sqrt (1.0 - X * X), 1.0, Cycle);
   end Arcsin;

   -------------
   -- Arcsinh --
   -------------

   function Arcsinh (X : Float_Type) return Float_Type is
   begin
      if abs X < Square_Root_Epsilon then
         return X;

      elsif X > 1.0 / Square_Root_Epsilon then
         return Log (X) + Log_Two;

      elsif X < -1.0 / Square_Root_Epsilon then
         return -(Log (-X) + Log_Two);

      elsif X < 0.0 then
         return -Log (abs X + Sqrt (X * X + 1.0));

      else
         return Log (X + Sqrt (X * X + 1.0));
      end if;
   end Arcsinh;

   ------------
   -- Arctan --
   ------------

   --  Natural cycle

   function Arctan
     (Y    : Float_Type;
      A    : Float_Type := 1.0)
      return Float_Type
   is
   begin
      if A = 0.0
        and then Y = 0.0
      then
         raise Argument_Error;

      elsif Y = 0.0 then
         if A > 0.0 then
            return 0.0;
         else -- A < 0.0
            return Pi;
         end if;

      elsif A = 0.0 then
         if Y > 0.0 then
            return Half_Pi;
         else -- Y < 0.0
            return -Half_Pi;
         end if;

      else
         return Local_Atan (Y, A);
      end if;
   end Arctan;

   --  Arbitrary cycle

   function Arctan
     (Y     : Float_Type;
      A     : Float_Type := 1.0;
      Cycle : Float_Type)
      return  Float_Type
   is
   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif A = 0.0
        and then Y = 0.0
      then
         raise Argument_Error;

      elsif Y = 0.0 then
         if A > 0.0 then
            return 0.0;
         else -- A < 0.0
            return Cycle / 2.0;
         end if;

      elsif A = 0.0 then
         if Y > 0.0 then
            return Cycle / 4.0;
         else -- Y < 0.0
            return -Cycle / 4.0;
         end if;

      else
         return Local_Atan (Y, A) *  Cycle / Two_Pi;
      end if;
   end Arctan;

   -------------
   -- Arctanh --
   -------------

   function Arctanh (X : Float_Type) return Float_Type is
   begin
      if abs X = 1.0 then
         raise Constraint_Error;

      elsif abs X > 1.0 then
         raise Argument_Error;

      elsif abs X < Square_Root_Epsilon then
         return X;

      else
         return 0.5 * Log ((1.0 + X) / (1.0 - X));
      end if;
   end Arctanh;

   ---------
   -- Cos --
   ---------

   --  Natural cycle

   function Cos (X : Float_Type) return Float_Type is
   begin
      if X = 0.0 then
         return 1.0;

      elsif abs X < Square_Root_Epsilon then
         return 1.0;

      end if;

      return Float_Type (Aux.Cos (Double (X)));
   end Cos;

   --  Arbitrary cycle

   function Cos (X, Cycle : Float_Type) return Float_Type is
      T : Float_Type;

   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif X = 0.0 then
         return 1.0;
      end if;

      T := Exact_Remainder (abs (X), Cycle) / Cycle;

      if T = 0.25
        or else T = 0.75
        or else T = -0.25
        or else T = -0.75
      then
         return 0.0;

      elsif T = 0.5 or T = -0.5 then
         return -1.0;
      end if;

      return Float_Type (Aux.Cos (Double (T * Two_Pi)));
   end Cos;

   ----------
   -- Cosh --
   ----------

   function Cosh (X : Float_Type) return Float_Type is
   begin
      if abs X < Square_Root_Epsilon then
         return 1.0;

      elsif abs X > Log_Inverse_Epsilon then
         return Exp ((abs X) - Log_Two);
      end if;

      return Float_Type (Aux.cosh (Double (X)));

   exception
      when others =>
         raise Constraint_Error;
   end Cosh;

   ---------
   -- Cot --
   ---------

   --  Natural cycle

   function Cot (X : Float_Type) return Float_Type is
   begin
      if X = 0.0 then
         raise Constraint_Error;

      elsif abs X < Square_Root_Epsilon then
         return 1.0 / X;
      end if;

      return 1.0 / Float_Type (Aux.tan (Double (X)));
   end Cot;

   --  Arbitrary cycle

   function Cot (X, Cycle : Float_Type) return Float_Type is
      T : Float_Type;
   begin

      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif X = 0.0 then
         raise Constraint_Error;

      elsif abs X < Square_Root_Epsilon then
         return 1.0 / X;
      end if;

      T := Exact_Remainder (X, Cycle) / Cycle;

      if T = 0.0 or T = 0.5 or T = -0.5 then
         raise Constraint_Error;
      else
         return Cos (T * Two_Pi) / Sin (T * Two_Pi);
      end if;
   end Cot;

   ----------
   -- Coth --
   ----------

   function Coth (X : Float_Type) return Float_Type is
   begin
      if X = 0.0 then
         raise Constraint_Error;

      elsif X < Half_Log_Epsilon then
         return -1.0;

      elsif X > -Half_Log_Epsilon then
         return 1.0;

      elsif abs X < Square_Root_Epsilon then
         return 1.0 / X;
      end if;

      return 1.0 / Float_Type (Aux.tanh (Double (X)));
   end Coth;

   ---------------------
   -- Exact_Remainder --
   ---------------------

   function Exact_Remainder
     (A    : Float_Type;
      Y    : Float_Type)
      return Float_Type
   is
      Denominator : Float_Type := abs A;
      Divisor     : Float_Type := abs Y;
      Reducer     : Float_Type;
      Sign        : Float_Type := 1.0;

   begin
      if Y = 0.0 then
         raise Constraint_Error;

      elsif A = 0.0 then
         return 0.0;

      elsif A = Y then
         return 0.0;

      elsif Denominator < Divisor then
         return A;
      end if;

      while Denominator >= Divisor loop

         --  Put divisors mantissa with denominators exponent to make reducer

         Reducer := Divisor;

         begin
            while Reducer * 1_048_576.0 < Denominator loop
               Reducer := Reducer * 1_048_576.0;
            end loop;

         exception
            when others => null;
         end;

         begin
            while Reducer * 1_024.0 < Denominator loop
               Reducer := Reducer * 1_024.0;
            end loop;

         exception
            when others => null;
         end;

         begin
            while Reducer * 2.0 < Denominator loop
               Reducer := Reducer * 2.0;
            end loop;

         exception
            when others => null;
         end;

         Denominator := Denominator - Reducer;
      end loop;

      if A < 0.0 then
         return -Denominator;
      else
         return Denominator;
      end if;
   end Exact_Remainder;

   ---------
   -- Exp --
   ---------

   function Exp (X : Float_Type) return Float_Type is
      Result : Float_Type;

   begin
      if X = 0.0 then
         return 1.0;

      else
         Result := Float_Type (Aux.Exp (Double (X)));

         --  The check here catches the case of Exp returning IEEE infinity

         if Result > Float_Type'Last then
            raise Constraint_Error;
         else
            return Result;
         end if;
      end if;
   end Exp;

   ----------------------
   -- Half_Log_Epsilon --
   ----------------------

   --  Cannot precompute this constant, because this is required to be a
   --  pure package, which allows no state. A pity, but no way around it!

   function Half_Log_Epsilon return Float_Type is
   begin
      return 0.5 * Float_Type (Aux.Log (DEpsilon));
   end Half_Log_Epsilon;

   ----------------
   -- Local_Atan --
   ----------------

   function Local_Atan
     (Y    : Float_Type;
      A    : Float_Type := 1.0)
      return Float_Type
   is
      Z        : Float_Type;
      Raw_Atan : Float_Type;

   begin
      if abs Y > abs A then
         Z := abs (A / Y);
      else
         Z := abs (Y / A);
      end if;

      if Z < Square_Root_Epsilon then
         Raw_Atan := Z;

      elsif Z = 1.0 then
         Raw_Atan := Pi / 4.0;

      elsif Z < Square_Root_Epsilon then
         Raw_Atan := Z;

      else
         Raw_Atan := Float_Type (Aux.Atan (Double (Z)));
      end if;

      if abs Y > abs A then
         Raw_Atan := Half_Pi - Raw_Atan;
      end if;

      if A > 0.0 then
         if Y > 0.0 then
            return Raw_Atan;
         else                 --  Y < 0.0
            return -Raw_Atan;
         end if;

      else                    --  A < 0.0
         if Y > 0.0 then
            return Pi - Raw_Atan;
         else                  --  Y < 0.0
            return -(Pi - Raw_Atan);
         end if;
      end if;
   end Local_Atan;

   ---------
   -- Log --
   ---------

   --  Natural base

   function Log (X : Float_Type) return Float_Type is
   begin
      if X < 0.0 then
         raise Argument_Error;

      elsif X = 0.0 then
         raise Constraint_Error;

      elsif X = 1.0 then
         return 0.0;
      end if;

      return Float_Type (Aux.Log (Double (X)));
   end Log;

   --  Arbitrary base

   function Log (X, Base : Float_Type) return Float_Type is
   begin
      if X < 0.0 then
         raise Argument_Error;

      elsif Base <= 0.0 or else Base = 1.0 then
         raise Argument_Error;

      elsif X = 0.0 then
         raise Constraint_Error;

      elsif X = 1.0 then
         return 0.0;
      end if;

      return Float_Type (Aux.Log (Double (X)) / Aux.Log (Double (Base)));
   end Log;

   -------------------------
   -- Log_Inverse_Epsilon --
   -------------------------

   --  Cannot precompute this constant, because this is required to be a
   --  pure package, which allows no state. A pity, but no way around it!

   function Log_Inverse_Epsilon return Float_Type is
   begin
      return Float_Type (Aux.Log (DIEpsilon));
   end Log_Inverse_Epsilon;

   ---------
   -- Sin --
   ---------

   --  Natural cycle

   function Sin (X : Float_Type) return Float_Type is
   begin
      if abs X < Square_Root_Epsilon then
         return X;
      end if;

      return Float_Type (Aux.Sin (Double (X)));
   end Sin;

   --  Arbitrary cycle

   function Sin (X, Cycle : Float_Type) return Float_Type is
      T : Float_Type;

   begin
      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif X = 0.0 then
         return X;
      end if;

      T := Exact_Remainder (X, Cycle) / Cycle;

      if T = 0.0 or T = 0.5 or T = -0.5 then
         return 0.0;

      elsif T = 0.25 or T = -0.75 then
         return 1.0;

      elsif T = -0.25 or T = 0.75 then
         return -1.0;

      end if;

      return Float_Type (Aux.Sin (Double (T * Two_Pi)));
   end Sin;

   ----------
   -- Sinh --
   ----------

   function Sinh (X : Float_Type) return Float_Type is
   begin
      if abs X < Square_Root_Epsilon then
         return X;

      elsif  X > Log_Inverse_Epsilon then
         return Exp (X - Log_Two);

      elsif X < -Log_Inverse_Epsilon then
         return -Exp ((-X) - Log_Two);
      end if;

      return Float_Type (Aux.Sinh (Double (X)));

   exception
      when others =>
         raise Constraint_Error;
   end Sinh;

   -------------------------
   -- Square_Root_Epsilon --
   -------------------------

   --  Cannot precompute this constant, because this is required to be a
   --  pure package, which allows no state. A pity, but no way around it!

   function Square_Root_Epsilon return Float_Type is
   begin
      return Float_Type (Aux.Sqrt (DEpsilon));
   end Square_Root_Epsilon;

   ----------
   -- Sqrt --
   ----------

   function Sqrt (X : Float_Type) return Float_Type is
   begin
      if X < 0.0 then
         raise Argument_Error;

      --  Special case Sqrt (0.0) to preserve possible minus sign per IEEE

      elsif X = 0.0 then
         return X;

      --  Sqrt (1.0) must be exact for good complex accuracy

      elsif X = 1.0 then
         return 1.0;

      end if;

      return Float_Type (Aux.Sqrt (Double (X)));
   end Sqrt;

   ---------
   -- Tan --
   ---------

   --  Natural cycle

   function Tan (X : Float_Type) return Float_Type is
   begin
      if abs X < Square_Root_Epsilon then
         return X;

      elsif abs X = Pi / 2.0 then
         raise Constraint_Error;
      end if;

      return Float_Type (Aux.tan (Double (X)));
   end Tan;

   --  Arbitrary cycle

   function Tan (X, Cycle : Float_Type) return Float_Type is
      T : Float_Type;
   begin

      if Cycle <= 0.0 then
         raise Argument_Error;

      elsif X = 0.0 then
         return X;
      end if;

      T := Exact_Remainder (X, Cycle) / Cycle;

      if T = 0.25
        or else T = 0.75
        or else T = -0.25
        or else T = -0.75
      then
         raise Constraint_Error;

      else
         return Sin (T * Two_Pi) / Cos (T * Two_Pi);
      end if;
   end Tan;

   ----------
   -- Tanh --
   ----------

   function Tanh (X : Float_Type) return Float_Type is
   begin
      if X < Half_Log_Epsilon then
         return -1.0;

      elsif X > -Half_Log_Epsilon then
         return 1.0;

      elsif abs X < Square_Root_Epsilon then
         return X;
      end if;

      return Float_Type (Aux.tanh (Double (X)));
   end Tanh;

   ----------------------------
   -- DEC-Specific functions --
   ----------------------------

   function Log10  (X : Float_Type) return Float_Type is
   begin
      return Log (X, 10.0);
   end Log10;

   function Log2   (X : Float_Type) return Float_Type is
   begin
      return Log (X, 2.0);
   end Log2;

   function Asin (X : Float_Type) return Float_Type renames Arcsin;
   function Acos (X : Float_Type) return Float_Type renames Arccos;

   function Arctan (X : Float_Type) return Float_Type is
   begin
      return Arctan (X, 1.0);
   end Arctan;

   function Arctan2 (Y, X : Float_Type) return Float_Type  renames Arctan;

   function Sind   (X : Float_Type) return Float_Type is
   begin
      return Sin (X, 360.0);
   end Sind;

   function Cosd   (X : Float_Type) return Float_Type is
   begin
      return Sin (X, 360.0);
   end Cosd;

   function Tand   (X : Float_Type) return Float_Type is
   begin
      return Tan (X, 360.0);
   end Tand;

   function Asind  (X : Float_Type) return Float_Type is
   begin
      return Arcsin (X, 360.0);
   end Asind;

   function Acosd  (X : Float_Type) return Float_Type is
   begin
      return Arccos (X, 360.0);
   end Acosd;

   function Atand (X : Float_Type) return Float_Type is
   begin
      return Arctan (X, 1.0, 360.0);
   end Atand;

   function Atand2 (Y, X : Float_Type) return Float_Type is
   begin
      return Arctan (Y, X, 360.0);
   end Atand2;

end Generic_Fast_Elementary_Functions;
