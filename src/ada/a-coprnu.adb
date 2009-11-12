------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--         A D A . C O N T A I N E R S . P R I M E _ N U M B E R S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2006, Free Software Foundation, Inc.         --
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

package body Ada.Containers.Prime_Numbers is

   --------------
   -- To_Prime --
   --------------

   function To_Prime (Length : Count_Type) return Hash_Type is
      I, J, K : Integer'Base;
      Index   : Integer'Base;

   begin
      I := Primes'Last - Primes'First;
      Index := Primes'First;
      while I > 0 loop
         J := I / 2;
         K := Index + J;

         if Primes (K) < Hash_Type (Length) then
            Index := K + 1;
            I := I - J - 1;
         else
            I := J;
         end if;
      end loop;

      return Primes (Index);
   end To_Prime;

end Ada.Containers.Prime_Numbers;
