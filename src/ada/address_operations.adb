------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   A D D R E S S _ O P E R A T I O N S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2000 Free Software Foundation, Inc.          --
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

--  This is the package body for use with the Address_Operations package
--  from the Dec Library. We simply use the standard operations in the
--  Ada 95 System.Storage_Elements package, rather than the coding in
--  the original DEC body, which was not completely portable.

with System;
with System.Storage_Elements;

package body Address_Operations is

   package SSE renames System.Storage_Elements;

   ---------
   -- "+" --
   ---------

   function "+"
     (Left  : System.Address;
      Right : Integer_Type)
      return  System.Address
   is
   begin
      return SSE."+" (Left, SSE.Storage_Offset (Right));
   end "+";

   function "+"
     (Left  : Integer_Type;
      Right : System.Address)
      return  System.Address
   is
   begin
      return SSE."+" (SSE.Storage_Offset (Left), Right);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-"
     (Left  : System.Address;
      Right : System.Address)
      return  Integer_Type
   is
   begin
      return Integer_Type (SSE."-" (Left, Right));
   end "-";

   function "-"
     (Left  : System.Address;
      Right : Integer_Type)
      return  System.Address
   is
   begin
      return SSE."-" (Left, SSE.Storage_Offset (Right));
   end "-";

   ----------------
   -- To_Address --
   ----------------

   function To_Address (X : Integer_Type) return System.Address is

      pragma Suppress (All_Checks);
      --  This is not just for efficiency. It ensures that we do not run into
      --  trouble with negative addresses, which DEC expects to be supported.

   begin
      return SSE.To_Address (SSE.Integer_Address (X));
   end To_Address;

   ---------------------
   -- To_Integer_Type --
   ---------------------

   function To_Integer_Type (X : System.Address) return Integer_Type is
   begin
      return Integer_Type (SSE.To_Integer (X));
   end To_Integer_Type;

end Address_Operations;
