------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     ADA.EXCEPTIONS.EXCEPTION_PROPAGATION                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
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

--  This is the dummy version of this package for SJLJ implementations of
--  exception handling.

separate (Ada.Exceptions)
package body Exception_Propagation is

   ---------------------
   -- Setup_Exception --
   ---------------------

   procedure Setup_Exception
     (Excep    : EOA;
      Current  : EOA;
      Reraised : Boolean := False)
   is
      pragma Unreferenced (Excep, Current, Reraised);
   begin
      null;
   end Setup_Exception;

   -------------------------
   -- Propagate_Exception --
   -------------------------

   procedure Propagate_Exception (From_Signal_Handler : Boolean) is
      pragma Unreferenced (From_Signal_Handler);

   begin
      --  The following junk raise of Program_Error is required because
      --  this is a No_Return procedure.

      raise Program_Error;
   end Propagate_Exception;

end Exception_Propagation;
