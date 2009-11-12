------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . E X C E P T I O N S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2008, Free Software Foundation, Inc.          --
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

package body System.Exceptions is

   ---------------------------
   -- Debug_Raise_Exception --
   ---------------------------

   procedure Debug_Raise_Exception (E : SSL.Exception_Data_Ptr) is
      pragma Inspection_Point (E);
   begin
      null;
   end Debug_Raise_Exception;

   -------------------------------
   -- Debug_unhandled_Exception --
   -------------------------------

   procedure Debug_Unhandled_Exception (E : SSL.Exception_Data_Ptr) is
      pragma Inspection_Point (E);
   begin
      null;
   end Debug_Unhandled_Exception;

   --------------------------------
   -- Debug_Raise_Assert_Failure --
   --------------------------------

   procedure Debug_Raise_Assert_Failure is
   begin
      null;
   end Debug_Raise_Assert_Failure;

   -----------------
   -- Local_Raise --
   -----------------

   procedure Local_Raise (Excep : System.Address) is
      pragma Warnings (Off, Excep);
   begin
      return;
   end Local_Raise;

end System.Exceptions;
