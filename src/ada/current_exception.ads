------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    C U R R E N T _ E X C E P T I O N                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1996-2005, Free Software Foundation, Inc.          --
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

--  This package is intended to duplicate the interface provided by the
--  OpenVMS package Current_Exception, as described in the DEC Ada manuals.

package Current_Exception is
   pragma Pure;

   function Exception_Information return String;
   --  Returns the result of calling Ada.Exceptions.Exception_Information
   --  with an argument that is the Exception_Occurrence corresponding to
   --  the current exception. Returns the null string if called from outside
   --  an exception handler.

   function Exception_Message return String;
   --  Returns the result of calling Ada.Exceptions.Exception_Message with
   --  an argument that is the Exception_Occurrence corresponding to the
   --  current exception. Returns the null string if called from outside an
   --  exception handler.

   function Exception_Name return String;
   --  Returns the result of calling Ada.Exceptions.Exception_Name with
   --  an argument that is the Exception_Occurrence corresponding to the
   --  current exception. Returns the null string if called from outside an
   --  exception handler.

private
   pragma Import (Intrinsic, Exception_Information);
   pragma Import (intrinsic, Exception_Message);
   pragma Import (Intrinsic, Exception_Name);

end Current_Exception;
