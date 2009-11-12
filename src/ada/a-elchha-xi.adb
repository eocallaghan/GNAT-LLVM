------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--    A D A . E X C E P T I O N S . L A S T _ C H A N C E _ H A N D L E R   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2006, Free Software Foundation, Inc.         --
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

with System.IO;                use System.IO;
with GNAT.Debug_Utilities;     use GNAT.Debug_Utilities;
with System;
with System.OS_Interface;

--  Default last chance handler for use with full run-time library on bare
--  board targets.

--  It dumps the exception identity and the non-symbolic traceback from the
--  point where the exception was triggered.

procedure Ada.Exceptions.Last_Chance_Handler (Except : Exception_Occurrence) is
begin
   Put_Line ("In last chance handler");
   Put_Line ("Unhandled Ada Exception: " & Exception_Name (Except));
   Put_Line ("Call stack traceback locations:");

   --  Dump backtrace PC values

   for J in 1 .. Except.Num_Tracebacks loop
      Put (Image_C (Except.Tracebacks (J)));
      Put (" ");
   end loop;

   New_Line;

   --  Suspend forever

   System.OS_Interface.Sleep;

   --  The following junk raise of Program_Error is required because
   --  this is a No_Return function, and unfortunately Suspend can
   --  return (although this particular call won't).

   raise Program_Error;

end Ada.Exceptions.Last_Chance_Handler;
