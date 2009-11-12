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

--  Default last chance handler for use with the Cert run-time library on
--  generic platforms (such as native ones).

--  Dump exception name and non-symbolic stack backtrace, then exit

with GNAT.IO;                  use GNAT.IO;
with GNAT.Debug_Utilities;     use GNAT.Debug_Utilities;
with System.Standard_Library;  use System.Standard_Library;
with System;

procedure Ada.Exceptions.Last_Chance_Handler (Except : Exception_Occurrence) is

   procedure OS_Exit;
   pragma Import (C, OS_Exit, "exit");
   pragma No_Return (OS_Exit);

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

   --  We finish the application here. We do not support multiple threads
   --  of execution here.

   OS_Exit;
end Ada.Exceptions.Last_Chance_Handler;
