------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--    A D A . E X C E P T I O N S . L A S T _ C H A N C E _ H A N D L E R   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2008, Free Software Foundation, Inc.         --
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

--  Default last chance handler for use with the ravenscar-cert run-time lib

--  Logs error with health monitor, and dumps exception identity and partial
--  argument string for vxaddr2line for generation of a symbolic stack
--  backtrace.

--  This version cannot reference "adainit" to form the vxaddr2line arguments,
--  as it can be installed in a shared library, possibly with the cert run
--  time. "adainit" is only available in a partition containing an Ada main.

with GNAT.IO;                  use GNAT.IO;
with GNAT.Debug_Utilities;     use GNAT.Debug_Utilities;
with System.Standard_Library;  use System.Standard_Library;
with System;

procedure Ada.Exceptions.Last_Chance_Handler (Except : Exception_Occurrence) is

   Max_Error_Message_Size : constant := 128;

   subtype Error_Message_Size_Type is Integer range
      1 .. Max_Error_Message_Size;

   ----------------------
   -- VxWorks Imports --
   ----------------------

   procedure Stop (ID : Integer := 0);
   pragma Import (C, Stop, "taskStop");
   pragma No_Return (Stop);
   --  Although taskSuspend returns a result, we ignore it,
   --  since in this case (ID = 0 = taskIdSelf) it does not return

   Message : String (1 .. Max_Error_Message_Size);

   Message_Length : Error_Message_Size_Type;

begin
   if Except.Id.Name_Length + 25 > Max_Error_Message_Size then
      Message_Length := Max_Error_Message_Size;
   else
      Message_Length := Except.Id.Name_Length + 25;
   end if;

   Message (1 .. 25) := "Unhandled Ada Exception: ";
   Message (26 .. Message_Length) :=
     To_Ptr (Except.Id.Full_Name) (1 .. Message_Length - 25);

   New_Line;
   Put_Line ("In last chance handler");
   Put_Line (Message);
   New_Line;

   Put_Line ("traceback addresses for vxaddr2line:");

   --  Dump backtrace PC values

   for J in 1 .. Except.Num_Tracebacks loop
      Put (Image_C (Except.Tracebacks (J)));
      Put (" ");
   end loop;

   New_Line;

   Stop;
end Ada.Exceptions.Last_Chance_Handler;
