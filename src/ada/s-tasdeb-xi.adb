------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                  S Y S T E M . T A S K I N G . D E B U G                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1997-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This package encapsulates all direct interfaces to task debugging services
--  that are needed by gdb with gnat mode.

--  Note : This file *must* be compiled with debugging information

--  Do not add any dependency to GNARL packages since this package is used
--  in both normal and restricted (ravenscar) environments.

package body System.Tasking.Debug is

   Max_Number_Of_Known_Tasks : constant := 1_000;
   --  Maximum number of known tasks visible from GDB.
   --  ??? For now, this number is hard-coded in GDB, so it should not
   --  be changed. As soon as GDB will be able to read this constant,
   --  Max_Number_Of_Known_Tasks will be set to a smaller number,
   --  e.g. 64.

   Known_Tasks : array (0 .. Max_Number_Of_Known_Tasks - 1) of Task_Id :=
                   (others => null);
   --  Global array of tasks read by gdb, and updated by
   --  Create_Task and Finalize_TCB.

   procedure Add_Task_Id (T : Task_Id) is
   begin
      for J in Known_Tasks'Range loop
         if Known_Tasks (J) = null then
            Known_Tasks (J) := T;
            exit;
         end if;
      end loop;
   end Add_Task_Id;

end System.Tasking.Debug;
