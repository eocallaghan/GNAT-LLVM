------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                 S Y S T E M . T A S K _ P R I M I T I V E S              --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 2001-2005, Free Software Foundation, Inc.         --
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

--  This is the HI-E version of this package

with System.OS_Interface;

package System.Task_Primitives is
   pragma Preelaborate;

   type Task_Body_Access is access procedure;
   --  Pointer to the task body's entry point (or possibly a wrapper
   --  declared local to the GNARL).

   type Private_Data is limited private;
   --  Any information that the GNULLI needs maintained on a per-task
   --  basis.  A component of this type is guaranteed to be included
   --  in the Ada_Task_Control_Block.

private

   type Private_Data is record
      Thread : aliased System.OS_Interface.t_id := 0;
      pragma Atomic (Thread);
      --  Thread field may be updated by two different threads of control.
      --  (See, Enter_Task and Create_Task in s-taprop.adb).
      --  They put the same value (thr_self value). We do not want to
      --  use lock on those operations and the only thing we have to
      --  make sure is that they are updated in atomic fashion.

      CV : aliased System.OS_Interface.SEM_ID;
   end record;

end System.Task_Primitives;
