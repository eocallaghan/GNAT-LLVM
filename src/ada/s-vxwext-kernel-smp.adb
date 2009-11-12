------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                     S Y S T E M . V X W O R K S . E X T                  --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--          Copyright (C) 2008-2009, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
------------------------------------------------------------------------------

--  This package provides vxworks specific support functions needed
--  by System.OS_Interface.

--  This is the VxWorks 6 SMP kernel version of this package

package body System.VxWorks.Ext is

   ERROR : constant := -1;

   --------------
   -- Int_Lock --
   --------------

   function Int_Lock return int is
   begin
      return ERROR;
   end Int_Lock;

   ----------------
   -- Int_Unlock --
   ----------------

   function Int_Unlock return int is
   begin
      return ERROR;
   end Int_Unlock;

   ---------------
   -- semDelete --
   ---------------

   function semDelete (Sem : SEM_ID) return int is
      function Os_Sem_Delete (Sem : SEM_ID) return int;
      pragma Import (C, Os_Sem_Delete, "semDelete");
   begin
      return Os_Sem_Delete (Sem);
   end semDelete;

   ------------------------
   -- taskCpuAffinitySet --
   ------------------------

   function taskCpuAffinitySet (tid : t_id; CPU : int) return int
   is
      function Set_Affinity (tid : t_id; CPU : int) return int;
      pragma Import (C, Set_Affinity, "__gnat_set_affinity");
   begin
      return Set_Affinity (tid, CPU);
   end taskCpuAffinitySet;

end System.VxWorks.Ext;
