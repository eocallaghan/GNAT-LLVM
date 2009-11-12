------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                     S Y S T E M . V X W O R K S . E X T                  --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--            Copyright (C) 2008-2009, Free Software Foundation, Inc.       --
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

--  This is the VxWorks 6 RTP version of this package

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

   --------------------
   -- Set_Time_Slice --
   --------------------

   function Set_Time_Slice (ticks : int) return int is
      pragma Unreferenced (ticks);
   begin
      return ERROR;
   end Set_Time_Slice;

   function Interrupt_Connect
     (Vector    : Interrupt_Vector;
      Handler   : Interrupt_Handler;
      Parameter : System.Address := System.Null_Address) return int is
      pragma Unreferenced (Vector, Handler, Parameter);
   begin
      return ERROR;
   end Interrupt_Connect;

   function Interrupt_Number_To_Vector
     (intNum : int) return Interrupt_Vector is
      pragma Unreferenced (intNum);
   begin
      return 0;
   end Interrupt_Number_To_Vector;

   ------------------------
   -- taskCpuAffinitySet --
   ------------------------

   function taskCpuAffinitySet (tid : t_id; CPU : int) return int is
      pragma Unreferenced (tid, CPU);
   begin
      return ERROR;
   end taskCpuAffinitySet;

end System.VxWorks.Ext;
