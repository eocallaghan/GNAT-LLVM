------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                     S Y S T E M . V X W O R K S . E X T                  --
--                                                                          --
--                                   S p e c                                --
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

--  This package provides VxWorks specific support functions needed
--  by System.OS_Interface.

--  This is the VxWorks 653 vThreads version of this package

with Interfaces.C;

package System.VxWorks.Ext is
   pragma Preelaborate;

   subtype SEM_ID is Long_Integer;
   --  typedef struct semaphore *SEM_ID;

   type t_id is new Long_Integer;
   subtype int is Interfaces.C.int;

   type Interrupt_Handler is access procedure (parameter : System.Address);
   pragma Convention (C, Interrupt_Handler);

   type Interrupt_Vector is new System.Address;
   function Int_Lock return int;
   pragma Inline (Int_Lock);

   function Int_Unlock return int;
   pragma Inline (Int_Unlock);

   function Interrupt_Connect
     (Vector    : Interrupt_Vector;
      Handler   : Interrupt_Handler;
      Parameter : System.Address := System.Null_Address) return int;
   pragma Convention (C, Interrupt_Connect);

   function Interrupt_Number_To_Vector
     (intNum : int) return Interrupt_Vector;
   pragma Convention (C, Interrupt_Number_To_Vector);

   function semDelete (Sem : SEM_ID) return int;
   pragma Import (C, semDelete, "semDelete");

   function Task_Cont (tid : t_id) return int;
   pragma Import (C, Task_Cont, "taskResume");

   function Task_Stop (tid : t_id) return int;
   pragma Import (C, Task_Stop, "taskSuspend");

   function kill (pid : t_id; sig : int) return int;
   pragma Import (C, kill, "kill");

   function getpid return t_id;
   pragma Import (C, getpid, "taskIdSelf");

   function Set_Time_Slice (ticks : int) return int;
   pragma Import (C, Set_Time_Slice, "kernelTimeSlice");

   type UINT64 is mod 2 ** Long_Long_Integer'Size;

   function tickGet return UINT64;
   --  "tickGet" not available for cert vThreads:
   pragma Import (C, tickGet, "tick64Get");

   --------------------------------
   -- Processor Affinity for SMP --
   --------------------------------

   function taskCpuAffinitySet (tid : t_id; CPU : int) return int;
   pragma Convention (C, taskCpuAffinitySet);
   --  For SMP run-times set the CPU affinity.
   --  For uniprocessor systems return ERROR status.

end System.VxWorks.Ext;
