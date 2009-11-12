------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   A D A . E X E C U T I O N _ T I M E                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2005-2008, Free Software Foundation, Inc.        --
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

--  This is a MaRTE OS version of this package

pragma Style_Checks (All_Checks);
--  Turn off mode in check for procedures since MaRTE packages do not follow
--  -gnatg style checks yet. ???

with System.Tasking;
with System.Task_Primitives.Operations;

with Ada.Unchecked_Conversion;
with Interfaces.C;

with MaRTE.Kernel.Timers;
with MaRTE.HAL;

package body Ada.Execution_Time is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function To_MaRTE_Task_Id
     (Ada_Task_Id : Ada.Task_Identification.Task_Id)
      return MaRTE.Kernel.Task_Id;
   pragma Inline (To_MaRTE_Task_Id);
   --  Conversion between run time and MaRTE task identifiers

   ---------
   -- "+" --
   ---------

   function "+"
     (Left  : CPU_Time;
      Right : Ada.Real_Time.Time_Span) return CPU_Time
   is
      use type Ada.Real_Time.Time;
   begin
      return CPU_Time (Ada.Real_Time.Time (Left) + Right);
   end "+";

   function "+"
     (Left  : Ada.Real_Time.Time_Span;
      Right : CPU_Time) return CPU_Time
   is
      use type Ada.Real_Time.Time;
   begin
      return CPU_Time (Left + Ada.Real_Time.Time (Right));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-"
     (Left  : CPU_Time;
      Right : Ada.Real_Time.Time_Span) return CPU_Time
   is
      use type Ada.Real_Time.Time;
   begin
      return CPU_Time (Ada.Real_Time.Time (Left) - Right);
   end "-";

   function "-"
     (Left  : CPU_Time;
      Right : CPU_Time) return Ada.Real_Time.Time_Span
   is
      use type Ada.Real_Time.Time;
   begin
      return (Ada.Real_Time.Time (Left) - Ada.Real_Time.Time (Right));
   end "-";

   ----------------------
   -- To_MaRTE_Task_Id --
   ----------------------

   function To_MaRTE_Task_Id
     (Ada_Task_Id : Ada.Task_Identification.Task_Id)
      return MaRTE.Kernel.Task_Id
   is
      function To_Task_Id is new Ada.Unchecked_Conversion
        (Ada.Task_Identification.Task_Id, System.Tasking.Task_Id);

   begin
      return
        MaRTE.Kernel.Task_Id
          (System.Task_Primitives.Operations.Get_Thread_Id
            (To_Task_Id (Ada_Task_Id)));
   end To_MaRTE_Task_Id;

   -----------
   -- Clock --
   -----------

   function Clock
     (T : Ada.Task_Identification.Task_Id :=
            Ada.Task_Identification.Current_Task) return CPU_Time
   is
      Clock : aliased MaRTE.Kernel.Timers.Clock_Id;

      function To_Time is new Ada.Unchecked_Conversion
        (Duration, Ada.Real_Time.Time);

      use type Interfaces.C.int, Ada.Task_Identification.Task_Id;

   begin
      if T = Ada.Task_Identification.Null_Task_Id then
         raise Program_Error;
      end if;

      --  How to know if an Ada task has terminated but the underlying thread
      --  has not finished yet?

      if MaRTE.Kernel.Timers.Pthread_Getcpuclockid
          (To_MaRTE_Task_Id (T), Clock'Access) /= 0
      then
         --  Assumes invalid task ID, probably because the task has terminated

         raise Tasking_Error;
      end if;

      return CPU_Time
        (To_Time (MaRTE.HAL.HWTime_To_Duration
         (MaRTE.Kernel.Timers.Get_Time (Clock))));
   end Clock;

   -----------
   -- Split --
   -----------

   procedure Split
     (T  : CPU_Time;
      SC : out Ada.Real_Time.Seconds_Count;
      TS : out Ada.Real_Time.Time_Span)
   is
      use type Ada.Real_Time.Time;
   begin
      Ada.Real_Time.Split (Ada.Real_Time.Time (T), SC, TS);
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (SC : Ada.Real_Time.Seconds_Count;
      TS : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero)
      return CPU_Time
   is
   begin
      return CPU_Time (Ada.Real_Time.Time_Of (SC, TS));
   end Time_Of;

end Ada.Execution_Time;
