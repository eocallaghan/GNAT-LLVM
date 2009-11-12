------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . E X E C U T I O N _ T I M E . T I M E R S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2005-2008, Free Software Foundation, Inc.        --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

with System;
with MaRTE.Kernel.Timed_Handlers.Ada_Timing_Events;

package Ada.Execution_Time.Timers is

   type Timer (T : not null access constant Ada.Task_Identification.Task_Id) is
      tagged limited private;

   type Timer_Handler is access protected procedure (TM : in out Timer);

   Min_Handler_Ceiling : constant System.Any_Priority :=
                           System.Interrupt_Priority'Last;

   procedure Set_Handler
     (TM      : in out Timer;
      In_Time : Ada.Real_Time.Time_Span;
      Handler : Timer_Handler);

   procedure Set_Handler
     (TM      : in out Timer;
      At_Time : CPU_Time;
      Handler : Timer_Handler);

   function Current_Handler (TM : Timer) return Timer_Handler;

   procedure Cancel_Handler
     (TM        : in out Timer;
      Cancelled : out Boolean);

   function Time_Remaining (TM : Timer) return Ada.Real_Time.Time_Span;

   Timer_Resource_Error : exception;

private
   type Timer (T : not null access constant Ada.Task_Identification.Task_Id) is
      new MaRTE.Kernel.Timed_Handlers.Ada_Timing_Events.Base_Timing_Event
        with null record;

   procedure Initialize (TM : in out Timer);
end Ada.Execution_Time.Timers;
