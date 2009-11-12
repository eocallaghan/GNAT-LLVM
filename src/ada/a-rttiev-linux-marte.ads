------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          A D A . R E A L _ T I M E . T I M I N G _ E V E N T S           --
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
--                                                                          --
------------------------------------------------------------------------------

--  This is a MaRTE OS version of this package

with MaRTE.Kernel.Timed_Handlers.Ada_Timing_Events;

package Ada.Real_Time.Timing_Events is

   type Timing_Event is tagged limited private;

   type Timing_Event_Handler
     is access protected procedure (Event : in out Timing_Event);

   procedure Set_Handler
     (Event   : in out Timing_Event;
      At_Time : Time;
      Handler : Timing_Event_Handler);

   procedure Set_Handler
     (Event   : in out Timing_Event;
      In_Time : Time_Span;
      Handler : Timing_Event_Handler);

   function Current_Handler
     (Event : Timing_Event) return Timing_Event_Handler;

   procedure Cancel_Handler
     (Event     : in out Timing_Event;
      Cancelled : out Boolean);

   function Time_Of_Event (Event : Timing_Event) return Time;

private
   type Timing_Event is
      new MaRTE.Kernel.Timed_Handlers.Ada_Timing_Events.Base_Timing_Event
        with null record;

   procedure Initialize (Event : in out Timing_Event);
end Ada.Real_Time.Timing_Events;
