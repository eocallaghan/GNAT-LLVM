------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                     S Y S T E M . I N T E R R U P T S                    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                     Copyright (C) 2001-2008, AdaCore                     --
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

--  This is the VxWorks/Cert version of this package

with Interfaces.VxWorks;
with System.OS_Interface; use System.OS_Interface;

package body System.Interrupts is

   -----------------------
   -- Local Subprograms --
   -----------------------

   type Handlers_Array is array (Interrupt_ID) of Parameterless_Handler;
   pragma Suppress_Initialization (Handlers_Array);

   Handlers : Handlers_Array;
   --  The actual handlers

   procedure Install_Handler
     (Interrupt : Interrupt_ID;
      Handler   : Parameterless_Handler);
   --  Install the runtime umbrella handler for a vectored hardware interrupt

   procedure Default_Handler (Interrupt : System.Address);
   --  Default interrupt handler

   ---------------------
   -- Default_Handler --
   ---------------------

   procedure Default_Handler (Interrupt : System.Address) is
   begin
      Handlers (Interrupt_ID (Interrupt)).all;
   end Default_Handler;

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Handler
     (Interrupt : Interrupt_ID;
      Handler   : Parameterless_Handler)
   is
      Stat : Interfaces.VxWorks.STATUS;
      pragma Unreferenced (Stat);
   begin
      Handlers (Interrupt) := Handler;
      Stat := Interfaces.VxWorks.intConnect
        (Interfaces.VxWorks.Interrupt_Vector (System.Address (Interrupt)),
         Default_Handler'Access,
         System.Address (Interrupt));
   end Install_Handler;

   ---------------------------------
   -- Install_Restricted_Handlers --
   ---------------------------------

   procedure Install_Restricted_Handlers (Handlers : Handler_Array) is
   begin
      for J in Handlers'Range loop
         Install_Handler (Handlers (J).Interrupt, Handlers (J).Handler);
      end loop;
   end Install_Restricted_Handlers;

end System.Interrupts;
