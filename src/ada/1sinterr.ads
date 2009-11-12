------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                     S Y S T E M . I N T E R R U P T S                    --
--                                                                          --
--                                  S p e c                                 --
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

--  This is the Ravenscar version of this package

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes.

--  This package encapsulates the implementation of interrupt or signal
--  handlers. It is logically an extension of the body of Ada.Interrupts.
--  It is made a child of System to allow visibility of various
--  runtime system internal data and operations.

with System.Tasking.Protected_Objects.Single_Entry;

with System.OS_Interface;
--  used for Max_Interrupt

package System.Interrupts is
   pragma Elaborate_Body;

   package ST renames System.Tasking;

   -------------------------
   -- Constants and types --
   -------------------------

   Default_Interrupt_Priority : constant System.Interrupt_Priority :=
                                  System.Interrupt_Priority'Last;
   --  Default value used when a pragma Interrupt_Handler or Attach_Handler is
   --  specified without an Interrupt_Priority pragma, see D.3(10).

   type Ada_Interrupt_ID is range 0 .. System.OS_Interface.Max_Interrupt;
   --  Avoid inheritance by Ada.Interrupts.Interrupt_ID of unwanted operations

   type Interrupt_ID is range 0 .. System.OS_Interface.Max_Interrupt;

   --  The following renaming is introduced so that the type is accessible
   --  through rtsfind, otherwise the name clashes with its homonym in
   --  ada.interrupts.

   subtype System_Interrupt_Id is Interrupt_ID;

   type Parameterless_Handler is access protected procedure;

   type Handler_Index is range 0 .. Integer'Last;

   type Handler_Item is record
      Interrupt : Interrupt_ID;
      Handler   : Parameterless_Handler;
   end record;
   --  Contains all the information from an Attach_Handler pragma

   type Handler_Array is array (Handler_Index range <>) of Handler_Item;

   --------------------------------
   -- Interrupt entries services --
   --------------------------------

   -----------------------------
   -- Interrupt entry service --
   -----------------------------

   procedure Install_Restricted_Handlers (Handlers : Handler_Array);
   --  Install the static Handlers for the given Interrupts

end System.Interrupts;
