------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                  S Y S T E M . B B . I N T E R R U P T S                 --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2004 The European Space Agency            --
--                     Copyright (C) 2003-2008, AdaCore                     --
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
-- The porting of GNARL to bare board  targets was initially  developed  by --
-- the Real-Time Systems Group at the Technical University of Madrid.       --
--                                                                          --
------------------------------------------------------------------------------

--  Package in charge of implementing the basic routines for interrupt
--  management.

pragma Restrictions (No_Elaboration_Code);

with System;
with System.BB.Parameters;
with System.BB.Peripherals;

package System.BB.Interrupts is
   pragma Preelaborate;

   Max_Interrupt : constant := System.BB.Parameters.Interrupt_Levels;
   --  The interrupts are distinguished by its interrupt level

   subtype Interrupt_ID is Natural range 0 .. Max_Interrupt;
   --  Interrupt identifier

   No_Interrupt : constant Interrupt_ID := 0;
   --  Special value indicating no interrupt

   type Interrupt_Handler is access procedure (Id : Interrupt_ID);
   --  Prototype of procedures used as low level handlers

   procedure Initialize_Interrupts;
   --  Initialize table containing the pointers to the different interrupt
   --  stacks. Should be called before any other subprograms in this package.

   procedure Attach_Handler
     (Handler : Interrupt_Handler;
      Id      : Interrupt_ID);
   pragma Inline (Attach_Handler);
   --  Attach the procedure Handler as handler of the interrupt Id

   function Priority_Of_Interrupt
     (Id : Interrupt_ID) return System.Any_Priority
   renames
     System.BB.Peripherals.Priority_Of_Interrupt;
   --  This function returns the software priority associated to the interrupt
   --  given as argument.

   function Current_Interrupt return Interrupt_ID;
   pragma Inline (Current_Interrupt);
   --  Function that returns the hardware interrupt currently being
   --  handled (if any). In case no hardware interrupt is being handled
   --  the returned value is No_Interrupt.

   function Within_Interrupt_Stack
     (Stack_Address : System.Address) return Boolean;
   pragma Inline (Within_Interrupt_Stack);
   --  Function that tells whether the Address passed as argument belongs to
   --  the interrupt stack that is currently being used (if any). It returns
   --  True if Stack_Address is within the range of the interrupt stack being
   --  used. In case Stack_Address is not within the interrupt stack (or no
   --  interrupt is being handled)

end System.BB.Interrupts;
