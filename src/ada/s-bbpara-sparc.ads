------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . B B . P A R A M E T E R S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
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

--  This package defines basic parameters used by the low level tasking system

--  This is the ERC32/LEON version of this package

pragma Restrictions (No_Elaboration_Code);

with System.Parameters;

package System.BB.Parameters is
   pragma Pure;

   -----------------------
   -- Stack information --
   -----------------------

   --  Boundaries of the stack for the environment task, defined by the linker
   --  script file.

   Top_Of_Environment_Stack : constant System.Address;
   pragma Import (Asm, Top_Of_Environment_Stack, "__stack");
   --  Top of the stack to be used by the environment task

   Bottom_Of_Environment_Stack : constant System.Address;
   pragma Import (Asm, Bottom_Of_Environment_Stack, "_stack_start");
   --  Bottom of the stack to be used by the environment task

   --------------------
   -- Hardware clock --
   --------------------

   Clock_Frequency : constant Natural;  --  Hertz
   pragma Import (Asm, Clock_Frequency, "clock_frequency");
   --  Frequency of the system clock

   ----------------
   -- Interrupts --
   ----------------

   --  The followings are ERC32/LEON definitions and they cannot be modified

   --  These definitions are in this package in order to isolate target
   --  dependencies.

   Interrupt_Levels : constant := 15;
   --  Number of interrupt levels in the SPARC architecture

   subtype Interrupt_Level is Natural range 0 .. Interrupt_Levels;
   --  Type that defines the range of possible interrupt levels

   subtype Range_Of_Vector is Natural range 0 .. 255;
   --  The SPARC arquitecture supports 256 vectors

   ------------
   -- Stacks --
   ------------

   Interrupt_Stack_Size : constant := 4 * 1024;  --  bytes
   --  Size of each of the interrupt stacks

end System.BB.Parameters;
