------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   A D A . I N T E R R U P T S . N A M E S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1991-2008, Free Software Foundation, Inc.         --
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

--  This is the version for ERC32 targets of this package

with Ada.Interrupts;

with System.OS_Interface;

package Ada.Interrupts.Names is

   -------------------------
   -- External Interrupts --
   -------------------------

   External_Interrupt_4 : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.External_Interrupt_4);
   External_Interrupt_4_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.External_Interrupt_4_Priority;

   External_Interrupt_3 : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.External_Interrupt_3);
   External_Interrupt_3_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.External_Interrupt_3_Priority;

   External_Interrupt_2 : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.External_Interrupt_2);
   External_Interrupt_2_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.External_Interrupt_2_Priority;

   External_Interrupt_1 : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.External_Interrupt_1);
   External_Interrupt_1_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.External_Interrupt_1_Priority;

   External_Interrupt_0 : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.External_Interrupt_0);
   External_Interrupt_0_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.External_Interrupt_0_Priority;

   -----------------------
   -- Timers Interrupts --
   -----------------------

   Watch_Dog_Time_Out : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.Watch_Dog_Time_Out);
   Watch_Dog_Time_Out_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.Watch_Dog_Time_Out_Priority;

   Real_Time_Clock : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.Real_Time_Clock);
   Real_Time_Clock_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.Real_Time_Clock_Priority;

   General_Purpose_Timer : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.General_Purpose_Timer);
   General_Purpose_Timer_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.General_Purpose_Timer_Priority;

   --------------------
   -- DMA Interrupts --
   --------------------

   DMA_Time_Out : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.DMA_Time_Out);
   DMA_Time_Out_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.DMA_Time_Out_Priority;

   DMA_Access_Error : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.DMA_Access_Error);
   DMA_Access_Error_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.DMA_Access_Error_Priority;

   ---------------------
   -- UART Interrupts --
   ---------------------

   UART_Error : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.UART_Error);
   UART_Error_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.UART_Error_Priority;

   UART_B_Ready : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.UART_B_Ready);
   UART_B_Ready_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.UART_B_Ready_Priority;

   UART_A_Ready : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.UART_A_Ready);
   UART_A_Ready_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.UART_A_Ready_Priority;

   -----------------------------
   -- Miscelaneous Interrupts --
   -----------------------------

   Correctable_Error_In_Memory : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.Correctable_Error_In_Memory);
   Correctable_Error_In_Memory_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.Correctable_Error_In_Memory_Priority;

   Masked_Hardware_Errors : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.Masked_Hardware_Errors);
   Masked_Hardware_Errors_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.Masked_Hardware_Errors_Priority;

end Ada.Interrupts.Names;
