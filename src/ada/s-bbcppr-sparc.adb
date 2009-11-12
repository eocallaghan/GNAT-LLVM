------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ P R I M I T I V E S            --
--                                                                          --
--                                  B o d y                                 --
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

pragma Restrictions (No_Elaboration_Code);

with System.Storage_Elements;

package body System.BB.CPU_Primitives is

   package SSE renames System.Storage_Elements;
   use type SSE.Integer_Address;
   use type SSE.Storage_Offset;

   ----------------
   -- Local data --
   ----------------

   SP  : constant Range_Of_Context :=  6;
   PC  : constant Range_Of_Context :=  7;
   PSR : constant Range_Of_Context :=  8;
   WIM : constant Range_Of_Context := 17;
   WIN : constant Range_Of_Context := 18;
   O0  : constant Range_Of_Context :=  0;
   --  These are the registers that are initialized: Program Counter,
   --  Stack Pointer, Window Invalid Mask, and Processor State Register.
   --  Moreover, the first input argument and the number of register windows
   --  to be restored are also initialized.

   FP : constant SSE.Storage_Offset := 56;
   --  The frame pointer needs also to be initialized; however, it is not
   --  kept in the thread descriptor but in the stack, and this value
   --  represents the offset from the stack pointer (expressed in bytes).

   type Trap_Entry is
      record
         First_Instr  : SSE.Integer_Address;
         Second_Instr : SSE.Integer_Address;
         Third_Instr  : SSE.Integer_Address;
         Fourth_Instr : SSE.Integer_Address;
      end record;
   --  Each entry in the trap table contains the four first instructions that
   --  will be executed as part of the handler. A trap is a vectored transfer
   --  of control to the supervisor through a special trap table that contains
   --  the first four instructions of each trap handler. The base address of
   --  the table is established by supervisor and the displacement, within the
   --  table, is determined by the trap type.

   Trap_Table : array (Parameters.Range_Of_Vector) of Trap_Entry;
   pragma Import (Asm, Trap_Table, "trap_table");
   --  This is the trap table, that is defined in the crt0 file. This table
   --  contains the trap entry for all the traps (synchronous and asynchronous)
   --  defined by the SPARC architecture.

   User_Vector_Table : array (Parameters.Range_Of_Vector) of System.Address;
   pragma Export (Asm, User_Vector_Table, "user_vector_table");
   --  In addition to the trap table there is another table that contains the
   --  addresses for the trap handlers defined by the user.

   ------------------------
   -- Initialize_Context --
   ------------------------

   procedure Initialize_Context
     (Buffer          : not null access Context_Buffer;
      Program_Counter : System.Address;
      Argument        : System.Address;
      Stack_Pointer   : System.Address)
   is
   begin
      --  The stack must be aligned to 16. 96 bytes are needed for storing
      --  a whole register window (96 bytes).

      Buffer (SP) := SSE.To_Address
        ((SSE.To_Integer (Stack_Pointer) / 16) * 16 - 96);

      --  Initialize PSR with the state expected by the context switch routine.
      --  Floating point unit is disabled. Traps are enabled, although
      --  interrupts are disabled (after the context switch only interrupts
      --  with a lower priority than the task will be masked). The supervisor
      --  and previous supervisor are set to 1 (the system always operates in
      --  supervisor mode).

      --  CWP = 0, ET = 1, PS = 1, S = 1, and PIL = 15

      Buffer (PSR) := SSE.To_Address (16#0FE0#);

      --  The WIM is initialized to 2 (corresponding to CWP = 1)

      Buffer (WIM) := SSE.To_Address (2);

      --  The number of windows that must be flushed is initially set to 0
      --  (only the current window).

      Buffer (WIN) := SSE.To_Address (0);

      --  Initialize PC with the starting address of the task. Substract 8
      --  to compensate the adjustment made in the context switch routine.

      Buffer (PC) := SSE.To_Address (SSE.To_Integer (Program_Counter) - 8);

      --  The argument to be used by the task wrapper function must be
      --  passed through the o0 register.

      Buffer (O0) := Argument;

      --  The frame pointer is initialized to be the top of the stack

      declare
         FP_In_Stack : System.Address;
         for FP_In_Stack'Address use (Buffer (SP) + FP);

      begin
         --  Mark the deepest stack frame by setting the frame pointer to zero

         FP_In_Stack := SSE.To_Address (0);
      end;

      --  The rest of registers do not need to be initialized

   end Initialize_Context;

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Handler
     (Service_Routine : System.Address;
      Vector          : System.BB.Parameters.Range_Of_Vector)
   is
      Common_Handler : System.Address;
      pragma Import (Asm, Common_Handler, "common_handler");
      --  There is a common part that is executed for every trap. This common
      --  handler executes some prologue, then jumps to the user code, and
      --  after that executes an epilogue.

   begin
      --  Install the 4 instructions in the SPARC trap table to point
      --  to the the shared interrupt service routine (Common_ISR).

      --  mov   Vector, %l3

      Trap_Table (Vector).First_Instr :=
        16#A6102000# + SSE.Integer_Address (Vector);

      --  sethi %hi(Common_Handler), %l4

      --  The 22 most significant bits of Common_Handler are obtained
      --  shifting 10 times to the right, which is equivalent to
      --  divide by 2**10.

      Trap_Table (Vector).Second_Instr :=
        16#29000000# + SSE.To_Integer (Common_Handler'Address) / 2**10;

      --  jmp   %l4 + %lo(Common_Handler)

      --  The 10 least significant bits of Common_Handler are obtained
      --  by masking, which is equivalent to divide by 2**10.

      Trap_Table (Vector).Third_Instr :=
        16#81C52000# + SSE.To_Integer (Common_Handler'Address) mod 2**10;

      --  mov   %psr, %l0

      --  Note that reading the psr at the first instruction of the trap
      --  handler would not be a good idea, because the trap may have happened
      --  just after changing the psr.

      Trap_Table (Vector).Fourth_Instr := 16#A1480000#;

      --  Put the address of the user handler in the
      --  User_Vector_Table, so that the Common_Handler can know where
      --  to jump when a trap (synchronous or asynchronous) occurs.

      User_Vector_Table (Vector) := Service_Routine;
   end Install_Handler;

end System.BB.CPU_Primitives;
