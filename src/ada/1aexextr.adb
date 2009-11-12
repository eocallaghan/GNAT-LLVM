------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--       A D A . E X C E P T I O N S . E X C E P T I O N _ T R A C E S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the dummy version of this package when we do not want exception
--  traces to be displayed for unhandled exceptions.

with Unchecked_Conversion;

separate (Ada.Exceptions)
package body Exception_Traces is

   type Exception_Action is access procedure (E : Exception_Occurrence);
   Global_Action : Exception_Action := null;
   pragma Export
     (Ada, Global_Action, "__gnat_exception_actions_global_action");
   --  Global action, executed whenever an exception is raised.  Changing the
   --  export name must be coordinated with code in g-excact.adb.

   Raise_Hook_Initialized : Boolean := False;
   pragma Export
     (Ada, Raise_Hook_Initialized, "__gnat_exception_actions_initialized");

   function To_Action is new Unchecked_Conversion
     (Raise_Action, Exception_Action);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Notify_Exception (Excep : EOA; Is_Unhandled : Boolean);
   --  Factorizes the common processing for Notify_Handled_Exception and
   --  Notify_Unhandled_Exception. Is_Unhandled is set to True only in the
   --  latter case because Notify_Handled_Exception may be called for an
   --  actually unhandled occurrence in the Front-End-SJLJ case.

   ---------------------------------
   -- Debugger Interface Routines --
   ---------------------------------

   --  The routines here are null routines that normally have no effect.
   --  they are provided for the debugger to place breakpoints on their
   --  entry points to get control on an exception.

   procedure Unhandled_Exception;
   pragma Export (C, Unhandled_Exception, "__gnat_unhandled_exception");
   --  Hook for GDB to support "break exception unhandled"

   --  For "break exception", GDB uses __gnat_raise_nodefer_with_msg, which
   --  is not in this section because it fullfills other purposes than a mere
   --  debugger interface.

   --------------------------------
   -- Import Run-Time C Routines --
   --------------------------------

   --  The purpose of the following pragma Import is to ensure that we
   --  generate appropriate subprogram descriptors for all C routines in
   --  the standard GNAT library that can raise exceptions. This ensures
   --  that the exception propagation can properly find these routines

   pragma Propagate_Exceptions;

   procedure Unhandled_Terminate;
   pragma No_Return (Unhandled_Terminate);
   pragma Import (C, Unhandled_Terminate, "__gnat_unhandled_terminate");
   --  Perform system dependent shutdown code

   ----------------------
   -- Notify_Exception --
   ----------------------

   procedure Notify_Exception (Excep : EOA; Is_Unhandled : Boolean) is
      pragma Unreferenced (Is_Unhandled);

   begin
      --  Call the user-specific actions
      --  ??? We should presumably look at the reraise status here.

      if Raise_Hook_Initialized
        and then Exception_Data_Ptr (Excep.Id).Raise_Hook /= null
      then
         To_Action (Exception_Data_Ptr (Excep.Id).Raise_Hook) (Excep.all);
      end if;

      if Global_Action /= null then
         Global_Action (Excep.all);
      end if;
   end Notify_Exception;

   ------------------------------
   -- Notify_Handled_Exception --
   ------------------------------

   procedure Notify_Handled_Exception is
   begin
      Notify_Exception (Get_Current_Excep.all, Is_Unhandled => False);
   end Notify_Handled_Exception;

   --------------------------------
   -- Notify_Unhandled_Exception --
   --------------------------------

   procedure Notify_Unhandled_Exception is
   begin
      Notify_Exception (Get_Current_Excep.all, Is_Unhandled => True);
      Unhandled_Exception;
   end Notify_Unhandled_Exception;

   -------------------------
   -- Unhandled_Exception --
   -------------------------

   procedure Unhandled_Exception is
   begin
      null;
   end Unhandled_Exception;

   -----------------------------------
   -- Unhandled_Exception_Terminate --
   -----------------------------------

   procedure Unhandled_Exception_Terminate is
   begin

      --  Let's shutdown the runtime now. The rest of the procedure
      --  needs to be careful not to use anything that would require
      --  runtime support. In particular, function returing strings
      --  are banned since the sec stack is not functional anymore

      System.Standard_Library.Adafinal;

      Unhandled_Terminate;
   end Unhandled_Exception_Terminate;

   ------------------------------------
   -- Handling GNAT.Exception_Traces --
   ------------------------------------

   --  The bulk of exception traces output is centralized in Notify_Exception,
   --  for both the Handled and Unhandled cases. Extra task specific output is
   --  triggered in the task wrapper for unhandled occurrences in tasks. It is
   --  not performed in this unit to avoid dragging dependencies against the
   --  tasking units here.

   --  We used to rely on the output performed by Unhanded_Exception_Terminate
   --  for the case of an unhandled occurrence in the environment thread, and
   --  the task wrapper was responsible for the whole output in the tasking
   --  case.

   --  This initial scheme had a drawback: the output from Terminate only
   --  occurs after finalization is done, which means possibly never if some
   --  tasks keep hanging around.

   --  The first "presumably obvious" fix consists in moving the Terminate
   --  output before the finalization. It has not been retained because it
   --  introduces annoying changes in output orders when the finalization
   --  itself issues outputs, this also in "regular" cases not resorting to
   --  Exception_Traces.

   --  Today's solution has the advantage of simplicity and better isolates
   --  the Exception_Traces machinery.

   --  It currently outputs the information about unhandled exceptions twice
   --  in the environment thread, once in the notification routine and once in
   --  the termination routine. Avoiding the second output is possible but so
   --  far has been considered undesirable. It would mean changing the order
   --  of outputs between the two runs with or without exception traces, while
   --  it seems preferrable to only have additional outputs in the former case.

end Exception_Traces;
