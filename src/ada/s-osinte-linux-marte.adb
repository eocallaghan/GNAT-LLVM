------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--          Copyright (C) 1995-2008, Free Software Foundation, Inc.         --
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

--  This is a MaRTE OS version of this package. Based in the original
--  GNU/LinuxThreads, Solaris pthread and HP-UX pthread version of
--  this package.

pragma Style_Checks (All_Checks);
--  Turn off mode in check for procedures since MaRTE packages do not follow
--  -gnatg style check rules yet. ???

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

pragma Warnings (Off);
with MaRTE.HAL;
with MaRTE.Kernel.Mutexes;
with MaRTE.Kernel.Initialization;
with MaRTE.Kernel.Devices_Table;
pragma Warnings (On);

with Interfaces.C; use Interfaces.C;

package body System.OS_Interface is

   package HAL renames MaRTE.HAL;

   -----------------
   -- Local types --
   -----------------

   type sigset_t_ac is access all sigset_t;
   type sigset_t_ac_const is access constant sigset_t;

   type struct_sigaction_ptr is access all struct_sigaction;
   type struct_sigaction_ptr_const is access constant struct_sigaction;

   ----------------------
   -- Type conversions --
   ----------------------

   function To_Descriptor
     (M : access pthread_mutex_t) return MaRTE.POSIX_Pthread.Mutex_Descriptor;
   pragma Inline (To_Descriptor);

   function To_Descriptor
     (CV : access pthread_cond_t)
      return MaRTE.POSIX_Pthread.Condition_Descriptor;
   pragma Inline (To_Descriptor);

   function To_Signal_Set_Ac is new
     Ada.Unchecked_Conversion (sigset_t_ac, PSignal.Signal_Set_Ac);

   function To_Signal_Set_Ac_Const is new
     Ada.Unchecked_Conversion (sigset_t_ac_const, PSignal.Signal_Set_Ac_Const);

   function To_Sigaction_Ac is new Ada.Unchecked_Conversion
     (struct_sigaction_ptr, PSignal.Struct_Sig_Action_Ac);

   function To_Sigaction_Ac_Const is new Ada.Unchecked_Conversion
     (struct_sigaction_ptr_const, PSignal.Struct_Sig_Action_Ac_Const);

   -------------------
   -- To_Descriptor --
   -------------------

   function To_Descriptor
     (M : access pthread_mutex_t) return MaRTE.POSIX_Pthread.Mutex_Descriptor
   is
   begin
      return MaRTE.POSIX_Pthread.Descriptor_Of
        (MaRTE.POSIX_Pthread.Pthread_Mutex_T (M.all));
   end To_Descriptor;

   function To_Descriptor
     (CV : access pthread_cond_t)
      return MaRTE.POSIX_Pthread.Condition_Descriptor
   is
   begin
      return MaRTE.POSIX_Pthread.Descriptor_Of
        (MaRTE.POSIX_Pthread.Pthread_Cond_T (CV.all));
   end To_Descriptor;

   --------------------
   -- Get_Stack_Base --
   --------------------

   function Get_Stack_Base (thread : pthread_t) return Address is
      pragma Warnings (Off, thread);
   begin
      return Null_Address;
   end Get_Stack_Base;

   ------------------
   -- pthread_init --
   ------------------

   --  Initialization for the MaRTE library

   procedure pthread_init is
   begin
      K.Initialization.Initialize;
   end pthread_init;

   -----------
   -- Errno --
   -----------

   function errno return int is
   begin
      return  MaRTE.POSIX_Pthread.Pthread_Self.Error_Code;
   end errno;

   -------------
   -- Signals --
   -------------

   ---------------
   -- sigaddset --
   ---------------

   function sigaddset (set : access sigset_t; sig : Signal) return int is
   begin
      return PSignal.Sigaddset (To_Signal_Set_Ac (sigset_t_ac (set)), sig);
   end sigaddset;

   ---------------
   -- sigdelset --
   ---------------

   function sigdelset (set : access sigset_t; sig : Signal) return int is
   begin
      return PSignal.Sigdelset (To_Signal_Set_Ac (sigset_t_ac (set)), sig);
   end sigdelset;

   ----------------
   -- sigfillset --
   ----------------

   function sigfillset (set : access sigset_t) return int is
   begin
      return PSignal.Sigfillset (To_Signal_Set_Ac (sigset_t_ac (set)));
   end sigfillset;

   -----------------
   -- sigismember --
   -----------------

   function sigismember
     (set : access constant sigset_t;
      sig : Signal) return int
   is
   begin
      return PSignal.Sigismember
        (To_Signal_Set_Ac_Const (sigset_t_ac_const (set)), sig);
   end sigismember;

   -----------------
   -- sigemptyset --
   -----------------

   function sigemptyset (set : access sigset_t) return int is
   begin
      return PSignal.Sigemptyset (To_Signal_Set_Ac (sigset_t_ac (set)));
   end sigemptyset;

   ---------------
   -- sigaction --
   ---------------

   function sigaction
     (sig  : Signal;
      act  : access constant struct_sigaction;
      oact : access struct_sigaction) return int
   is
   begin
      return PSignal.Sigaction
        (sig,
         To_Sigaction_Ac_Const (struct_sigaction_ptr_const (act)),
         To_Sigaction_Ac (struct_sigaction_ptr (oact)));
   end sigaction;

   ----------
   -- Time --
   ----------

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : timespec) return Duration is
   begin
      return TS;
   end To_Duration;

   -----------------
   -- To_Timespec --
   -----------------

   function To_Timespec (D : Duration) return timespec is
   begin
      return D;
   end To_Timespec;

   ------------------
   -- gettimeofday --
   ------------------

   function gettimeofday
     (tv : access struct_timeval;
      tz : System.Address := System.Null_Address) return int
   is
   begin
      pragma Unreferenced (tz);
      tv.all := To_Timeval (MaRTE.POSIX_Time.Get_Time_Gnat);
      return 0;
   end gettimeofday;

   -------------
   -- Process --
   -------------

   --  Not implemented

   function getpid return pid_t is
   begin
      return 0;
   end getpid;

   ---------------------------
   --  POSIX.1c  Section 3  --
   ---------------------------

   ---------------
   --  sigwait  --
   ---------------

   function sigwait
     (set : access constant sigset_t;
      sig : access Signal) return int is
   begin
      return PSignal.Sigwait
        (To_Signal_Set_Ac_Const (sigset_t_ac_const (set)), sig);
   end sigwait;

   -----------------------
   --  pthread_sigmask  --
   -----------------------

   function pthread_sigmask
     (how  : int;
      set  : access constant sigset_t;
      oset : access sigset_t) return int
   is
   begin
      return PSignal.Pthread_Sigmask
        (how,
         To_Signal_Set_Ac_Const (sigset_t_ac_const (set)),
         To_Signal_Set_Ac (sigset_t_ac (oset)));
   end pthread_sigmask;

   ---------
   -- LWP --
   ---------

   function lwp_self return System.Address is
      function To_Address is new
        Ada.Unchecked_Conversion (pthread_t, System.Address);
   begin
      return To_Address (pthread_self);
   end lwp_self;

   --------------------------
   -- POSIX.1c  Section 11 --
   --------------------------

   ------------------------
   -- pthread_mutex_lock --
   ------------------------

   function pthread_mutex_lock (mutex : access pthread_mutex_t) return int is
   begin
      return MaRTE.POSIX_Pthread.Pthread_Mutex_Lock (To_Descriptor (mutex));
   end pthread_mutex_lock;

   --------------------------
   -- pthread_mutex_unlock --
   --------------------------

   function pthread_mutex_unlock (mutex : access pthread_mutex_t) return int is
   begin
      return MaRTE.POSIX_Pthread.Pthread_Mutex_Unlock (To_Descriptor (mutex));
   end pthread_mutex_unlock;

   -----------------------------------------
   -- pthread_mutex_setprioceiling_locked --
   -----------------------------------------

   function pthread_mutex_setprioceiling_locked
     (mutex : access pthread_mutex_t; prioceiling : int) return int
   is
   begin
      return K.Mutexes.Pthread_Mutex_Setprioceiling_Locked
        (To_Descriptor (mutex), prioceiling);
   end pthread_mutex_setprioceiling_locked;

   -----------------------
   -- pthread_cond_wait --
   -----------------------

   function pthread_cond_wait
     (cond  : access pthread_cond_t;
      mutex : access pthread_mutex_t) return int
   is
   begin
      return  MaRTE.POSIX_Pthread.Pthread_Cond_Wait
        (To_Descriptor (cond), To_Descriptor (mutex));
   end pthread_cond_wait;

   ----------------------------
   -- pthread_cond_timedwait --
   ----------------------------

   function pthread_cond_timedwait
     (cond    : access pthread_cond_t;
      mutex   : access pthread_mutex_t;
      abstime : access timespec) return int
   is
   begin
      return MaRTE.POSIX_Pthread.Pthread_Cond_Timedwait_Gnat
        (To_Descriptor (cond),
         To_Descriptor (mutex),
         HAL.Duration_To_HWTime (abstime.all));
   end pthread_cond_timedwait;

   --------------------------
   -- POSIX.1c  Section 13 --
   --------------------------

   ---------------------------
   -- pthread_setschedparam --
   ---------------------------

   function pthread_setschedparam
     (thread : pthread_t;
      policy : int;
      param  : access struct_sched_param) return int
   is
   begin
      --  The GNAT run time only sets the 'sched_priority' param, so the
      --  normal 'POSIX_Pthread.Pthread_Setschedparam' can not be called from
      --  here.

      return MaRTE.POSIX_Pthread.Pthread_Setschedparam_Gnat
        (thread,
         MaRTE.POSIX_Pthread.Scheduling_Policies (policy),
         MaRTE.POSIX_Pthread.Task_Priority (param.sched_priority));
   end pthread_setschedparam;

   ---------------------------
   -- P1003.1c - Section 16 --
   ---------------------------

   --------------------
   -- Pthread_Create --
   --------------------

   function pthread_create
     (thread        : access pthread_t;
      attributes    : access constant pthread_attr_t;
      start_routine : Thread_Body;
      arg           : System.Address) return int
   is
   begin
      return MaRTE.POSIX_Pthread.Pthread_Create
        (thread,
         MaRTE.POSIX_Pthread.Pthread_Attr_T_Ac_Const (attributes),
         MaRTE.POSIX_Pthread.Task_Body_Function (start_routine),
         arg);
   end pthread_create;

   ------------------
   -- pthread_self --
   ------------------

   function pthread_self return pthread_t is
   begin
      return MaRTE.POSIX_Pthread.Pthread_Self;
   end pthread_self;

   --------------------------
   -- POSIX.1c  Section 17 --
   --------------------------

   --  Gnat uses the thread specific data functionality to store the gnat
   --  task id (see 'System.Task_Primitives.Operations.Specific.Self').

   Keys_Used_By_Gnat : Integer := 0;

   function Destructor_To_Address is
     new Ada.Unchecked_Conversion (destructor_pointer, System.Address);

   ------------------------
   -- pthread_key_create --
   ------------------------

   function pthread_key_create
     (key        : access pthread_key_t;
      destructor : destructor_pointer) return int
   is
      Ret  : int;

   begin
      pragma Assert (Keys_Used_By_Gnat = 0,
                     "s-osinte.adb: gnat used more than one key");
      Keys_Used_By_Gnat := Keys_Used_By_Gnat + 1;
      Ret := MaRTE.POSIX_Pthread.Pthread_Key_Create
        (MaRTE.POSIX_Pthread.Pthread_Key_T_Ac (key),
         Destructor_To_Address (destructor));

      --  In the following, Self_Key is used from
      --  System.Task_Primitives.Operations.Abort_Handler'.

      Self_Key := key.all;

      return Ret;
   end pthread_key_create;

end System.OS_Interface;
