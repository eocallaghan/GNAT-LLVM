------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                    S Y S T E M . O S _ I N T E R F A C E                 --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is the Nucleus version of this package

--  This package encapsulates all direct interfaces to OS services
--  that are needed by the tasking run-time (libgnarl).

--  PLEASE DO NOT add any with-clauses to this package or remove the pragma
--  Preelaborate. This package is designed to be a bottom-level (leaf) package.

with Interfaces.C;

package System.OS_Interface is
   pragma Preelaborate;

   subtype int        is Interfaces.C.int;
   subtype short      is Short_Integer;
   type unsigned_int  is mod 2 ** int'Size;
   subtype long       is Long_Integer;
   type unsigned_long is mod 2 ** long'Size;
   subtype size_t     is Interfaces.C.size_t;

   type int_access is access all int;
   type unsigned_long_array is array (Natural range <>) of unsigned_long;

   type pid_t is new int;              -- processes not actually supported
   type pthread_t is new int;          -- thread id

   -----------------------------------
   --  Types and constant for GNARL --
   -----------------------------------

   Time_Slice_Supported : constant Boolean := True;  --  supports round robin

   subtype Thread_Id is pthread_t;     -- for GNARL
   subtype Thread_Body is System.Address;

   subtype To_Target_Priority is int;
   --  On Nucleus, priority mapping is one-to-one, so To_Target_Priority can
   --  be a type conversion instead of a function

   procedure pthread_init;
   --  dummy, needed on some systems

   getpid : constant pid_t := -1; -- dummy, no processes on Nucleus

   -----------
   -- Stack --
   -----------

   type stack_t is record
      ss_sp    : System.Address;
      ss_size  : size_t;
      ss_flags : int;
   end record;
   pragma Convention (C, stack_t);

   function sigaltstack
     (ss  : not null access stack_t;
      oss : access stack_t) return int;
   --  This is a dummy procedure

   Alternate_Stack : aliased System.Address;
   --  This is a dummy definition, never used (Alternate_Stack_Size is null)

   Alternate_Stack_Size : constant := 0;
   --  No alternate signal stack is used on this platform

   Stack_Base_Available : constant Boolean := False;
   --  Disable guard page

   --  The following are all dummies for guard page operations, which are
   --  not supported on Nucleus as indicated in Stack_Base_Available.

   Get_Page_Size : constant := 0;
   PROT_ON       : constant := 0;
   PROT_OFF      : constant := 0;

   subtype Get_Stack_Base is System.Address;

   function mprotect
     (addr : System.Address;
      len  : size_t;
      prot : int) return int;

   -----------
   -- Errno --
   -----------

   function errno return int_access;
   pragma Import (C, errno, "_errno");

   EINTR     : constant := 4;
   EAGAIN    : constant := 11;
   ENOMEM    : constant := 12;
   EINVAL    : constant := 22;
   ETIMEDOUT : constant := 60;
   ENOTSUP   : constant := 82;

   ----------------------------
   -- Signals and Interrupts --
   ----------------------------

   --  Number of signals on the target OS

   SIGABRT  : constant :=  1; --  process abort signal
   SIGFPE   : constant :=  2; --  floating point exception
   SIGILL   : constant :=  3; --  illegal instruction
   SIGINT   : constant :=  4; --  terminal interrupt signal
   SIGSEGV  : constant :=  5; --  segmentation violation
   SIGTERM  : constant :=  6; --  termination signal
   SIGALRM  : constant :=  7; --  alarm clock
   SIGHUP   : constant :=  8; --  hangup
   SIGKILL  : constant :=  9; --  kill (cannot be caught or ignored)
   SIGPIPE  : constant := 10; --  write on a pipe with no one to read it
   SIGQUIT  : constant := 11; --  terminal quit signal
   SIGUSR1  : constant := 12; --  user defined signal 1
   SIGUSR2  : constant := 13; --  user defined signal 2
   SIGCHLD  : constant := 14; --  child process terminated or stopped;
   SIGCONT  : constant := 15; --  continue executing, if stopped
   SIGSTOP  : constant := 16; --  stop executing (cannot be caught or ignored)
   SIGTSTP  : constant := 17; --  terminal stop signal
   SIGTTIN  : constant := 18; --  background process attempting read
   SIGTTOU  : constant := 19; --  background process attempting write
   SIGBUS   : constant := 20; --  bus error
   SIGRTMIN : constant := SIGBUS + 1; -- start of signal numbers for app use
   SIGRTMAX : constant := 32; --  last signal number reserved for app use

   Max_Interrupt : constant := SIGRTMAX;
   SIGADAABORT   : constant := SIGABRT;

   subtype Signal is int range 0 .. Max_Interrupt;
   type Signal_Set is array (Natural range <>) of Signal;

   Unmasked    : constant Signal_Set := (SIGTTIN, SIGTTOU, SIGTSTP);
   Reserved    : constant Signal_Set := (SIGABRT, SIGKILL, SIGSTOP);

   -----------------------------------
   -- Signal processing definitions --
   -----------------------------------

   --  The how in sigprocmask()

   SIG_BLOCK   : constant := 0;
   SIG_UNBLOCK : constant := 1;
   SIG_SETMASK : constant := 2;

   --  The sa_flags in struct sigaction

   SA_SIGINFO : constant := 1; --  causes extra info to be passed to handlers
   SA_ONSTACK : constant := 16#0004#;

   SIG_IGN : constant := 0;
   SIG_DFL : constant := 1;
   SIG_ERR : constant := -1;

   type sigset_t is new long;

   type struct_sigaction is record
      sa_handler   : System.Address;
      sa_mask      : sigset_t;
      sa_flags     : int;
      sa_sigcction : System.Address;
   end record;
   pragma Convention (C, struct_sigaction);
   type struct_sigaction_ptr is access all struct_sigaction;

   function pthread_sigmask
     (how  : int;
      set  : access sigset_t;
      oset : access sigset_t) return int;
   pragma Import (C, pthread_sigmask, "pthread_sigmask");

   function C_raise (sig : int) return int;
   pragma Import (C, C_raise, "raise");

   function sigaction
     (sig  : int;
      act  : struct_sigaction_ptr;
      oact : struct_sigaction_ptr) return int;
   pragma Import (C, sigaction, "sigaction");

   function sigaddset (set : access sigset_t; signo : int) return int;
   pragma Import (C, sigaddset, "sigaddset");

   function sigdelset (set : access sigset_t; signo : int) return int;
   pragma Import (C, sigdelset, "sigdelset");

   function sigemptyset (set : access sigset_t) return int;
   pragma Import (C, sigemptyset, "sigemptyset");

   function sigfillset (set : access sigset_t) return int;
   pragma Import (C, sigfillset, "sigfillset");

   function sigismember (set : access sigset_t; signo : int) return int;
   pragma Import (C, sigismember, "sigismember");

   function sigpending (set : access sigset_t) return int;
   pragma Import (C, sigpending, "sigpending");

   function sigprocmask
    (how  : int;
     set  : access sigset_t;
     oset : sigset_t) return int;
   pragma Import (C, sigprocmask, "sigprocmask");

   function sigsuspend (sigmask : access sigset_t) return int;
   pragma Import (C, sigsuspend, "sigsuspend");

   function sigwait (set : access sigset_t; sig : access Signal) return int;
   pragma Import (C, sigwait, "sigwait");
   --  ??? can't use type int for sig, because of caller in s-intmaop-posix

   function kill (pid : pid_t; sig : int) return int;
   pragma Import (C, kill);
   --  This will always return POSIX_ERROR and set errno to ENOTSUP,
   --  since there is no process support in Nucleus

   function pthread_kill (thread : pthread_t; sig : int) return int;
   pragma Import (C, pthread_kill);

   ----------
   -- Time --
   ----------

   type time_t is new unsigned_long;

   type timespec is record
      ts_sec  : time_t;
      ts_nsec : long;
   end record;

   type clockid_t is new int;

   CLOCK_REALTIME  : constant clockid_t := 0; --  System wide realtime clock
   CLOCK_MONOTONIC : constant clockid_t := 1; --  System wide monotonic clock

   function clock_gettime
     (clock_id : clockid_t; tp : access timespec) return int;
   pragma Import (C, clock_gettime, "clock_gettime");

   function clock_getres
     (clock_id : clockid_t; res : access timespec) return int;
   pragma Import (C, clock_getres);

   function nanosleep (rqtp, rmtp : access timespec)  return int;
   pragma Import (C, nanosleep, "nanosleep");

   function To_Duration (TS : timespec) return Duration;
   pragma Inline (To_Duration);

   function To_Timespec (D : Duration) return timespec;
   pragma Inline (To_Timespec);

   function To_Clock_Ticks (D : Duration) return int;
   --  Convert a duration value (in seconds) into clock ticks

   --------------------------
   -- Nucleus specific API --
   --------------------------

   POSIX_ERROR : constant int := int (-1);

   ----------------------------
   --  POSIX.1c  Section 11  --
   ----------------------------

   PTHREAD_PRIO_NONE    : constant := 0;
   PTHREAD_PRIO_INHERIT : constant := 1;
   PTHREAD_PRIO_PROTECT : constant := 2;

   type pthread_mutex_t is record
      nu_scb         : unsigned_long_array (0 .. 10); -- sema control block
      owner_task_p   : System.Address; -- Mutex owner task pointer
      option         : unsigned_int;   -- PTHREAD_PRIO_xxx
      prioceiling    : int;            -- Ceiling value if PTHREAD_PRIO_INHERIT
      owner_priority : int;            -- Mutex owner task priority
   end record;

   type pthread_mutexattr_t is record
      is_initialized : int; -- True iff mutex attributes are initialized
      protocol       : int; -- PTHREAD_PRIO_xxx
      prioceiling    : int; -- Value of the priority ceiling
      pshared        : int; -- True iff mutex can be shared accross processes
   end record;

   type pthread_cond_t is record
      nu_cond        : unsigned_long_array (0 .. 7); -- cond control bloxk
   end record;

   type pthread_condattr_t is record
      is_initialized : int; -- True iff mutex attributes are initialized
      pshared        : int; -- True iff mutex can be shared accross processes
   end record;

   function pthread_mutexattr_init
     (attr : access pthread_mutexattr_t) return int;
   pragma Import (C, pthread_mutexattr_init);

   function pthread_mutexattr_destroy
     (attr : access pthread_mutexattr_t) return int;
   pragma Import (C, pthread_mutexattr_destroy);

   function pthread_mutex_init
     (mutex : access pthread_mutex_t;
      attr  : access pthread_mutexattr_t) return int;
   pragma Import (C, pthread_mutex_init);

   function pthread_mutex_destroy (mutex : access pthread_mutex_t) return int;
   pragma Import (C, pthread_mutex_destroy);

   function pthread_mutex_lock (mutex : access pthread_mutex_t) return int;
   pragma Import (C, pthread_mutex_lock);

   function pthread_mutex_unlock (mutex : access pthread_mutex_t) return int;
   pragma Import (C, pthread_mutex_unlock);

   function pthread_condattr_init
     (attr : access pthread_condattr_t) return int;
   pragma Import (C, pthread_condattr_init);

   function pthread_condattr_destroy
     (attr : access pthread_condattr_t) return int;
   pragma Import (C, pthread_condattr_destroy);

   function pthread_cond_init
     (cond : access pthread_cond_t;
      attr : access pthread_condattr_t) return int;
   pragma Import (C, pthread_cond_init);

   function pthread_cond_destroy (cond : access pthread_cond_t) return int;
   pragma Import (C, pthread_cond_destroy);

   function pthread_cond_signal (cond : access pthread_cond_t) return int;
   pragma Import (C, pthread_cond_signal);

   function pthread_cond_wait
     (cond  : access pthread_cond_t;
      mutex : access pthread_mutex_t) return int;
   pragma Import (C, pthread_cond_wait);

   function pthread_cond_timedwait
     (cond    : access pthread_cond_t;
      mutex   : access pthread_mutex_t;
      abstime : access timespec) return int;
   pragma Import (C, pthread_cond_timedwait);

   Relative_Timed_Wait : constant Boolean := False;
   --  pthread_cond_timedwait requires an absolute delay time

   --------------------------
   -- POSIX.1c  Section 13 --
   --------------------------

   SCHED_FIFO  : constant := 0;
   SCHED_RR    : constant := 1;
   SCHED_OTHER : constant := SCHED_FIFO; -- not supported on Nucleus

   PTHREAD_SCOPE_SYSTEM  : constant := 0;
   PTHREAD_SCOPE_PROCESS : constant := 1;

   PTHREAD_CREATE_JOINABLE : constant := 0;
   PTHREAD_CREATE_DETACHED : constant := 1;

   type struct_sched_param is record
      sched_priority : int;            -- process execution scheduling priority
   end record;

   type pthread_attr_t is record
      kernel_stack_addr : System.Address; -- kernel mode stack address
      rrinterval        : long;        -- round robin interval in nanoseconds
      kernel_stack_size : size_t;      -- kernel mode stack size
      is_initialized    : int;         -- 0 to destroy the attributes
      inheritsched      : int;         -- inherited scheduler parameter
      schedpolicy       : int;         -- SCHED_xxx
      priority          : int;
      detach_state      : int;
      contention_scope  : int;
   end record;

   function pthread_mutexattr_setprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : int) return int;
   pragma Import (C, pthread_mutexattr_setprotocol);

   function pthread_mutexattr_setprioceiling
     (attr        : access pthread_mutexattr_t;
      prioceiling : int) return int;
   pragma Import (C, pthread_mutexattr_setprioceiling);

   function pthread_setschedparam
     (thread : pthread_t;
      policy : int;
      param  : access struct_sched_param) return int;
   pragma Import (C, pthread_setschedparam);

   function pthread_attr_setscope
     (attr            : access pthread_attr_t;
      contentionscope : int) return int;
   pragma Import (C, pthread_attr_setscope);

   function pthread_attr_setinheritsched
     (attr         : access pthread_attr_t;
      inheritsched : int) return int;
   pragma Import (C, pthread_attr_setinheritsched);

   function pthread_attr_setschedpolicy
     (attr   : access pthread_attr_t;
      policy : int) return int;
   pragma Import (C, pthread_attr_setschedpolicy);

   function sched_yield return int;
   pragma Import (C, sched_yield);

   --------------------------
   -- P1003.1c  Section 16 --
   --------------------------

   function pthread_attr_init (attributes : access pthread_attr_t) return int;
   pragma Import (C, pthread_attr_init);

   function pthread_attr_destroy
     (attributes : access pthread_attr_t) return int;
   pragma Import (C, pthread_attr_destroy);

   function pthread_attr_setdetachstate
     (attr        : access pthread_attr_t;
      detachstate : int) return int;
   pragma Import (C, pthread_attr_setdetachstate);

   function pthread_attr_setstacksize
     (attr      : access pthread_attr_t;
      stacksize : size_t) return int;
   pragma Import (C, pthread_attr_setstacksize);

   function pthread_create
     (thread        : access pthread_t;
      attributes    : access pthread_attr_t;
      start_routine : System.Address;
      arg           : System.Address) return int;
   pragma Import (C, pthread_create);

   procedure pthread_exit (status : System.Address);
   pragma Import (C, pthread_exit);

   function pthread_self return pthread_t;
   pragma Import (C, pthread_self);

   function lwp_self return System.Address;
   --  lwp_self does not exist on this thread library, revert to pthread_self
   --  which is the closest approximation (with getpid). This function is
   --  needed to share 7staprop.adb across POSIX-like targets.
   pragma Import (C, lwp_self, "pthread_self");

   --------------------------
   -- POSIX.1c  Section 17 --
   --------------------------

   type pthread_key_t is new int;

   function pthread_setspecific
     (key   : pthread_key_t;
      value : System.Address) return int;
   pragma Import (C, pthread_setspecific);

   function pthread_getspecific
     (key   : pthread_key_t) return System.Address;
   pragma Import (C, pthread_getspecific);

   type destructor_pointer is access procedure (arg : System.Address);
   pragma Convention (C, destructor_pointer);

   function pthread_key_create
     (key        : access pthread_key_t;
      destructor : destructor_pointer) return int;
   pragma Import (C, pthread_key_create);

end System.OS_Interface;
