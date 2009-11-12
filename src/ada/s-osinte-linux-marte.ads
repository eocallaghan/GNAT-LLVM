------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
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
--  GNU/Linux (GNU/LinuxThreads) version of this package.

--  This package encapsulates all direct interfaces to OS services that are
--  needed by the tasking run-time (libgnarl).

pragma Style_Checks (All_Checks);
--  Turn off mode in check for procedures since MaRTE packages do not follow
--  -gnatg style check rules yet. ???

with Ada.Unchecked_Conversion;

with Interfaces.C;

pragma Warnings (Off);
with MaRTE.POSIX_Pthread;
with MaRTE.Kernel.Tasks_Operations.Attributes;
with MaRTE.POSIX_Time;
with MaRTE.POSIX_Signal;
with MaRTE.POSIX_Sched;
with MaRTE.POSIX_Constants;
pragma Warnings (On);

package System.OS_Interface is
   pragma Preelaborate;

   pragma Linker_Options ("-lmarte" & ASCII.NUL & "-lgnat");
   --  MaRTE library depends on the non-tasking part of the GNAT library

   package K       renames MaRTE.Kernel;
   package PC      renames MaRTE.POSIX_Constants;
   package PSignal renames MaRTE.POSIX_Signal;
   --  To avoid confusion with type 'Signal'

   use type MaRTE.POSIX_Pthread.Pthread_T;

   subtype int            is Interfaces.C.int;
   subtype char           is Interfaces.C.char;
   subtype short          is Interfaces.C.short;
   subtype long           is Interfaces.C.long;
   subtype unsigned       is Interfaces.C.unsigned;
   subtype unsigned_short is Interfaces.C.unsigned_short;
   subtype unsigned_long  is Interfaces.C.unsigned_long;
   subtype unsigned_char  is Interfaces.C.unsigned_char;
   subtype plain_char     is Interfaces.C.plain_char;
   subtype size_t         is Interfaces.C.size_t;

   -----------
   -- Errno --
   -----------

   function errno return int;

   EAGAIN    : constant := PC.RESOURCE_TEMPORARILY_UNAVAILABLE;
   EINTR     : constant := PC.INTERRUPTED_OPERATION;
   EINVAL    : constant := PC.INVALID_ARGUMENT;
   ENOMEM    : constant := PC.NOT_ENOUGH_SPACE;
   EPERM     : constant := PC.OPERATION_NOT_PERMITTED;
   ETIMEDOUT : constant := PC.TIMED_OUT;

   -------------
   -- Signals --
   -------------

   Max_Interrupt : constant := PSignal.Signal'Last;
   subtype Signal is PSignal.Signal;

   SIGHUP      : constant := PC.SIGHUP;  --  hangup
   SIGINT      : constant := PC.SIGINT;  --  interrupt (rubout)
   SIGQUIT     : constant := PC.SIGQUIT; --  quit (ASCD FS)
   SIGILL      : constant := PC.SIGILL;  --  illegal instruction (not reset)
   SIGTRAP     : constant := PC.SIGTRAP; --  trace trap (not reset)
   SIGIOT      : constant := PC.SIGNULL; --  IOT instruction (unsupported)
   SIGABRT     : constant := PC.SIGABRT; --  used by abort
   SIGFPE      : constant := PC.SIGFPE;  --  floating point exception
   SIGKILL     : constant := PC.SIGKILL; --  kill (cannot be caught or ignored)
   SIGBUS      : constant := PC.SIGBUS;  --  bus error
   SIGSEGV     : constant := PC.SIGSEGV; --  segmentation violation
   SIGPIPE     : constant := PC.SIGPIPE; --  write on pipe with no one to read
   SIGALRM     : constant := PC.SIGALRM; --  alarm clock
   SIGTERM     : constant := PC.SIGTERM; --  software termination from kill
   SIGUSR1     : constant := PC.SIGUSR1; --  user defined signal 1
   SIGUSR2     : constant := PC.SIGUSR2; --  user defined signal 2
   SIGCLD      : constant := PC.SIGCHLD; --  alias for SIGCHLD
   SIGCHLD     : constant := PC.SIGCHLD; --  child status change
   SIGPWR      : constant := PC.SIGNULL; --  power-fail restart (unsupported)
   SIGWINCH    : constant := PC.SIGNULL; --  window size change (unsupported)
   SIGURG      : constant := PC.SIGNULL; --  urgent cond. on IO (unsupported)
   SIGPOLL     : constant := PC.SIGNULL; --  pollable event (unsupported)
   SIGIO       : constant := PC.SIGNULL; --  I/O now possible (unsupported)
   SIGLOST     : constant := PC.SIGNULL; --  file lock lost (unsupported)
   SIGSTOP     : constant := PC.SIGSTOP; --  stop (cannot be caught or ignored)
   SIGTSTP     : constant := PC.SIGTSTP; --  user stop requested from tty
   SIGCONT     : constant := PC.SIGCONT; --  stopped process has been continued
   SIGTTIN     : constant := PC.SIGTTIN; --  background tty read attempted
   SIGTTOU     : constant := PC.SIGTTOU; --  background tty write attempted
   SIGVTALRM   : constant := PC.SIGVTALRM; --  virtual timer expired
   SIGPROF     : constant := PC.SIGPROF; --  profiling timer expired
   SIGXCPU     : constant := PC.SIGNULL; --  CPU time exceeded (unsupported)
   SIGXFSZ     : constant := PC.SIGNULL; --  filesize exceeded (unsupported)
   SIGUNUSED   : constant := PC.SIGUNUSED; --  unused signal (Linux)
   SIGSTKFLT   : constant := PC.SIGNULL; --  coproc. stack fault (unsupported)

   SIGADAABORT : constant := SIGABRT;
   --  Change this if you want to use another signal for task abort. SIGTERM
   --  might be a good one.

   type Signal_Set is array (Natural range <>) of Signal;

   Unmasked : constant Signal_Set :=
     (SIGTRAP,
      --  To enable debugging on multithreaded applications, mark SIGTRAP to be
      --  kept unmasked.

      SIGBUS,

      SIGTTIN, SIGTTOU, SIGTSTP,
      --  Keep these three signals unmasked so that background processes
      --  and IO behaves as normal "C" applications

      SIGKILL, SIGSTOP);
      --  These two signals actually cannot be masked;
      --  POSIX simply won't allow it.

   Reserved : constant Signal_Set :=
   --  Not clear why the following two signals are reserved.
   --  Perhaps they are not supported by this version of GNU/Linux ???
     (SIGVTALRM, SIGUNUSED);

   type sigset_t is private;

   function sigaddset (set : access sigset_t; sig : Signal) return int;
   pragma Inline (sigaddset);

   function sigdelset (set : access sigset_t; sig : Signal) return int;
   pragma Inline (sigdelset);

   function sigfillset (set : access sigset_t) return int;
   pragma Inline (sigfillset);

   function sigismember
     (set : access constant sigset_t;
      sig : Signal) return int;
   pragma Inline (sigismember);

   function sigemptyset (set : access sigset_t) return int;
   pragma Inline (sigemptyset);

   type struct_sigaction is record
      sa_handler   : System.Address;
      sa_mask      : sigset_t;
      sa_flags     : int;
   end record;
   pragma Convention (C, struct_sigaction);

   type Machine_State is record
      eip : unsigned_long;
      ebx : unsigned_long;
      esp : unsigned_long;
      ebp : unsigned_long;
      esi : unsigned_long;
      edi : unsigned_long;
   end record;
   type Machine_State_Ptr is access all Machine_State;

   SA_SIGINFO : constant int := MaRTE.POSIX_Constants.SA_SIGINFO;
   SA_ONSTACK : constant int := 0;  -- unsupported on MARTE

   SIG_BLOCK   : constant int := int (PSignal.SIG_BLOCK);
   SIG_UNBLOCK : constant int := int (PSignal.SIG_UNBLOCK);
   SIG_SETMASK : constant int := int (PSignal.SIG_SETMASK);

   SIG_DFL : constant int := int (PSignal.SIG_DFL);
   SIG_IGN : constant int := int (PSignal.SIG_IGN);

   function sigaction
     (sig  : Signal;
      act  : access constant struct_sigaction;
      oact : access struct_sigaction) return int;
   pragma Inline (sigaction);

   ----------
   -- Time --
   ----------

   Time_Slice_Supported : constant Boolean := True;
   --  Indicates wether time slicing is supported

   subtype timespec is Duration;

   type clockid_t is private;

   CLOCK_REALTIME : constant clockid_t;

   function clock_gettime
     (clock_id : clockid_t;
      tp       : access timespec) return int;
   pragma Import (C, clock_gettime, "clock_gettime");

   function To_Duration (TS : timespec) return Duration;
   pragma Inline (To_Duration);

   function To_Timespec (D : Duration) return timespec;
   pragma Inline (To_Timespec);

   subtype struct_timeval is MaRTE.POSIX_Time.Timeval;

   function To_Duration (TV : struct_timeval) return Duration
     renames MaRTE.POSIX_Time.To_Duration;

   function To_Timeval (D : Duration) return struct_timeval
     renames MaRTE.POSIX_Time.To_Timeval;

   function gettimeofday
     (tv : access struct_timeval;
      tz : System.Address := System.Null_Address) return int;

   -------------------------
   -- Priority Scheduling --
   -------------------------

   SCHED_FIFO  : constant := MaRTE.POSIX_Constants.SCHED_FIFO;
   SCHED_RR    : constant := MaRTE.POSIX_Constants.SCHED_RR;
   SCHED_OTHER : constant := MaRTE.POSIX_Constants.SCHED_OTHER;

   -------------
   -- Process --
   -------------

   subtype pid_t is Interfaces.C.int;

   function kill (pid : pid_t; sig : Signal) return int renames PSignal.Kill;

   function getpid return pid_t;

   ---------
   -- LWP --
   ---------

   function lwp_self return System.Address;
   pragma Inline (lwp_self);

   -------------
   -- Threads --
   -------------

   type Thread_Body is access
     function (arg : System.Address) return System.Address;

   function Thread_Body_Access is new
     Ada.Unchecked_Conversion (System.Address, Thread_Body);

   subtype pthread_t is MaRTE.POSIX_Pthread.Pthread_T;
   subtype Thread_Id is pthread_t;

   subtype pthread_mutex_t     is MaRTE.POSIX_Pthread.Pthread_Mutex_T;
   subtype pthread_cond_t      is MaRTE.POSIX_Pthread.Pthread_Cond_T;
   subtype pthread_attr_t      is MaRTE.POSIX_Pthread.Pthread_Attr_T;
   subtype pthread_mutexattr_t is MaRTE.POSIX_Pthread.Pthread_Mutexattr_T;
   subtype pthread_condattr_t  is MaRTE.POSIX_Pthread.Pthread_Condattr_T;
   subtype pthread_key_t       is MaRTE.POSIX_Pthread.Pthread_Key_T;

   Self_Key : pthread_key_t;

   PTHREAD_CREATE_DETACHED : constant := PC.PTHREAD_CREATE_DETACHED;

   -----------
   -- Stack --
   -----------

   Alternate_Stack_Size : constant := 0;
   --  No alternate signal stack is used on this platform

   Stack_Base_Available : constant Boolean := False;
   --  Indicates whether the stack base is available on this target

   function Get_Stack_Base (thread : pthread_t) return Address;
   pragma Inline (Get_Stack_Base);
   --  Returns the stack base of the specified thread. Only call this function
   --  when Stack_Base_Available is True.

   function Get_Page_Size return size_t;
   function Get_Page_Size return Address;
   pragma Import (C, Get_Page_Size, "getpagesize");
   --  Returns the size of a page, or 0 if this is not relevant on this target

   PROT_NONE  : constant := 0;
   PROT_READ  : constant := 1;
   PROT_WRITE : constant := 2;
   PROT_EXEC  : constant := 4;
   PROT_ALL   : constant := PROT_READ + PROT_WRITE + PROT_EXEC;
   PROT_ON    : constant := PROT_READ;
   PROT_OFF   : constant := PROT_ALL;

   function mprotect (addr : Address; len : size_t; prot : int) return int;
   pragma Import (C, mprotect);

   ---------------------------------------
   -- Nonstandard Thread Initialization --
   ---------------------------------------

   procedure pthread_init;
   pragma Inline (pthread_init);

   -------------------------
   -- POSIX.1c  Section 3 --
   -------------------------

   function sigwait
     (set : access constant sigset_t;
      sig : access Signal) return int;

   function pthread_kill (thread : pthread_t; sig : Signal) return int
     renames PSignal.Pthread_Kill;

   type sigset_t_ptr is access all sigset_t;

   function pthread_sigmask
     (how  : int;
      set  : access constant sigset_t;
      oset : access sigset_t) return int;

   --------------------------
   -- POSIX.1c  Section 11 --
   --------------------------

   function pthread_mutexattr_init
     (attr : access pthread_mutexattr_t) return int
     renames MaRTE.POSIX_Pthread.Pthread_Mutexattr_Init;

   function pthread_mutexattr_destroy
     (attr : access pthread_mutexattr_t) return int
     renames MaRTE.POSIX_Pthread.Pthread_Mutexattr_Destroy;

   function pthread_mutex_init
     (mutex : access pthread_mutex_t;
      attr  : access pthread_mutexattr_t) return int
     renames MaRTE.POSIX_Pthread.Pthread_Mutex_Init;

   function pthread_mutex_destroy (mutex : access pthread_mutex_t) return int
     renames MaRTE.POSIX_Pthread.Pthread_Mutex_Destroy;

   function pthread_mutex_lock (mutex : access pthread_mutex_t) return int;

   function pthread_mutex_unlock (mutex : access pthread_mutex_t) return int;

   function pthread_condattr_init
     (attr : access pthread_condattr_t) return int
     renames MaRTE.POSIX_Pthread.Pthread_Condattr_Init;

   function pthread_condattr_destroy
     (attr : access pthread_condattr_t) return int
     renames MaRTE.POSIX_Pthread.Pthread_Condattr_Destroy;

   function pthread_cond_init
     (cond : access pthread_cond_t;
      attr : access pthread_condattr_t) return int
     renames MaRTE.POSIX_Pthread.Pthread_Cond_Init;

   function pthread_cond_destroy (cond : access pthread_cond_t) return int
     renames MaRTE.POSIX_Pthread.Pthread_Cond_Destroy;

   function pthread_cond_signal (cond : access pthread_cond_t) return int
     renames MaRTE.POSIX_Pthread.Pthread_Cond_Signal;

   function pthread_cond_wait
     (cond  : access pthread_cond_t;
      mutex : access pthread_mutex_t) return int;

   function pthread_cond_timedwait
     (cond    : access pthread_cond_t;
      mutex   : access pthread_mutex_t;
      abstime : access timespec) return int;

   Relative_Timed_Wait : constant Boolean := False;
   --  pthread_cond_timedwait requires an absolute delay time

   --------------------------
   -- POSIX.1c  Section 13 --
   --------------------------

   PTHREAD_PRIO_NONE    : constant := PC.NO_PRIORITY_INHERITANCE;
   PTHREAD_PRIO_INHERIT : constant := PC.HIGHEST_BLOCKED_TASK;
   PTHREAD_PRIO_PROTECT : constant := PC.HIGHEST_CEILING_PRIORITY;

   function pthread_mutexattr_setprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : int) return int
     renames MaRTE.POSIX_Pthread.Pthread_Mutexattr_Setprotocol;

   function pthread_mutexattr_setprioceiling
     (attr        : access pthread_mutexattr_t;
      prioceiling : int) return int
     renames MaRTE.POSIX_Pthread.Pthread_Mutexattr_Setprioceiling;

   type struct_sched_param is record
      sched_priority : int;  --  scheduling priority
   end record;
   pragma Convention (C, struct_sched_param);

   function pthread_setschedparam
     (thread : pthread_t;
      policy : int;
      param  : access struct_sched_param) return int;

   function pthread_setschedprio
     (thread : pthread_t;
      prio : int) return int
     renames K.Tasks_Operations.Pthread_Setschedprio;

   function pthread_attr_setscope
     (attr            : access pthread_attr_t;
      contentionscope : int) return int
     renames MaRTE.POSIX_Pthread.Pthread_Attr_Setscope;

   function pthread_attr_setschedpolicy
     (attr   : access pthread_attr_t;
      policy : int) return int
     renames K.Tasks_Operations.Attributes.Pthread_Attr_Setschedpolicy;

   function sched_yield return int
     renames MaRTE.POSIX_Sched.Sched_Yield;

   ----------------------------------
   -- Nonstandard dynamic ceilings --
   ----------------------------------

   function pthread_mutex_setprioceiling_locked
     (mutex       : access pthread_mutex_t;
      prioceiling : int) return int;

   ---------------------------
   -- P1003.1c - Section 16 --
   ---------------------------

   function pthread_attr_init
     (attributes : access pthread_attr_t) return int
     renames MaRTE.POSIX_Pthread.Pthread_Attr_Init;

   function pthread_attr_destroy
     (attributes : access pthread_attr_t) return int
     renames MaRTE.POSIX_Pthread.Pthread_Attr_Destroy;

   function pthread_attr_setdetachstate
     (attr        : access pthread_attr_t;
      detachstate : int) return int
     renames K.Tasks_Operations.Attributes.Pthread_Attr_Setdetachstate;

   function pthread_attr_setstacksize
     (attr      : access pthread_attr_t;
      stacksize : size_t) return int
     renames K.Tasks_Operations.Attributes.Pthread_Attr_Setstacksize;

   function pthread_create
     (thread        : access pthread_t;
      attributes    : access constant pthread_attr_t;
      start_routine : Thread_Body;
      arg           : System.Address) return int;

   procedure pthread_exit (status : System.Address)
     renames MaRTE.POSIX_Pthread.Pthread_Exit;

   function pthread_self return pthread_t;

   --------------------------
   -- POSIX.1c  Section 17 --
   --------------------------

   function pthread_setspecific
     (key   : pthread_key_t;
      value : System.Address) return int
     renames MaRTE.POSIX_Pthread.Pthread_Setspecific;

   function pthread_getspecific (key : pthread_key_t) return System.Address
     renames MaRTE.POSIX_Pthread.Pthread_Getspecific;

   type destructor_pointer is access procedure (arg : System.Address);

   function pthread_key_create
     (key        : access pthread_key_t;
      destructor : destructor_pointer) return int;

private
   type sigset_t is new PSignal.Signal_Set;

   type clockid_t is new int;
   CLOCK_REALTIME : constant clockid_t := 0;

end System.OS_Interface;
