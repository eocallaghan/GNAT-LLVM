------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is a RTX version of this package

--  This package encapsulates all direct interfaces to OS services
--  that are needed by the tasking run-time (libgnarl).

--  PLEASE DO NOT add any with-clauses to this package or remove the pragma
--  Preelaborate. This package is designed to be a bottom-level (leaf) package.

with Ada.Unchecked_Conversion;

with Interfaces.C;
with Interfaces.C.Strings;
with System.Win32;

package System.OS_Interface is
   pragma Preelaborate;

   pragma Linker_Options ("-mthreads");
   --  The mthreads option ensures that the concurrent version of support
   --  libraries is used, with proper protection for concurrent access to
   --  global data (such as mingwthrd).

   subtype int  is Interfaces.C.int;
   subtype long is Interfaces.C.long;

   -------------------
   -- General Types --
   -------------------

   subtype PSZ   is Interfaces.C.Strings.chars_ptr;
   Null_Void : constant Win32.PVOID := System.Null_Address;

   -------------------------
   -- Handles for objects --
   -------------------------

   subtype Thread_Id is Win32.HANDLE;

   -----------
   -- Errno --
   -----------

   NO_ERROR : constant := 0;
   FUNC_ERR : constant := -1;

   ----------------
   -- Interrupts --
   ----------------

   type INTERFACE_TYPE is
     (InterfaceTypeUndefined,
      Internal,
      Isa,
      Eisa,
      MicroChannel,
      TurboChannel,
      PCIBus,
      VMEBus,
      NuBus,
      PCMCIABus,
      CBus,
      MPIBus,
      MPSABus,
      ProcessorInternal,
      InternalPowerBus,
      PNPISABus,
      MaximumInterfaceType);

   for INTERFACE_TYPE use
      (InterfaceTypeUndefined => -1,
       Internal => 0,
       Isa => 1,
       Eisa => 2,
       MicroChannel => 3,
       TurboChannel => 4,
       PCIBus => 5,
       VMEBus => 6,
       NuBus => 7,
       PCMCIABus => 8,
       CBus => 9,
       MPIBus => 10,
       MPSABus => 11,
       ProcessorInternal => 12,
       InternalPowerBus => 13,
       PNPISABus => 14,
       MaximumInterfaceType => 15);

   Max_Interrupt : constant := 255;
   --  The interrupt vector table for the x86 architecture has room for
   --  256 Interrupt, 0 - 255.

   function AttachInterruptVector
     (pThreadAttributes  : access Win32.SECURITY_ATTRIBUTES;
      StackSize          : Win32.DWORD;
      PRoutine           : access procedure (Context : Win32.PVOID);
      Context            : Win32.PVOID;
      Priority           : Win32.DWORD;
      InterfaceType      : INTERFACE_TYPE;
      BusNumber          : Win32.DWORD;
      BusInterruptLevel  : Win32.DWORD;
      BusInterruptVector : Win32.DWORD) return Win32.HANDLE;
   pragma Import (C, AttachInterruptVector, "RtAttachInterruptVector");

   -------------
   -- Signals --
   -------------

   type Signal is new int range 0 .. Max_Interrupt;

   type sigset_t is private;

   -------------
   -- Threads --
   -------------

   type Thread_Body is access
     function (arg : System.Address) return System.Address;
   pragma Convention (C, Thread_Body);

   function Thread_Body_Access is new
     Ada.Unchecked_Conversion (System.Address, Thread_Body);

   -----------------------
   -- Critical sections --
   -----------------------

   type CRITICAL_SECTION is private;

   procedure InitializeCriticalSection
     (pCriticalSection : access CRITICAL_SECTION);
   pragma Import
     (Stdcall, InitializeCriticalSection, "InitializeCriticalSection");

   procedure EnterCriticalSection
     (pCriticalSection : access CRITICAL_SECTION);
   pragma Import (Stdcall, EnterCriticalSection, "EnterCriticalSection");

   procedure LeaveCriticalSection
     (pCriticalSection : access CRITICAL_SECTION);
   pragma Import (Stdcall, LeaveCriticalSection, "LeaveCriticalSection");

   procedure DeleteCriticalSection
     (pCriticalSection : access CRITICAL_SECTION);
   pragma Import (Stdcall, DeleteCriticalSection, "DeleteCriticalSection");

   -------------------------------------------------------------
   -- Thread Creation, Activation, Suspension And Termination --
   -------------------------------------------------------------

   type PTHREAD_START_ROUTINE is access function
     (pThreadParameter : Win32.PVOID) return Win32.DWORD;
   pragma Convention (Stdcall, PTHREAD_START_ROUTINE);

   function To_PTHREAD_START_ROUTINE is new
     Ada.Unchecked_Conversion (System.Address, PTHREAD_START_ROUTINE);

   function CreateThread
     (pThreadAttributes    : access Win32.SECURITY_ATTRIBUTES;
      dwStackSize          : Win32.DWORD;
      pStartAddress        : PTHREAD_START_ROUTINE;
      pParameter           : Win32.PVOID;
      dwCreationFlags      : Win32.DWORD;
      pThreadId            : access Win32.DWORD) return Win32.HANDLE;
   pragma Import (Stdcall, CreateThread, "CreateThread");

   Debug_Process                     : constant := 16#00000001#;
   Debug_Only_This_Process           : constant := 16#00000002#;
   Create_Suspended                  : constant := 16#00000004#;
   Detached_Process                  : constant := 16#00000008#;
   Create_New_Console                : constant := 16#00000010#;

   Create_New_Process_Group          : constant := 16#00000200#;

   Create_No_window                  : constant := 16#08000000#;

   Profile_User                      : constant := 16#10000000#;
   Profile_Kernel                    : constant := 16#20000000#;
   Profile_Server                    : constant := 16#40000000#;

   Stack_Size_Param_Is_A_Reservation : constant := 16#00010000#;

   function GetExitCodeThread
     (hThread   : Win32.HANDLE;
      pExitCode : not null access Win32.DWORD) return Win32.BOOL;
   pragma Import (Stdcall, GetExitCodeThread, "GetExitCodeThread");

   function ResumeThread (hThread : Win32.HANDLE) return Win32.DWORD;
   pragma Import (Stdcall, ResumeThread, "ResumeThread");

   function SuspendThread (hThread : Win32.HANDLE) return Win32.DWORD;
   pragma Import (Stdcall, SuspendThread, "SuspendThread");

   procedure ExitThread (dwExitCode : Win32.DWORD);
   pragma Import (Stdcall, ExitThread, "ExitThread");

   function TerminateThread
     (hThread    : Win32.HANDLE;
      dwExitCode : Win32.DWORD) return Win32.BOOL;
   pragma Import (Stdcall, TerminateThread, "TerminateThread");

   function GetCurrentThread return Win32.HANDLE;
   pragma Import (Stdcall, GetCurrentThread, "GetCurrentThread");

   function GetCurrentProcess return Win32.HANDLE;
   pragma Import (Stdcall, GetCurrentProcess, "GetCurrentProcess");

   function GetCurrentThreadId return Win32.DWORD;
   pragma Import (Stdcall, GetCurrentThreadId, "GetCurrentThreadId");

   function TlsAlloc return Win32.DWORD;
   pragma Import (Stdcall, TlsAlloc, "TlsAlloc");

   function TlsGetValue (dwTlsIndex : Win32.DWORD) return Win32.PVOID;
   pragma Import (Stdcall, TlsGetValue, "TlsGetValue");

   function TlsSetValue
     (dwTlsIndex : Win32.DWORD; pTlsValue : Win32.PVOID) return Win32.BOOL;
   pragma Import (Stdcall, TlsSetValue, "TlsSetValue");

   function TlsFree (dwTlsIndex : Win32.DWORD) return Win32.BOOL;
   pragma Import (Stdcall, TlsFree, "TlsFree");

   TLS_Nothing : constant := Win32.DWORD'Last;

   procedure ExitProcess (uExitCode : Interfaces.C.unsigned);
   pragma Import (Stdcall, ExitProcess, "ExitProcess");

   function WaitForSingleObject
     (hHandle        : Win32.HANDLE;
      dwMilliseconds : Win32.DWORD) return Win32.DWORD;
   pragma Import (Stdcall, WaitForSingleObject, "RtWaitForSingleObject");

   Wait_Infinite : constant := Win32.DWORD'Last;
   WAIT_TIMEOUT  : constant := 16#0000_0102#;
   WAIT_FAILED   : constant := 16#FFFF_FFFF#;

   ------------------------------------
   -- Semaphores, Events and Mutexes --
   ------------------------------------

   function CreateSemaphore
     (pSemaphoreAttributes : access Win32.SECURITY_ATTRIBUTES;
      lInitialCount        : Interfaces.C.long;
      lMaximumCount        : Interfaces.C.long;
      pName                : PSZ) return Win32.HANDLE;
   pragma Import (Stdcall, CreateSemaphore, "RtCreateSemaphoreA");

   function OpenSemaphore
     (dwDesiredAccess : Win32.DWORD;
      bInheritHandle  : Win32.BOOL;
      pName           : PSZ) return Win32.HANDLE;
   pragma Import (Stdcall, OpenSemaphore, "RtOpenSemaphoreA");

   function ReleaseSemaphore
     (hSemaphore     : Win32.HANDLE;
      lReleaseCount  : Interfaces.C.long;
      pPreviousCount : access Win32.LONG) return Win32.BOOL;
   pragma Import (Stdcall, ReleaseSemaphore, "RtReleaseSemaphore");

   function CreateEvent
     (pEventAttributes : access Win32.SECURITY_ATTRIBUTES;
      bManualReset     : Win32.BOOL;
      bInitialState    : Win32.BOOL;
      pName            : PSZ) return Win32.HANDLE;
   pragma Import (Stdcall, CreateEvent, "RtCreateEventA");

   function OpenEvent
     (dwDesiredAccess : Win32.DWORD;
      bInheritHandle  : Win32.BOOL;
      pName           : PSZ) return Win32.HANDLE;
   pragma Import (Stdcall, OpenEvent, "RtOpenEventA");

   function SetEvent (hEvent : Win32.HANDLE) return Win32.BOOL;
   pragma Import (Stdcall, SetEvent, "RtSetEvent");

   function ResetEvent (hEvent : Win32.HANDLE) return Win32.BOOL;
   pragma Import (Stdcall, ResetEvent, "RtResetEvent");

   function PulseEvent (hEvent : Win32.HANDLE) return Win32.BOOL;
   pragma Import (Stdcall, PulseEvent, "RtPulseEvent");

   function CreateMutex
     (pMutexAttributes : access Win32.SECURITY_ATTRIBUTES;
      bInitialOwner    : Win32.BOOL;
      pName            : PSZ) return Win32.HANDLE;
   pragma Import (Stdcall, CreateMutex, "RtCreateMutexA");

   function OpenMutex
     (dwDesiredAccess : Win32.DWORD;
      bInheritHandle  : Win32.BOOL;
      pName           : PSZ) return Win32.HANDLE;
   pragma Import (Stdcall, OpenMutex, "RtOpenMutexA");

   function ReleaseMutex (hMutex : Win32.HANDLE) return Win32.BOOL;
   pragma Import (Stdcall, ReleaseMutex, "RtReleaseMutex");

   ---------------------------------------------------
   -- Accessing properties of Threads and Processes --
   ---------------------------------------------------

   -----------------
   --  Priorities --
   -----------------

   RT_PRIORITY_MAX : constant := 127;
   RT_PRIORITY_MIN : constant := 0;

   function SetThreadPriority
     (hThread   : Win32.HANDLE;
      nPriority : Interfaces.C.int) return Win32.BOOL;
   pragma Import (Stdcall, SetThreadPriority, "RtSetThreadPriority");

   function GetThreadPriority (hThread : Win32.HANDLE) return Interfaces.C.int;
   pragma Import (Stdcall, GetThreadPriority, "RtGetThreadPriority");

private

   type sigset_t is new Signal;

   type CRITICAL_SECTION is record
      DebugInfo      : System.Address;
      --  The following three fields control entering and
      --  exiting the critical section for the resource
      LockCount      : Long_Integer;
      RecursionCount : Long_Integer;
      OwningThread   : Win32.HANDLE;
      LockSemaphore  : Win32.HANDLE;
      Reserved       : Win32.DWORD;
   end record;

end System.OS_Interface;
