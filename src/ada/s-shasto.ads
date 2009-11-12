------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . S H A R E D _ S T O R A G E                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1998-2008, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package manages the shared/persistant storage required for
--  full implementation of variables in Shared_Passive packages, more
--  precisely variables whose enclosing dynamic scope is a shared
--  passive package. This implementation is specific to GNAT and GLADE
--  provides a more general implementation not dedicated to file
--  storage.

--  This unit (and shared passive partitions) are supported on all
--  GNAT implementations except on OpenVMS (where problems arise from
--  trying to share files, and with version numbers of files)

--  --------------------------
--  -- Shared Storage Model --
--  --------------------------

--  The basic model used is that each partition that references the
--  Shared_Passive package has a local copy of the package data that
--  is initialized in accordance with the declarations of the package
--  in the normal manner. The routines in System.Shared_Storage are
--  then used to ensure that the values in these separate copies are
--  properly synchronized with the state of the overall system.

--  In the GNAT implementation, this synchronization is ensured by
--  maintaining a set of files, in a designated directory. The
--  directory is designated by setting the environment variable
--  SHARED_MEMORY_DIRECTORY. This variable must be set for all
--  partitions. If the environment variable is not defined, then the
--  current directory is used.

--  There is one storage for each variable. The name is the fully
--  qualified name of the variable with all letters forced to lower
--  case. For example, the variable Var in the shared passive package
--  Pkg results in the storage name pkg.var.

--  If the storage does not exist, it indicates that no partition has
--  assigned a new value, so that the initial value is the correct
--  one. This is the critical component of the model. It means that
--  there is no system-wide synchronization required for initializing
--  the package, since the shared storages need not (and do not)
--  reflect the initial state. There is therefore no issue of
--  synchronizing initialization and read/write access.

--  -----------------------
--  -- Read/Write Access --
--  -----------------------

--  The approach is as follows:

--    For each shared variable, var, an instanciation of the below generic
--    package is created which provides Read and Write supporting procedures.

--    The routine Read in package System.Shared_Storage.Shared_Var_Procs
--    ensure to assign variable V to the last written value among processes
--    referencing it. A call to this procedure is generated by the expander
--    before each read access to the shared variable.

--    The routine Write in package System.Shared_Storage.Shared_Var_Proc
--    set a new value to the shared variable and, according to the used
--    implementation, propagate this value among processes referencing it.
--    A call to this procedure is generated by the expander after each
--    assignment of the shared variable.

--  Note: a special circuit allows the use of stream attributes Read and
--  Write for limited types (using the corresponding attribute for the
--  full type), but there are limitations on the data that can be placed
--  in shared passive partitions. See sem_smem.ads/adb for details.

--  ----------------------------------------------------------------
--  -- Handling of Protected Objects in Shared Passive Partitions --
--  ----------------------------------------------------------------

--  In the context of GNAT, during the execution of a protected
--  subprogram call, access is locked out using a locking mechanism
--  per protected object, as provided by the GNAT.Lock_Files
--  capability in the specific case of GNAT. This package contains the
--  lock and unlock calls, and the expander generates a call to the
--  lock routine before the protected call and a call to the unlock
--  routine after the protected call.

--  Within the code of the protected subprogram, the access to the
--  protected object itself uses the local copy, without any special
--  synchronization.  Since global access is locked out, no other task
--  or partition can attempt to read or write this data as long as the
--  lock is held.

--  The data in the local copy does however need synchronizing with
--  the global values in the shared storage. This is achieved as
--  follows:

--    The protected object generates a read and assignment routine as
--    described for other shared passive variables. The code for the
--    'Read and 'Write attributes (not normally allowed, but allowed
--    in this special case) simply reads or writes the values of the
--    components in the protected record.

--    The lock call is followed by a call to the shared read routine to
--    synchronize the local copy to contain the proper global value.

--    The unlock call in the procedure case only is preceded by a call
--    to the shared assign routine to synchronize the global shared
--    storages with the (possibly modified) local copy.

--    These calls to the read and assign routines, as well as the lock
--    and unlock routines, are inserted by the expander (see exp_smem.adb).

package System.Shared_Storage is

   procedure Shared_Var_Lock (Var : String);
   --  This procedure claims the shared storage lock. It is used for
   --  protected types in shared passive packages. A call to this
   --  locking routine is generated as the first operation in the code
   --  for the body of a protected subprogram, and it busy waits if
   --  the lock is busy.

   procedure Shared_Var_Unlock (Var : String);
   --  This procedure releases the shared storage lock obtaind by a
   --  prior call to the Shared_Var_Lock procedure, and is to be
   --  generated as the last operation in the body of a protected
   --  subprogram.

   --  This generic package is instantiated for each shared passive
   --  variable. It provides supporting procedures called upon each
   --  read or write access by the expanded code.

   generic

      type Typ is limited private;
      --  Shared passive variable type

      V : in out Typ;
      --  Shared passive variable

      Full_Name : String;
      --  Shared passive variable storage name

   package Shared_Var_Procs is

      procedure Read;
      --  Shared passive variable access routine. Each reference to the
      --  shared variable, V, is preceded by a call to the corresponding
      --  Read procedure, which either leaves the initial value unchanged
      --  if the storage does not exist, or reads the current value from
      --  the shared storage.

      procedure Write;
      --  Shared passive variable assignment routine. Each assignment to
      --  the shared variable, V, is followed by a call to the corresponding
      --  Write procedure, which writes the new value to the shared storage.

   end Shared_Var_Procs;

end System.Shared_Storage;
