------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--                     Copyright (C) 1995-2009, AdaCore                     --
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

--  This is a OpenVMS/Alpha version of this package

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with Interfaces.C; use Interfaces.C;
with System.Machine_Code; use System.Machine_Code;

package body System.OS_Interface is

   ------------------
   -- pthread_self --
   ------------------

   function pthread_self return pthread_t is
      use ASCII;
      Self : pthread_t;

   begin
      Asm ("call_pal 0x9e" & LF & HT &
           "bis $31, $0, %0",
           Outputs  => pthread_t'Asm_Output ("=r", Self),
           Clobber  => "$0",
           Volatile => True);
      return Self;
   end pthread_self;

   -----------------
   -- sched_yield --
   -----------------

   function sched_yield return int is
      procedure sched_yield_base;
      pragma Import (C, sched_yield_base, "PTHREAD_YIELD_NP");

   begin
      sched_yield_base;
      return 0;
   end sched_yield;

end System.OS_Interface;
