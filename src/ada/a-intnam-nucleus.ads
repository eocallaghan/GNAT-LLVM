------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
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

--  This is a Nucleus version of this package

with System.OS_Interface;

package Ada.Interrupts.Names is

   --  Note: mapping of names->signals may be many-to-one. There may be aliases

   SIGABRT  : constant Interrupt_ID :=  --  process abort signal
                System.OS_Interface.SIGABRT;
   SIGFPE   : constant Interrupt_ID :=  --  floating point exception
                System.OS_Interface.SIGFPE;
   SIGILL   : constant Interrupt_ID :=  --  illegal instruction
                System.OS_Interface.SIGILL;
   SIGINT   : constant Interrupt_ID :=  --  terminal interrupt signal
                System.OS_Interface.SIGINT;
   SIGSEGV  : constant Interrupt_ID :=  --  segmentation violation
                System.OS_Interface.SIGSEGV;
   SIGTERM  : constant Interrupt_ID :=  --  termination signal
                System.OS_Interface.SIGTERM;
   SIGALRM  : constant Interrupt_ID :=  --  alarm clock
                System.OS_Interface.SIGALRM;
   SIGHUP   : constant Interrupt_ID :=  --  hangup
                System.OS_Interface.SIGHUP;
   SIGKILL  : constant Interrupt_ID :=  --  kill (cannot be caught or ignored)
                System.OS_Interface.SIGKILL;
   SIGPIPE  : constant Interrupt_ID :=  --  write on a pipe with no readers
                System.OS_Interface.SIGPIPE;
   SIGQUIT  : constant Interrupt_ID :=  --  terminal quit signal
                System.OS_Interface.SIGQUIT;
   SIGUSR1  : constant Interrupt_ID :=  --  user defined signal 1
                System.OS_Interface.SIGUSR1;
   SIGUSR2  : constant Interrupt_ID :=  --  user defined signal 2
                System.OS_Interface.SIGUSR2;
   SIGCHLD  : constant Interrupt_ID :=  --  child process terminated/stopped;
                System.OS_Interface.SIGCHLD;
   SIGCONT  : constant Interrupt_ID :=  --  continue executing, if stopped
                System.OS_Interface.SIGCONT;
   SIGSTOP  : constant Interrupt_ID :=  --  stop executing (can't catch/ignore)
                System.OS_Interface.SIGSTOP;
   SIGTSTP  : constant Interrupt_ID :=  --  terminal stop signal
                System.OS_Interface.SIGTSTP;
   SIGTTIN  : constant Interrupt_ID :=  --  background process attempting read
                System.OS_Interface.SIGTTIN;
   SIGTTOU  : constant Interrupt_ID :=  --  background process attempting write
                System.OS_Interface.SIGTTOU;
   SIGBUS   : constant Interrupt_ID :=  --  bus error
                System.OS_Interface.SIGBUS;
   SIGRTMIN : constant Interrupt_ID :=  -- start of signal numbers for app use
                System.OS_Interface.SIGRTMIN;
   SIGRTMAX : constant Interrupt_ID :=  -- last signal number for app use
                System.OS_Interface.SIGRTMAX;

end Ada.Interrupts.Names;
