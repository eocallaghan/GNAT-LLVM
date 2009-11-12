------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                             S Y S T E M . I N I T                        --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--          Copyright (C) 2003-2006, Free Software Foundation, Inc.         --
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

--  This unit contains initialization circuits that are system dependent

--  This package is for use with configurable runtimes, and replaces init.c

--  There are currently two versions of its body:

--   2sinit.adb is an untargeted, general version that is not currently
--   used in any product.

--   s-init-bb-sparc.adb is the version for bare board SPARC targets

package System.Init is

   procedure Init_Float;
   pragma Import (C, Init_Float, "__gnat_init_float");
   --  Initialize FPP

   procedure Install_Handler;
   pragma Export (C, Install_Handler, "__gnat_install_handler");
   --  Install handler for the synchronous signals. The C profile
   --  here is what is expected by the binder-generated main.

end System.Init;
