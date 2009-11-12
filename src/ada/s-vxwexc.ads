------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--              S Y S T E M . V X W O R K S _ E X C E P T I O N S           --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--           Copyright (C) 2004-2005 Free Software Foundation, Inc.         --
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

--  This package contains the low level initialization routines to deal with
--  the VxWorks specificities regarding the Exception Handling circuitry.

package System.VxWorks_Exceptions is

   procedure Setup_For_EH;
   pragma Export (C, Setup_For_EH, "__gnat_vxw_setup_for_eh");
   --  Perform the necessary low level actions required to support the Ada
   --  level EH circuitry to work, such as registering the exception tables in
   --  the ZCX case. This routine has no effect in a SJLJ runtime.
   --
   --  This is to be called by the early run-time library initialization
   --  routine (__gnat_initialize) on targets where the ZCX scheme is
   --  supported.

end System.VxWorks_Exceptions;
