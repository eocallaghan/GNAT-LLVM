------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                        S Y S T E M . V X W O R K S                       --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--          Copyright (C) 1998-2007, Free Software Foundation, Inc.         --
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

--  This is the MIPS VxWorks version of this package

with Interfaces.C;

package System.VxWorks is
   pragma Preelaborate;

   package IC renames Interfaces.C;

   --  Floating point context record. MIPS version

   FP_NUM_DREGS : constant := 16;
   type Fpx_Array is array (1 .. FP_NUM_DREGS) of IC.double;

   type FP_CONTEXT is record
      fpx   : Fpx_Array;
      fpcsr : IC.int;
   end record;
   pragma Convention (C, FP_CONTEXT);

   Num_HW_Interrupts : constant := 256;
   --  Number of entries in hardware interrupt vector table

end System.VxWorks;
