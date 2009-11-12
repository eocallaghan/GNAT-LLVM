------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              S Y S T E M . T R A C E B A C K _ E N T R I E S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2003-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  This is the Alpha/OpenVMS version of this package

package System.Traceback_Entries is
   pragma Preelaborate;

   --  Symbolization is performed by a VMS service which requires more
   --  than an instruction pointer.

   type Traceback_Entry is record
      PC : System.Address;  --  Program Counter
      PV : System.Address;  --  Procedure Value
   end record;

   pragma Suppress_Initialization (Traceback_Entry);

   Null_TB_Entry : constant Traceback_Entry :=
                     (PC => System.Null_Address,
                      PV => System.Null_Address);

   function PC_For (TB_Entry : Traceback_Entry) return System.Address;
   function PV_For (TB_Entry : Traceback_Entry) return System.Address;

   function TB_Entry_For (PC : System.Address) return Traceback_Entry;

end System.Traceback_Entries;
