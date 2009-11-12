------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     A D A . F I N A L I Z A T I O N                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2008, Free Software Foundation, Inc.         --
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

pragma Warnings (Off);
--  System.Finalization_Root does not have category Remote_Types, but we
--  allow it anyway.
with System.Finalization_Root;
pragma Warnings (On);

package Ada.Finalization is
   pragma Preelaborate;
   pragma Remote_Types;

   type Controlled is abstract tagged private;
   pragma Preelaborable_Initialization (Controlled);

   procedure Initialize (Object : in out Controlled);
   procedure Adjust     (Object : in out Controlled);
   procedure Finalize   (Object : in out Controlled);

   type Limited_Controlled is abstract tagged limited private;
   pragma Preelaborable_Initialization (Limited_Controlled);

   procedure Initialize (Object : in out Limited_Controlled);
   procedure Finalize   (Object : in out Limited_Controlled);

private
   package SFR renames System.Finalization_Root;

   type Controlled is abstract new SFR.Root_Controlled with null record;

   overriding function "=" (A, B : Controlled) return Boolean;
   --  Need to be defined explicitly because we don't want to compare the
   --  hidden pointers.

   type Limited_Controlled is
     abstract new SFR.Root_Controlled with null record;

end Ada.Finalization;
