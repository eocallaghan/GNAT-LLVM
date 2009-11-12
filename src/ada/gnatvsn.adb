------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G N A T V S N                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
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

package body Gnatvsn is

   ----------------------
   -- Copyright_Holder --
   ----------------------

   function Copyright_Holder return String is
   begin
      return "Free Software Foundation, Inc.";
   end Copyright_Holder;

   ------------------------
   -- Gnat_Free_Software --
   ------------------------

   function Gnat_Free_Software return String is
   begin
      case Build_Type is
         when GPL | FSF =>
            return
              "This is free software; see the source for copying conditions." &
              ASCII.LF &
              "There is NO warranty; not even for MERCHANTABILITY or FITNESS" &
              " FOR A PARTICULAR PURPOSE.";

         when Gnatpro =>
            return
              "This is free software; see the source for copying conditions." &
               ASCII.LF &
               "See your AdaCore support agreement for details of warranty" &
               " and support." &
               ASCII.LF &
               "If you do not have a current support agreement, then there" &
               " is absolutely" &
               ASCII.LF &
               "no warranty; not even for MERCHANTABILITY or FITNESS FOR" &
               " A PARTICULAR" &
               ASCII.LF &
               "PURPOSE.";
      end case;
   end Gnat_Free_Software;

   -------------------------
   -- Gnat_Version_String --
   -------------------------

   function Gnat_Version_String return String is
   begin
      case Build_Type is
         when Gnatpro =>
            return "Pro " & Gnat_Static_Version_String;
         when GPL =>
            return "GPL " & Gnat_Static_Version_String;
         when FSF =>
            return Gnat_Static_Version_String;
      end case;
   end Gnat_Version_String;

end Gnatvsn;
