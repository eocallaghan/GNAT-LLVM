------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T R A C E B A C K                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1999-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a method for generating a traceback of the current
--  execution location. The traceback shows the locations of calls in the call
--  chain, up to either the top or a designated number of levels.

--  This version is for the AE653 Level A run time and for bare board targets

with Ada.Exceptions.Traceback;
package System.Traceback is

   procedure Call_Chain
     (Traceback   : in out Ada.Exceptions.Traceback.Tracebacks_Array;
      Max_Len     : Natural;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1);
   --  Store up to Max_Len code locations in Traceback, corresponding to the
   --  current call chain.
   --
   --    Traceback is  an array of addresses where the result will be stored.
   --
   --    Max_Len is the length of the Traceback array. If the call chain is
   --    longer than this, then additional entries are discarded, and the
   --    traceback is missing some of the highest level entries.
   --
   --    Len is the returned actual number of addresses stored
   --    in the Traceback array.
   --
   --    Exclude_Min/Exclude_Max, if non null, provide a range of addresses
   --    to ignore from the computation of the traceback.
   --
   --    Skip_Frames says how many of the most recent calls should be excluded
   --    from the result, regardless of the exclusion bounds and starting with
   --    this procedure itself: 1 means exclude the frame for this procedure, 2
   --    means 1 + exclude the frame for this procedure's caller, ...
   --
   --  On return, the Traceback array is filled in, and Len indicates the
   --  number of stored entries. The first entry is the most recent call, and
   --  the last entry is the highest level call.

end System.Traceback;
