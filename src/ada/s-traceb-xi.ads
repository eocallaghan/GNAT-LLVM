------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T R A C E B A C K                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1999-2006, Free Software Foundation, Inc.         --
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

--  This is the bare board/ZFP version of this package

package System.Traceback is

   type Tracebacks_Array is array (Positive range <>) of System.Address;
   --  A traceback array is an array of traceback entries

   procedure Call_Chain
     (Traceback   : out Tracebacks_Array;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1);
   --  Store up to Traceback'Length code locations in Traceback, corresponding
   --  to the current call chain.
   --
   --    Traceback is an array of addresses where the result will be stored.
   --
   --    If the call chain is longer than Traceback'Length, then additional
   --    entries are discarded, and the traceback is missing some of the
   --    highest level entries.
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

   procedure Call_Chain
     (Frame_Pointer : System.Address;
      Traceback     : out Tracebacks_Array;
      Len           : out Natural;
      Exclude_Min   : System.Address := System.Null_Address;
      Exclude_Max   : System.Address := System.Null_Address;
      Skip_Frames   : Natural := 0);
   --  Same as above, but compute a call stack given a frame pointer.
   --  This version is more flexible, as it allows computing call stack of
   --  e.g. other threads.
   --
   --  Under the PPC ABI, Frame_Pointer corresponds typically to r1 (sp) for a
   --  given thread.
   --
   --  Since the frame pointer is already computed, Skip_Frames in general
   --  should not be used, and defaults to 0.

   function C_Call_Chain
     (Frame_Pointer : System.Address;
      Traceback     : System.Address;
      Traceback_Len : Integer) return Integer;
   --  Version that can be called from C code directly
   --  Traceback is the address of an array of void*, e.g. void *traceback[64];
   --  Traceback_Len is the number of elements of Traceback
   --
   --  This function returns the number of elements stored in Traceback
   --  C profile:
   --    int __gnat_call_chain (void *fp, void **traceback, int len);
   pragma Export (C, C_Call_Chain, "__gnat_call_chain");

end System.Traceback;
