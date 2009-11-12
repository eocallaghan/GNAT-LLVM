------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--      S Y N C H R O N I Z E _ N O N R E E N T R A N T _ A C C E S S       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2005 Free Software Foundation, Inc.          --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is an AlphaVMS package use with DECLIB to implement the functions
--  described in the package spec.  For DEC Ada they are calls into the
--  runtime, which, after examining the source listings appear to call
--  equivalent pthread global locking routines.

with Interfaces.C;
with System.OS_Interface;
use System.OS_Interface;
package body Synchronize_Nonreentrant_Access is

   use type Interfaces.C.int;

   -----------------
   -- Global_Lock --
   -----------------

   procedure Global_Lock is
      I : int;
   begin
      I := pthread_lock_global_np;
   end Global_Lock;

   -------------------
   -- Global_Unlock --
   -------------------

   procedure Global_Unlock is
      I : int;
   begin
      I := pthread_unlock_global_np;
      if I /= 0 then
         raise Program_Error;
      end if;
   end Global_Unlock;

end Synchronize_Nonreentrant_Access;
