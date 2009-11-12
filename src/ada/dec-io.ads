------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                               D E C . I O                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1996-2005, Free Software Foundation, Inc.         --
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

--  This is an AlphaVMS package that contains the declarations and function
--  specifications needed by the DECLib IO packages.

with System.Task_Primitives;

package DEC.IO is
private

   package IO_Locking is
      type Access_Mutex is private;
      function Create_Mutex return Access_Mutex;
      procedure Acquire (M : Access_Mutex);
      procedure Release (M : Access_Mutex);

   private
      type Access_Mutex is access System.Task_Primitives.RTS_Lock;

      pragma Export_Function (Create_Mutex, "GNAT$CREATE_MUTEX",
          Mechanism => Value);
      pragma Export_Procedure (Acquire, "GNAT$ACQUIRE_MUTEX",
          Mechanism => Value);
      pragma Export_Procedure (Release, "GNAT$RELEASE_MUTEX",
          Mechanism => Value);
   end IO_Locking;

end DEC.IO;
