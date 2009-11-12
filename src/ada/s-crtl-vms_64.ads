------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                          S Y S T E M . C R T L                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2003-2008, Free Software Foundation, Inc.         --
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

--  This package provides the low level interface to the C Run Time Library
--  on VMS 64bit systems (note malloc32 and realloc32 functions).

with System.Parameters;

package System.CRTL is
   pragma Preelaborate;

   subtype chars is System.Address;
   --  Pointer to null-terminated array of characters

   subtype DIRs is System.Address;
   --  Corresponds to the C type DIR*

   subtype FILEs is System.Address;
   --  Corresponds to the C type FILE*

   subtype int is Integer;

   type long is range -(2 ** (System.Parameters.long_bits - 1))
      .. +(2 ** (System.Parameters.long_bits - 1)) - 1;

   subtype off_t is Long_Integer;

   type size_t is mod 2 ** Standard'Address_Size;

   type Filename_Encoding is (UTF8, ASCII_8bits, Unspecified);
   for Filename_Encoding use (UTF8 => 0, ASCII_8bits => 1, Unspecified => 2);
   pragma Convention (C, Filename_Encoding);
   --  Describes the filename's encoding

   function atoi (A : System.Address) return Integer;
   pragma Import (C, atoi, "atoi");

   procedure clearerr (stream : FILEs);
   pragma Import (C, clearerr, "clearerr");

   function dup  (handle : int) return int;
   pragma Import (C, dup, "dup");

   function dup2 (from, to : int) return int;
   pragma Import (C, dup2, "dup2");

   function fclose (stream : FILEs) return int;
   pragma Import (C, fclose, "fclose");

   function fdopen (handle : int; mode : chars) return FILEs;
   pragma Import (C, fdopen, "fdopen");

   function fflush (stream : FILEs) return int;
   pragma Import (C, fflush, "fflush");

   function fgetc (stream : FILEs) return int;
   pragma Import (C, fgetc, "fgetc");

   function fgets (strng : chars; n : int; stream : FILEs) return chars;
   pragma Import (C, fgets, "fgets");

   function fopen
     (filename : chars;
      mode     : chars;
      encoding : Filename_Encoding := Unspecified) return FILEs;
   pragma Import (C, fopen, "__gnat_fopen");

   function fputc (C : int; stream : FILEs) return int;
   pragma Import (C, fputc, "fputc");

   function fputs (Strng : chars; Stream : FILEs) return int;
   pragma Import (C, fputs, "fputs");

   procedure free (Ptr : System.Address);
   pragma Import (C, free, "free");

   function freopen
     (filename : chars;
      mode     : chars;
      stream   : FILEs;
      encoding : Filename_Encoding := Unspecified) return FILEs;
   pragma Import (C, freopen, "__gnat_freopen");

   function fseek
     (stream : FILEs;
      offset : long;
      origin : int)
      return   int;
   pragma Import (C, fseek, "fseek");

   function ftell (stream : FILEs) return long;
   pragma Import (C, ftell, "ftell");

   function getenv (S : String) return System.Address;
   pragma Import (C, getenv, "getenv");

   function isatty (handle : int) return int;
   pragma Import (C, isatty, "isatty");

   function lseek (fd : int; offset : off_t; direction : int) return off_t;
   pragma Import (C, lseek, "lseek");

   function malloc (Size : size_t) return System.Address;
   pragma Import (C, malloc, "malloc");

   function malloc32 (Size : size_t) return System.Address;
   pragma Import (C, malloc32, "_malloc32");
   --  An alias for malloc for allocating 32bit memory on 64bit VMS

   procedure memcpy (S1 : System.Address; S2 : System.Address; N : size_t);
   pragma Import (C, memcpy, "memcpy");

   procedure memmove (S1 : System.Address; S2 : System.Address; N : size_t);
   pragma Import (C, memmove, "memmove");

   procedure mktemp (template : chars);
   pragma Import (C, mktemp, "mktemp");

   function pclose (stream : System.Address) return int;
   pragma Import (C, pclose, "pclose");

   function popen (command, mode : System.Address) return System.Address;
   pragma Import (C, popen, "popen");

   function realloc
     (Ptr : System.Address; Size : size_t) return System.Address;
   pragma Import (C, realloc, "realloc");

   function realloc32
     (Ptr : System.Address; Size : size_t) return System.Address;
   pragma Import (C, realloc32, "_realloc32");
   --  An alias for realloc for allocating 32bit memory on 64bit VMS

   procedure rewind (stream : FILEs);
   pragma Import (C, rewind, "rewind");

   function rmdir (dir_name : String) return int;
   pragma Import (C, rmdir, "rmdir");

   function chdir (dir_name : String) return int;
   pragma Import (C, chdir, "chdir");

   function setvbuf
     (stream : FILEs;
      buffer : chars;
      mode   : int;
      size   : size_t)
      return   int;
   pragma Import (C, setvbuf, "setvbuf");

   procedure tmpnam (string : chars);
   pragma Import (C, tmpnam, "tmpnam");

   function tmpfile return FILEs;
   pragma Import (C, tmpfile, "tmpfile");

   function ungetc (c : int; stream : FILEs) return int;
   pragma Import (C, ungetc, "ungetc");

   function unlink (filename : chars) return int;
   pragma Import (C, unlink, "unlink");

   function open (filename : chars; oflag : int) return int;
   pragma Import (C, open, "open");

   function close (fd : int) return int;
   pragma Import (C, close, "close");

   function read (fd : int; buffer : chars; nbytes : int) return int;
   pragma Import (C, read, "read");

   function write (fd : int; buffer : chars; nbytes : int) return int;
   pragma Import (C, write, "write");

end System.CRTL;
