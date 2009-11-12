------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               G N A T . E N C O D E _ U T F 8 _ S T R I N G              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2007, AdaCore                        --
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

--  This package provides a pre-instantiation of GNAT.Encode_String for the
--  common case of UTF-8 encoding. As noted in the documentation of that
--  package, this UTF-8 instantiation is efficient and specialized so that
--  it has only the code for the UTF-8 case. See g-encstr.ads for full
--  documentation on this package.

with GNAT.Encode_String;

with System.WCh_Con;

package GNAT.Encode_UTF8_String is
  new GNAT.Encode_String (System.WCh_Con.WCEM_UTF8);
