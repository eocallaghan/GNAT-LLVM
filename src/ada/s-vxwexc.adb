------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--              S Y S T E M . V X W O R K S _ E X C E P T I O N S           --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--           Copyright (C) 2004-2005 Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

package body System.VxWorks_Exceptions is

   --------------------
   --  Setup_For_EH  --
   --------------------

   procedure Setup_For_EH is
   begin

      --  In a ZCX library, call the EH table registration function if need
      --  be, that is, if the module is known not to have the special _ctor
      --  variable expected to trigger the call at load time.

      --  Note that we rely on the front-end not to expand the code inside the
      --  condition when it is False (the System flag is constant), to avoid
      --  issuing references to the external symbols in the non-zcx case. Such
      --  references are damaging for modules not linked against the crt
      --  objects because the references would remain unsatisfied.

      if System.ZCX_By_Default then
         declare
            module_has_ctors : Integer;
            pragma Import (C, module_has_ctors, "__module_has_ctors");

            procedure do_global_ctors;
            pragma Import (C, do_global_ctors, "__do_global_ctors");
         begin
            if module_has_ctors = 0 then
               do_global_ctors;
            end if;
         end;
      end if;
   end Setup_For_EH;

end System.VxWorks_Exceptions;
