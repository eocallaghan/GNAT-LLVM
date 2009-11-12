------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--           S Y S T E M . I N T E R R U P T _ M A N A G E M E N T          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1992-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is a OpenVMS/Alpha version of this package

package body System.Interrupt_Management is

   ----------------
   -- Initialize --
   ----------------

   Initialized : Boolean := False;

   procedure Initialize is
      use System.OS_Interface;
      Status : Cond_Value_Type;

   begin
      if Initialized then
         return;
      end if;

      Initialized := True;
      Abort_Task_Interrupt := Interrupt_ID_0;
      --  Unused

      Reserve := Reserve or Keep_Unmasked or Keep_Masked;
      Reserve (Interrupt_ID_0) := True;

      Sys_Crembx
        (Status => Status,
         Prmflg => 0,
         Chan   => Rcv_Interrupt_Chan,
         Maxmsg => Interrupt_ID'Size,
         Bufquo => Interrupt_Bufquo,
         Lognam => "GNAT_Interrupt_Mailbox",
         Flags  => CMB_M_READONLY);
      pragma Assert ((Status and 1) = 1);

      Sys_Assign
        (Status => Status,
         Devnam => "GNAT_Interrupt_Mailbox",
         Chan   => Snd_Interrupt_Chan,
         Flags  => AGN_M_WRITEONLY);
      pragma Assert ((Status and 1) = 1);
   end Initialize;

end System.Interrupt_Management;
