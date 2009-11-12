------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . S O F T _ L I N K S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2008, Free Software Foundation, Inc.         --
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

package body System.Soft_Links is

   NT_TSD : TSD;
   --  Note: we rely on the default initialization of NT_TSD

   ------------------------
   -- Get_GNAT_Exception --
   ------------------------

   function Get_GNAT_Exception return Ada.Exceptions.Exception_Id is
   begin
      return Ada.Exceptions.Exception_Identity (Get_Current_Excep.all.all);
   end Get_GNAT_Exception;

   --------------------------
   -- Get_Current_Excep_NT --
   --------------------------

   function Get_Current_Excep_NT return EOA is
   begin
      return NT_TSD.Current_Excep'Access;
   end Get_Current_Excep_NT;

   ---------------------------
   -- Get_Jmpbuf_Address_NT --
   ---------------------------

   function Get_Jmpbuf_Address_NT return  Address is
   begin
      return NT_TSD.Jmpbuf_Address;
   end Get_Jmpbuf_Address_NT;

   -----------------------------
   -- Get_Jmpbuf_Address_Soft --
   -----------------------------

   function Get_Jmpbuf_Address_Soft return  Address is
   begin
      return Get_Jmpbuf_Address.all;
   end Get_Jmpbuf_Address_Soft;

   ---------------------------
   -- Get_Sec_Stack_Addr_NT --
   ---------------------------

   function Get_Sec_Stack_Addr_NT return  Address is
   begin
      return NT_TSD.Sec_Stack_Addr;
   end Get_Sec_Stack_Addr_NT;

   -----------------------------
   -- Get_Sec_Stack_Addr_Soft --
   -----------------------------

   function Get_Sec_Stack_Addr_Soft return  Address is
   begin
      return Get_Sec_Stack_Addr.all;
   end Get_Sec_Stack_Addr_Soft;

   -------------------
   -- Null_Adafinal --
   -------------------

   procedure Null_Adafinal is
   begin
      null;
   end Null_Adafinal;

   ---------------------------
   -- Set_Jmpbuf_Address_NT --
   ---------------------------

   procedure Set_Jmpbuf_Address_NT (Addr : Address) is
   begin
      NT_TSD.Jmpbuf_Address := Addr;
   end Set_Jmpbuf_Address_NT;

   -----------------------------
   -- Set_Jmpbuf_Address_Soft --
   -----------------------------

   procedure Set_Jmpbuf_Address_Soft (Addr : Address) is
   begin
      Set_Jmpbuf_Address (Addr);
   end Set_Jmpbuf_Address_Soft;

   ---------------------------
   -- Set_Sec_Stack_Addr_NT --
   ---------------------------

   procedure Set_Sec_Stack_Addr_NT (Addr : Address) is
   begin
      NT_TSD.Sec_Stack_Addr := Addr;
   end Set_Sec_Stack_Addr_NT;

   -----------------------------
   -- Set_Sec_Stack_Addr_Soft --
   -----------------------------

   procedure Set_Sec_Stack_Addr_Soft (Addr : Address) is
   begin
      Set_Sec_Stack_Addr (Addr);
   end Set_Sec_Stack_Addr_Soft;

end System.Soft_Links;
