------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                S Y S T E M _ R U N T I M E _ T U N I N G                 --
--                                                                          --
--                                 B o d y                                  --
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

--  This is an AlphaVMS package

with System.AST_Handling;
package body System_Runtime_Tuning is

   ----------------------------
   -- Expand_AST_Packet_Pool --
   ----------------------------

   procedure Expand_AST_Packet_Pool
     (Requested_Packets : AST_Packet_Request_Type;
      Actual_Number     : out Natural;
      Total_Number      : out Natural)
   is
   begin
      System.AST_Handling.Expand_AST_Packet_Pool
        (Requested_Packets, Actual_Number, Total_Number);
   end Expand_AST_Packet_Pool;

   ------------------------
   -- Request_Time_Slice --
   ------------------------

   procedure Request_Time_Slice (Requested_Value : Duration) is
      procedure AST_In_Prog (Result : out Boolean);
      pragma Interface (External, AST_In_Prog);
      pragma Import_Valued_Procedure
        (AST_In_Prog, "lib$ast_in_prog", (Boolean), (Value));

      In_Prog : Boolean;

   begin
      --  Raise Program_Error if called from AST Level iaw C980VTE,
      --  otherwise it's just a no-op.

      AST_In_Prog (In_Prog);

      if In_Prog then
         raise Program_Error;
      end if;
   end Request_Time_Slice;
end System_Runtime_Tuning;
