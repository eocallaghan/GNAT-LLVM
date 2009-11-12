------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       ADA.EXCEPTIONS.EXCEPTION_DATA                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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

--  This is a simplified version of this package where no exception messages
--  are stored in exception occurrences.

with System.Storage_Elements; use System.Storage_Elements;

separate (Ada.Exceptions)
package body Exception_Data is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Address_Image (A : System.Address) return String;
   --  Returns at string of the form 0xhhhhhhhhh for 32-bit addresses
   --  or 0xhhhhhhhhhhhhhhhh for 64-bit addresses. Hex characters are
   --  in lower case.

   procedure Append_Info_Nat
     (N    : Natural;
      Info : in out String;
      Ptr  : in out Natural);
   --  Append the image of N at the end of the provided information string

   procedure Append_Info_NL
     (Info : in out String;
      Ptr  : in out Natural);
   --  Append a LF at the end of the provided information string

   procedure Append_Info_String
     (S    : String;
      Info : in out String;
      Ptr  : in out Natural);
   --  Append a string at the end of the provided information string

   --  To build Exception_Information and Tailored_Exception_Information,
   --  we then use three intermediate functions :

   function Basic_Exception_Information
     (X    : Exception_Occurrence)
      return String;
   --  Returns the basic exception information string associated with a
   --  given exception occurrence. This is the common part shared by both
   --  Exception_Information and Tailored_Exception_Infomation.

   function Basic_Exception_Traceback
     (X    : Exception_Occurrence)
      return String;
   --  Returns an image of the complete call chain associated with an
   --  exception occurence in its most basic form, that is as a raw sequence
   --  of hexadecimal binary addresses.

   function Tailored_Exception_Traceback
     (X    : Exception_Occurrence)
      return String;
   --  Returns an image of the complete call chain associated with an
   --  exception occurrence, either in its basic form if no decorator is
   --  in place, or as formatted by the decorator otherwise.

   --  The overall organization of the exception information related code
   --  is summarized below :
   --
   --           Exception_Information
   --                    |
   --            +-------+--------+
   --            |                |
   --     Basic_Exc_Info & Basic_Exc_Tback
   --
   --
   --       Tailored_Exception_Information
   --                    |
   --         +----------+----------+
   --         |                     |
   --  Basic_Exc_Info    &  Tailored_Exc_Tback
   --                               |
   --                   +-----------+------------+
   --                   |                        |
   --            Basic_Exc_Tback    Or    Tback_Decorator
   --          if no decorator set           otherwise

   -------------------
   -- Address_Image --
   -------------------

   function Address_Image (A : Address) return String is
      S : String (1 .. 18);
      P : Natural;
      N : Integer_Address;

      H : constant array (Integer range 0 .. 15) of Character :=
                                                         "0123456789abcdef";
   begin
      P := S'Last;
      N := To_Integer (A);
      while N /= 0 loop
         S (P) := H (Integer (N mod 16));
         P := P - 1;
         N := N / 16;
      end loop;

      S (P - 1) := '0';
      S (P) := 'x';
      return S (P - 1 .. S'Last);
   end Address_Image;

   ---------------------
   -- Append_Info_Nat --
   ---------------------

   procedure Append_Info_Nat
     (N    : Natural;
      Info : in out String;
      Ptr  : in out Natural)
   is
   begin
      if N > 9 then
         Append_Info_Nat (N / 10, Info, Ptr);
      end if;

      Ptr := Ptr + 1;
      Info (Ptr) := Character'Val (Character'Pos ('0') + N mod 10);
   end Append_Info_Nat;

   --------------------
   -- Append_Info_NL --
   --------------------

   procedure Append_Info_NL
     (Info : in out String;
      Ptr  : in out Natural)
   is
   begin
      Ptr := Ptr + 1;
      Info (Ptr) := ASCII.LF;
   end Append_Info_NL;

   ------------------------
   -- Append_Info_String --
   ------------------------

   procedure Append_Info_String
     (S    : String;
      Info : in out String;
      Ptr  : in out Natural)
   is
      Last : constant Natural := Integer'Min (Ptr + S'Length, Info'Last);

   begin
      Info (Ptr + 1 .. Last) := S;
      Ptr := Last;
   end Append_Info_String;

   ---------------------------------
   -- Basic_Exception_Information --
   ---------------------------------

   function Basic_Exception_Information
     (X    : Exception_Occurrence)
      return String
   is
      Name : constant String := Exception_Name (X);
      --  Exception name that is going to be included in the
      --  information to return, if not empty.

      Name_Len : constant Natural := Name'Length;
      --  Length of name string, useful to compute the size of the string
      --  we have to allocate for the complete result as well as in the body
      --  of this procedure.

      Info_Maxlen : constant Natural := 39 + Name_Len;
      --  Maximum length of the information string we will build, with :
      --
      --  39 =    16 + 2   for the text associated with the name
      --        +  5 + 2   for the text associated with the pid
      --        + 14       for the text image of the pid itself and a margin.
      --
      --  This is indeed a maximum since some data may not appear at all if
      --  not relevant. For example, nothing related to the exception message
      --  will be there if this message is empty.
      --
      --  WARNING : Do not forget to update these numbers if anything
      --  involved in the computation changes.

      Info : String (1 .. Info_Maxlen);
      --  Information string we are going to build, containing the common
      --  part shared by Exc_Info and Tailored_Exc_Info.

      Ptr  : Natural := 0;

   begin
      --  Output exception name and message except for _ABORT_SIGNAL, where
      --  these two lines are omitted (see discussion above).

      if Name (1) /= '_' then
         Append_Info_String ("Exception name: ", Info, Ptr);
         Append_Info_String (Name, Info, Ptr);
         Append_Info_NL (Info, Ptr);
      end if;

      --  Output PID line if non-zero

      if X.Pid /= 0 then
         Append_Info_String ("PID: ", Info, Ptr);
         Append_Info_Nat (X.Pid, Info, Ptr);
         Append_Info_NL (Info, Ptr);
      end if;

      return Info (1 .. Ptr);
   end Basic_Exception_Information;

   -------------------------------
   -- Basic_Exception_Traceback --
   -------------------------------

   function Basic_Exception_Traceback
     (X    : Exception_Occurrence)
      return String
   is
      Info_Maxlen : constant Natural := 35 + X.Num_Tracebacks * 19;
      --  Maximum length of the information string we are building, with :
      --  33 = 31 + 4      for the text before and after the traceback, and
      --  19 =  2 + 16 + 1 for each address ("0x" + HHHH + " ")
      --
      --  WARNING : Do not forget to update these numbers if anything
      --  involved in the computation changes.

      Info : String (1 .. Info_Maxlen);
      --  Information string we are going to build, containing an image
      --  of the call chain associated with the exception occurrence in its
      --  most basic form, that is as a sequence of binary addresses.

      Ptr  : Natural := 0;

   begin
      if X.Num_Tracebacks > 0 then
         Append_Info_String ("Call stack traceback locations:", Info, Ptr);
         Append_Info_NL (Info, Ptr);

         for J in 1 .. X.Num_Tracebacks loop
            Append_Info_String (Address_Image (X.Tracebacks (J)), Info, Ptr);
            exit when J = X.Num_Tracebacks;
            Append_Info_String (" ", Info, Ptr);
         end loop;

         Append_Info_NL (Info, Ptr);
      end if;

      return Info (1 .. Ptr);
   end Basic_Exception_Traceback;

   ---------------------------
   -- Exception_Information --
   ---------------------------

   --  The format of the string is:

   --    Exception_Name: nnnnn
   --    PID: ppp
   --    Call stack traceback locations:
   --    0xhhhh 0xhhhh 0xhhhh ... 0xhhh

   --  where

   --    nnnn is the fully qualified name of the exception in all upper
   --    case letters. This line is always present.

   --    ppp is the Process Id value as a decimal integer (this line is
   --    present only if the Process Id is non-zero). Currently we are
   --    not making use of this field.

   --    The Call stack traceback locations line and the following values
   --    are present only if at least one traceback location was recorded.
   --    the values are given in C style format, with lower case letters
   --    for a-f, and only as many digits present as are necessary.

   --  The line terminator sequence at the end of each line, including the
   --  last line is a CR-LF sequence (16#0D# followed by 16#0A#).

   --  The Exception_Name line is omitted in the abort
   --  signal case, since this is not really an exception, and the only
   --  use of this routine is internal for printing termination output.

   --  WARNING: if the format of the generated string is changed, please note
   --  that an equivalent modification to the routine String_To_EO must be
   --  made to preserve proper functioning of the stream attributes.

   function Exception_Information (X : Exception_Occurrence) return String is

      --  This information is now built using the circuitry introduced in
      --  association with the support of traceback decorators, as the
      --  catenation of the exception basic information and the call chain
      --  backtrace in its basic form.

      Basic_Info : constant String  := Basic_Exception_Information (X);
      Tback_Info : constant String  := Basic_Exception_Traceback (X);

      Basic_Len  : constant Natural := Basic_Info'Length;
      Tback_Len  : constant Natural := Tback_Info'Length;

      Info : String (1 .. Basic_Len + Tback_Len);
      Ptr  : Natural := 0;

   begin
      Append_Info_String (Basic_Info, Info, Ptr);
      Append_Info_String (Tback_Info, Info, Ptr);

      return Info;
   end Exception_Information;

   -------------------------
   -- Set_Exception_C_Msg --
   -------------------------

   procedure Set_Exception_C_Msg
     (Id   : Exception_Id;
      Msg1 : Big_String_Ptr;
      Line : Integer        := 0;
      Msg2 : Big_String_Ptr := null)
   is
      pragma Unreferenced (Msg1, Line, Msg2);

      Excep  : constant EOA := Get_Current_Excep.all;
   begin
      Exception_Propagation.Setup_Exception (Excep, Excep);
      Excep.Exception_Raised := False;
      Excep.Id               := Id;
      Excep.Num_Tracebacks   := 0;
      Excep.Pid              := Local_Partition_ID;
      Excep.Msg_Length       := 0;
      Excep.Cleanup_Flag     := False;
   end Set_Exception_C_Msg;

   -----------------------
   -- Set_Exception_Msg --
   -----------------------

   procedure Set_Exception_Msg
     (Id      : Exception_Id;
      Message : String)
   is
      pragma Unreferenced (Message);

      Excep  : constant EOA := Get_Current_Excep.all;
   begin
      Exception_Propagation.Setup_Exception (Excep, Excep);
      Excep.Exception_Raised := False;
      Excep.Msg_Length       := 0;
      Excep.Id               := Id;
      Excep.Num_Tracebacks   := 0;
      Excep.Pid              := Local_Partition_ID;
      Excep.Msg_Length       := 0;
      Excep.Cleanup_Flag     := False;
   end Set_Exception_Msg;

   ----------------------------------
   -- Tailored_Exception_Traceback --
   ----------------------------------

   function Tailored_Exception_Traceback
     (X    : Exception_Occurrence)
      return String
   is
      --  We indeed reference the decorator *wrapper* from here and not the
      --  decorator itself. The purpose of the local variable Wrapper is to
      --  prevent a potential crash by race condition in the code below. The
      --  atomicity of this assignment is enforced by pragma Atomic in
      --  System.Soft_Links.

      --  The potential race condition here, if no local variable was used,
      --  relates to the test upon the wrapper's value and the call, which
      --  are not performed atomically. With the local variable, potential
      --  changes of the wrapper's global value between the test and the
      --  call become inoffensive.

      Wrapper : constant Traceback_Decorator_Wrapper_Call :=
                  Traceback_Decorator_Wrapper;

   begin
      if Wrapper = null then
         return Basic_Exception_Traceback (X);
      else
         return Wrapper.all (X.Tracebacks'Address, X.Num_Tracebacks);
      end if;
   end Tailored_Exception_Traceback;

   ------------------------------------
   -- Tailored_Exception_Information --
   ------------------------------------

   function Tailored_Exception_Information
     (X    : Exception_Occurrence)
      return String
   is
      --  The tailored exception information is simply the basic information
      --  associated with the tailored call chain backtrace.

      Basic_Info : constant String  := Basic_Exception_Information (X);
      Tback_Info : constant String  := Tailored_Exception_Traceback (X);

      Basic_Len  : constant Natural := Basic_Info'Length;
      Tback_Len  : constant Natural := Tback_Info'Length;

      Info : String (1 .. Basic_Len + Tback_Len);
      Ptr  : Natural := 0;

   begin
      Append_Info_String (Basic_Info, Info, Ptr);
      Append_Info_String (Tback_Info, Info, Ptr);

      return Info;
   end Tailored_Exception_Information;

   procedure Tailored_Exception_Information
     (X    : Exception_Occurrence;
      Buff : in out String;
      Last : in out Integer)
   is
   begin
      Append_Info_String (Basic_Exception_Information (X), Buff, Last);
      Append_Info_String (Tailored_Exception_Traceback (X), Buff, Last);
   end Tailored_Exception_Information;

end Exception_Data;
