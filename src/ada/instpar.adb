------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              I N S T P A R                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2006-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Gnatvsn;  use Gnatvsn;
with Namet;    use Namet;
with Opt;
with Output;   use Output;
with Sdefault; use Sdefault;
with Sinput;   use Sinput;
with Sinput.L; use Sinput.L;
with Types;    use Types;

with System.OS_Lib; use System.OS_Lib;
with System.CRC32;  use System.CRC32;

with Interfaces;   use Interfaces;

package body Instpar is

   --  Format of the license file:

   --  The following license to [use|evaluate] GNAT <buildtype-version>
   --  [for <target-platform>] [hosted on <host-platform>] is granted to
   --  <licensee> [until <validity-date>].

   --  blah
   --  blah
   --  blah

   --  <signature>

   --  The installation parameters file is checked by identifying a certain
   --  sequence of words. Words are delimited by white space, and handled
   --  using the offsets of their first and last character.

   type Word is record
      First : Source_Ptr := No_Location;
      Last  : Source_Ptr := No_Location;
   end record;

   --  When the installation parameters file is read, the following fields
   --  are identified for validation.

   IPS_Version_String : Word;
   --  Version string as returned by Gnatvsn.Gnat_Version_String

   IPS_Target         : Word;
   --  Target platform name

   IPS_Host           : Word;
   --  Host platform name

   --  The host platform name is not currently checked

   pragma Warnings (Off, IPS_Host);

   IPS_Licensee       : Word;
   --  Name of licensee

   IPS_Validity       : Word;
   --  License validity date in YYYY-MM-DD format

   IPS_Signature      : Word;
   --  Signature for file integrity verification (CRC-32 of the text up to
   --  the last character before the signature, in unsigned decimal
   --  representation).

   IPS_Full_Text : Word;
   --  Full text, excluding signature (last position of full text is defined
   --  to be last character before IPS_Signature.First).

   function Parse (Source : Source_Buffer_Ptr; IPS : Source_Ptr) return Int;
   --  Read the installation parameters file, identify the fields noted above,
   --  and return information as to the final state of the parser (0 is
   --  success, otherwise the returned value is the state in which the parser
   --  stopped).

   subtype Date_Text_Buffer is Text_Buffer (1 .. 10);

   function Expired (Exp_Date : Date_Text_Buffer) return Boolean;
   --  This function returns False if Expiration_Date is a valid date of
   --  the form YYYY-MM-DD and is in the future.

   -----------------------------------
   -- Check_Installation_Parameters --
   -----------------------------------

   procedure Check_Installation_Parameters is
      Params_File_Name : constant String := "gnatlic.adl";
      --  Name of the parameters file, which is loaded from the GCC lib
      --  directory.

      Params_Config_File : Source_File_Index;

      Status : Int;

      --  Status code:
      --  0     Success

      --  1..15 Parse failed

      --  or a bitwise 'or' of

      Invalid_Signature      : constant := 16;
      --  Signature check failed

      Invalid_Version_String : constant := 32;
      --  Version string of compiler does not match parameters file

      Invalid_Target         : constant := 64;
      --  Target platform of compiler does not match parameters file

      --  Invalid_Host       : constant := 128;
      --  Host platform of compiler does not match parameters file
      --  (we currently do not check that)

      Invalid_Expiration_Date : constant := 256;
      --  Parameters are expired

      Invalid_Parameters_File : constant := 512;
      --  Parameters file is missing

      CRC : System.CRC32.CRC32;
      Expected_CRC : Interfaces.Unsigned_32;

      Source : Source_Buffer_Ptr;

   begin
      --  No check if running without the standard runtime library

      if Opt.No_Stdinc then
         return;
      end if;

      --  Otherwise proceed with check

      Name_Len := 0;
      Add_Str_To_Name_Buffer (Params_File_Name);
      Params_Config_File := Load_Source_File (Name_Enter);

      --  1. Check for missing file

      if Params_Config_File = No_Source_File then
         Status := Invalid_Parameters_File;
         goto Check_Failed;
      end if;

      --  2. Load and parse installation parameters file

      Source := Source_Text (Params_Config_File);
      Status := Parse (Source, Source_First (Params_Config_File));
      if Status /= 0 then
         goto Check_Failed;
      end if;

      --  3. Check signature

      --  Read expected value from parameters file

      Expected_CRC := 0;
      for J in IPS_Signature.First .. IPS_Signature.Last loop
         Expected_CRC := Expected_CRC * 10
                           + Character'Pos (Source (J)) - Character'Pos ('0');
      end loop;

      --  Recompute value

      Initialize (CRC);
      for J in IPS_Full_Text.First .. IPS_Full_Text.Last loop
         Update (CRC, Source (J));
      end loop;

      --  Check that they match

      if Get_Value (CRC) /= Expected_CRC then
         Status := Status + Invalid_Signature;
      end if;

      --  4. Check version string: check that the version from the installation
      --     parameters file is a prefix of Gnat_Version_String. This allows
      --     the IPs file to be shared by all builds of a given GNAT version
      --     (if no date is included) or to be generated for a specific build
      --     (if a version is included).

      declare
         IPS_Version_String_Length : constant Integer :=
                                       Integer (IPS_Version_String.Last
                                              - IPS_Version_String.First)
                                       + 1;
         Gnat_Version : constant String := Gnat_Version_String;
      begin
         if IPS_Version_String_Length > Gnat_Version'Length
           or else String (Source (IPS_Version_String.First
                                .. IPS_Version_String.Last))
                     /= Gnat_Version (Gnat_Version'First
                                   .. Gnat_Version'First
                                        + IPS_Version_String_Length - 1)
         then
            Status := Status + Invalid_Version_String;
         end if;
      end;

      --  5. Check target platform, if specified in parameters file

      if IPS_Target.First < IPS_Target.Last then
         declare
            Target      : constant String := Target_Name.all;
            Target_Last : Integer         := Target'Last;

         begin
            --  Target_Name may append a trailing '/' to Target

            if Target_Last > Target'First
              and then Target (Target_Last) = '/'
            then
               Target_Last := Target_Last - 1;
            end if;

            if String (Source (IPS_Target.First .. IPS_Target.Last))
                 /= Target (Target'First .. Target_Last)
            then
               Status := Status + Invalid_Target;
            end if;
         end;
      end if;

      --  6. Check host platform, if specified (not currently enforced)

      --  if IPS_Host.First < IPS_Host.Last
      --    and then String (Source (IPS_Host.First .. IPS_Host.Last))
      --               /= Host (Host'First .. Host_Last)
      --  then
      --     Status := Status + Invalid_Host;
      --  end if;

      --  7. Check expiration date

      if IPS_Validity.First /= No_Location then
         declare
            Exp_Date : constant Date_Text_Buffer :=
                         Source (IPS_Validity.First .. IPS_Validity.Last);
         begin
            if Expired (Exp_Date) then
               Status := Status + Invalid_Expiration_Date;
            end if;
         end;
      end if;

      if Status = 0 then
         return;
      end if;

   <<Check_Failed>>

      --  Output fatal error message including Status

      Write_Str ("fatal error, installation parameters check failed: ");
      Write_Int (Status);
      Write_Eol;
      raise Unrecoverable_Error;
   end Check_Installation_Parameters;

   -------------
   -- Expired --
   -------------

   function Expired (Exp_Date : Date_Text_Buffer) return Boolean is
      Year   : Year_Type;
      Month  : Month_Type;
      Day    : Day_Type;
      Hour   : Hour_Type;
      Minute : Minute_Type;
      Second : Second_Type;

      pragma Warnings (Off, Hour);
      pragma Warnings (Off, Minute);
      pragma Warnings (Off, Second);

      Cur_Date : Date_Text_Buffer := "0000-00-00";

      procedure Image (X : Integer; LSD : Text_Ptr);
      --  Store decimal image of X into Cur_Date with least significant digit
      --  at LSD.

      procedure Image (X : Integer; LSD : Text_Ptr) is
         Val : Integer := X;
         Pos : Text_Ptr := LSD;
      begin
         while Pos >= Cur_Date'First and then Val > 0 loop
            Cur_Date (Pos) := Character'Val (Character'Pos ('0') + Val rem 10);
            Pos := Pos - 1;
            Val := Val / 10;
         end loop;
      end Image;

   --  Start of processing for Expired

   begin
      GM_Split (Current_Time, Year, Month, Day, Hour, Minute, Second);
      Image (Year, 4);
      Image (Month, 7);
      Image (Day, 10);
      return Cur_Date > Exp_Date;
   end Expired;

   -----------
   -- Parse --
   -----------

   function Parse (Source : Source_Buffer_Ptr; IPS : Source_Ptr) return Int is

      --  Parser states

      --  Note: the specification for Parse defines that a 0 result is success,
      --  and the returned value is actually State'Pos of the final parser
      --  state, so S_Success must be the first literal in type State.

      type State is
        (S_Success,
         S_Initial,
         S_Version_String,
         S_Target,
         S_Host,
         S_Granted,
         S_Licensee,
         S_Validity,
         S_Signature);

      Current_State : State;
      --  Current state of the parser

      IPS_Current_Word : Word;
      --  The current word being looked at by the parser

      function Current_Word return Word;
      --  Return the current word (IPS_Current_Word)

      function Current_Word return Text_Buffer;
      --  Return the text of the current word as extracted from Source, as an
      --  1-based string.

      procedure Next_Word;
      --  Move IPS_Current_Word to the next word in the file. If end-of-file is
      --  reached, IPS_Current_Word is set to a word of null length.

      procedure Next_State;
      --  Move to the next state in the natural order of type State

      procedure Next_State (New_State : State);
      --  Move to the indicated new state

      ------------------
      -- Current_Word --
      ------------------

      function Current_Word return Word is
      begin
         return IPS_Current_Word;
      end Current_Word;

      ------------------
      -- Current_Word --
      ------------------

      function Current_Word return Text_Buffer is
         Result : Text_Buffer
                    (1 .. IPS_Current_Word.Last - IPS_Current_Word.First + 1);
      begin
         Result := Source (IPS_Current_Word.First .. IPS_Current_Word.Last);
         return Result;
      end Current_Word;

      ----------------
      -- Next_State --
      ----------------

      procedure Next_State is
      begin
         Current_State := State'Succ (Current_State);
      end Next_State;

      ----------------
      -- Next_State --
      ----------------

      procedure Next_State (New_State : State) is
      begin
         Current_State := New_State;
      end Next_State;

      ---------------
      -- Next_Word --
      ---------------

      procedure Next_Word is

         procedure Find_Skip_Whitespace
           (Index : in out Source_Ptr;
            Skip  : Boolean);
         --  Advance Index until the next character is non-whitespace (if Skip
         --  is True) or whitespace (if Skip is False). Also exit if the next
         --  character is the end of file marker.

         procedure Find_Skip_Whitespace
           (Index : in out Source_Ptr;
            Skip  : Boolean)
         is
            use ASCII;

            Whitespace : constant array (Character) of Boolean :=
                           (' ' | HT | CR | LF => True, others => False);

         begin
            loop
               exit when Source (Index + 1) = EOF;
               exit when Whitespace (Source (Index + 1)) xor Skip;
               Index := Index + 1;
            end loop;
         end Find_Skip_Whitespace;

      --  Start of processing for Next_Word

      begin
         IPS_Current_Word.First := IPS_Current_Word.Last + 1;

         --  Here IPS_Current_Word.First is either a whitespace character,
         --  or EOF, except for the first word.

         if Source (IPS_Current_Word.First) = EOF then
            return;
         end if;

         if IPS_Current_Word.First /= IPS then

            --  Advance Current_Word.First up to last whitespace character in
            --  this span.

            Find_Skip_Whitespace (IPS_Current_Word.First, Skip => True);

            --  Advance to following non-whitespace character

            IPS_Current_Word.First := IPS_Current_Word.First + 1;

         end if;

         --  Case of a file ending with whitespace

         if Source (IPS_Current_Word.First) = EOF then
            return;
         end if;

         --  Find end of current word

         IPS_Current_Word.Last := IPS_Current_Word.First - 1;
         Find_Skip_Whitespace (IPS_Current_Word.Last, Skip => False);
      end Next_Word;

   --  Start of processing for Parse

   begin
      IPS_Current_Word.Last := IPS - 1;
      IPS_Full_Text.First := IPS;
      Current_State := S_Initial;

      loop
         Next_Word;

         if Current_Word.Last < Current_Word.First then

            --  If end-of-file is encountered while looking for the signature,
            --  we have a succesful parse (the signature is just the last word
            --  of the file).

            if Current_State = S_Signature then
               Next_State (S_Success);
            end if;

            exit;
         end if;

         case Current_State is

            when S_Initial =>

               --  Skip everything until we encounter "GNAT"

               if Current_Word = "GNAT" then
                  Next_State;
               end if;

            when S_Version_String =>

               if Current_Word = "for" then
                  Next_State (S_Target);

               elsif Current_Word = "hosted" then
                  Next_State (S_Host);

               elsif Current_Word = "is" then
                  Next_State (S_Granted);

               else
                  if IPS_Version_String.First < IPS then
                     IPS_Version_String.First := Current_Word.First;
                  end if;
                  IPS_Version_String.Last := Current_Word.Last;
               end if;

            when S_Target =>

               --  Target platform

               if IPS_Target.First < IPS then
                  IPS_Target := Current_Word;

               elsif Current_Word = "hosted" then
                  Next_State;

               else
                  Next_State (S_Granted);
               end if;

            when S_Host =>

               --  Host platform

               --  Skip "on"

               Next_Word;

               IPS_Host := Current_Word;
               Next_State;

            when S_Granted =>

               if Current_Word = "granted" then

                  --  Skip "to"

                  Next_Word;
                  Next_State;
               end if;

            when S_Licensee =>

               --  Licensee

               if Current_Word = "until" then
                  Next_State;

               else
                  if IPS_Licensee.First < IPS then
                     IPS_Licensee.First := Current_Word.First;
                  end if;

                  IPS_Licensee.Last := Current_Word.Last;

                  if Source (IPS_Licensee.Last) = '.' then
                     IPS_Licensee.Last := IPS_Licensee.Last - 1;
                     Next_State (S_Signature);
                  end if;
               end if;

            when S_Validity =>

               --  Validity date (optional field)

               IPS_Validity := Current_Word;

               if Source (IPS_Validity.Last) = '.' then
                  IPS_Validity.Last := IPS_Validity.Last - 1;
               end if;

               Next_State (S_Signature);

            when S_Signature =>

               --  The signature is the last word of the file: slide it through
               --  until we reach the end.

               IPS_Signature := Current_Word;

            when S_Success =>
               raise Program_Error;
         end case;

         exit when Current_State = S_Success;
      end loop;

      --  End of full text is last character before signature

      if IPS_Signature.First > IPS then
         IPS_Full_Text.Last := IPS_Signature.First - 1;
      end if;

      --  Return result (0 is success)

      return State'Pos (Current_State);
   end Parse;

end Instpar;
