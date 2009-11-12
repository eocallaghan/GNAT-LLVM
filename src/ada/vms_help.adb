------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            V M S _ H E L P                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2007, Free Software Foundation, Inc.         --
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

--  This tool is used to construct a text file "gnat.hlp" that is the input
--  to build the VMS GNAT help.
--
--  It takes its inputs from
--    - a file "gnat.hlp_in" for the general documentation of the GNAT driver
--      and of its commands, and
--    - package VMS_Data that contains the definition of the qualifiers;
--      each qualifier declaration is followed by comments that either are
--      the qualifier documentation or a special comment (NODOC) that
--      indicates that there is no documentation for this qualifier.

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with GNAT.OS_Lib;       use GNAT.OS_Lib;

with VMS_Conv; use VMS_Conv;

procedure VMS_Help is

   --  Strings that are found in vms_data.ads to characterize the line kind

   Switches_For_GNAT  : constant String := "   -- Switches for GNAT ";
   Constant_Switches  : constant String := "aliased constant Switches";
   Aliased_Constant_S : constant String := "aliased constant S";
   NODOC              : constant String := "   --  NODOC";
   Comment_Start      : constant String := "   --";

   Out_File_Name : String_Access;
   --  Path of the file "gnat.hlp", given as argument 3

   File_Out : File_Type;

   GNAT_Help_In : String_Access;
   --  Path of the file "gnat.hlp_in", given as argument 1

   VMS_Data_Ads : String_Access;
   --  Path of the file "vms_data.ads", given as argument 2

   File_In  : File_Type;
   Line_In : String (1 .. 100);
   Last_In : Natural;

   Visited : array (Command_Type) of Boolean := (others => False);
   --  An array to check which commands have qualifiers declared, and which
   --  have qualifiers declared several times.

   type Status_Kind is
     (No_Command,
      Command_Start,
      First_Qualifier,
      In_Qualifiers);

   Status : Status_Kind := No_Command;

   type Line_Kind is
     (Undefined,
      Switches_Command,
      Switches_Declaration,
      Constant_S,
      No_Doc,
      Comment,
      EOF);

   --  The state variables

   The_Line_Kind   : Line_Kind    := Undefined;
   The_Command     : Command_Type := Undefined;
   Current_Command : Command_Type := Undefined;

   Qualifier      : String (1 .. 50);
   Qualifier_Last : Natural;

   Current_Qualifier      : String (1 .. 50);
   Current_Qualifier_Last : Natural;

   Errors : Boolean := False;
   --  Set to True when there is an error

   type Doc_Line;
   type Doc_Line_Ref is access Doc_Line;
   Current_Line : Doc_Line_Ref := null;

   type Doc_Line is record
      Line : String_Access;
      Next : Doc_Line_Ref;
   end record;

   Pre_Qualifiers  : array (Command_Type) of Doc_Line_Ref;
   --  For each command, a linked list of lines to be written to the out file
   --  before the documentation of the qualifiers. Built from file
   --  "gnat.hlp_in".

   Post_Qualifiers : array (Command_Type) of Doc_Line_Ref;
   --  For each command, an optional linked list of lines to be written to the
   --  out file after the documentation of the qualifiers. Built from file
   --  "gnat.hlp_in".

   procedure Error (S : String);
   --  Report an error

   procedure Get_Line;
   --  Read one line from vms_data.ads and set The_Line_Kind

   -----------
   -- Error --
   -----------

   procedure Error (S : String) is
   begin
      Put_Line (Standard_Output, "***ERROR: " & S);
      Put_Line
        (Standard_Output,
         Line (File_In)'Img & ": " & Line_In (1 .. Last_In));
      New_Line (Standard_Output);
      Errors := True;
   end Error;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line is
      Start, Finish : Natural;

   begin
      if End_Of_File (File_In) then
         The_Line_Kind := EOF;

      else
         Get_Line (File_In, Line_In, Last_In);

         declare
            Line : constant String := Line_In (1 .. Last_In);
         begin
            --  Switches for GNAT <COMMAND>

            if Index (Line, Switches_For_GNAT) = 1 then
               The_Line_Kind := Switches_Command;
               Start := Switches_For_GNAT'Length + 1;
               Finish := Start;

               --  Find the command

               while Finish < Line'Last
                 and then Is_Letter (Line (Finish + 1))
               loop
                  Finish := Finish + 1;
               end loop;

               begin
                  The_Command := Command_Type'Value (Line (Start .. Finish));

                  if Visited (The_Command) then
                     Error ("Double definition");

                  else
                     Visited (The_Command) := True;
                  end if;

                  Status := Command_Start;

               exception
                  when Constraint_Error =>
                     Error ("Undefined Command");
                     The_Command := Undefined;
                     The_Line_Kind := Undefined;
                     Status := No_Command;
               end;

            --  <Command>_Switches : aliased constant Switches :=

            elsif Index (Line, Constant_Switches) /= 0 then
               The_Line_Kind := Switches_Declaration;
               Status := No_Command;
               The_Command := Undefined;

            --  S_<cmd>_<qual> : aliased constant S := "/<QUALIFIER>...

            elsif Index (Line, Aliased_Constant_S) /= 0 then
               if Status = No_Command then
                  Error ("Qualifier for undefined command");
               elsif Status = Command_Start then
                  Status := First_Qualifier;
               else
                  Status := In_Qualifiers;
               end if;

               The_Line_Kind := Constant_S;

               --  Find the qualifier

               Start := Index (Line, """/");

               if Start = 0 then
                  Error ("Undefined Qualifier");
                  The_Line_Kind := Undefined;
               else
                  Finish := Index (Line (Start .. Line'Last), "=");

                  if Finish = 0 then
                     Finish := Index (Line (Start .. Line'Last), " """);

                     if Finish = 0 then
                        Error ("Undefined Qualifier");
                        The_Line_Kind := Undefined;
                     end if;
                  end if;

                  if Finish /= 0 then
                     Qualifier_Last := Finish - Start - 1;
                     Qualifier (1 .. Qualifier_Last) :=
                       Line (Start + 1 .. Finish - 1);
                  end if;
               end if;

            --  NODOC (...)

            elsif Index (Line, NODOC) = 1 then
               The_Line_Kind := No_Doc;

            --  other comments

            elsif Index (Line, Comment_Start) = 1 then
               The_Line_Kind := Comment;

            else
               The_Line_Kind := Undefined;
            end if;
         end;
      end if;
   end Get_Line;

begin

   if Argument_Count /= 3 then
      Put_Line ("usage: vms_help <help_in> <vms_data.ads> <gnat.hlp>");
      OS_Exit (1);
   end if;

   GNAT_Help_In := new String'(Argument (1));
   VMS_Data_Ads := new String'(Argument (2));
   Out_File_Name := new String'(Argument (3));

   Create (File_Out, Out_File, Out_File_Name.all);
   Set_Output (File_Out);

   Visited (Undefined) := True;

   --  GNAT doc

   Open (File_In, In_File, GNAT_Help_In.all);

   Initial_Doc_Loop : loop
      Get_Line (File_In, Line_In, Last_In);

      if Last_In > 4 and then Line_In (1 .. 3) = "2  " then
         begin
            The_Command := Command_Type'Value (Line_In (4 .. Last_In));
            exit Initial_Doc_Loop;

         exception
            when Constraint_Error =>
               null;
         end;
      end if;

      Put_Line (Line_In (1 .. Last_In));
   end loop Initial_Doc_Loop;

   --  Get the Pre_Qualifiers and, if any, the Post_Qualifiers lines for
   --  each command.

   Command_Loop : loop
      begin
         The_Command := Command_Type'Value (Line_In (4 .. Last_In));

      exception
         when Constraint_Error =>
            Error ("not a command");
            exit Command_Loop;
      end;

      Current_Line := null;

      Pre_Qualifiers_Loop : loop
         exit Command_Loop when End_Of_File (File_In);

         Get_Line (File_In, Line_In, Last_In);

         --  >>>Qualifiers, if present, is where the qualifier documentation
         --  should be inserted.

         if Line_In (1 .. Last_In) = ">>>Qualifiers" then
            Current_Line := null;

            Post_Qualifiers_Loop : loop
               exit Command_Loop when End_Of_File (File_In);

               Get_Line (File_In, Line_In, Last_In);
               exit Pre_Qualifiers_Loop when
                 Last_In > 4 and then Line_In (1 .. 3) = "2  ";

               if Current_Line = null then
                  Current_Line :=
                    new Doc_Line'(new String'(Line_In (1 .. Last_In)), null);
                  Post_Qualifiers (The_Command) := Current_Line;

               else
                  Current_Line.Next :=
                    new Doc_Line'(new String'(Line_In (1 .. Last_In)), null);
                  Current_Line := Current_Line.Next;
               end if;
            end loop Post_Qualifiers_Loop;
         end if;

         --  "2  <COMMAND>" is the start of the next command,
         --  if ">>>Qualifiers" is not present.

         exit Pre_Qualifiers_Loop when
           Last_In > 4 and then Line_In (1 .. 3) = "2  ";

         if Current_Line = null then
            Current_Line :=
              new Doc_Line'(new String'(Line_In (1 .. Last_In)), null);
            Pre_Qualifiers (The_Command) := Current_Line;

         else
            Current_Line.Next :=
              new Doc_Line'(new String'(Line_In (1 .. Last_In)), null);
            Current_Line := Current_Line.Next;
         end if;
      end loop Pre_Qualifiers_Loop;

   end loop Command_Loop;

   Close (File_In);

   --  Open "vms_data.ads" to read the qualifiers' doc

   Open (File_In, In_File, VMS_Data_Ads.all);

   --  Initialize the state variables

   The_Command     := Undefined;
   Current_Command := Undefined;
   Status          := No_Command;

   --  Loop through all the commands

   Big_Loop : loop

      --  If there is text for the last command to be put after the qualifiers,
      --  write it now to the output file.

      if Current_Command /= Undefined then
         Current_Line := Post_Qualifiers (Current_Command);

         while Current_Line /= null loop
            Put_Line (Current_Line.Line.all);
            Current_Line := Current_Line.Next;
         end loop;
      end if;

      --  Find the next command

      Find_Command_Loop : loop
         Get_Line;
         exit Big_Loop when The_Line_Kind = EOF;
         exit Find_Command_Loop when The_Command /= Undefined;
      end loop Find_Command_Loop;

      --  We may return here in case of error

      <<New_Command>>
      Put_Line ("2  " & The_Command'Img);
      Current_Line := Pre_Qualifiers (The_Command);

      --  Write into the output file the text that precedes the qualifiers

      while Current_Line /= null loop
         Put_Line (Current_Line.Line.all);
         Current_Line := Current_Line.Next;
      end loop;

      --  Record the current command, to be used for the post qualifier lines
      --  and in case of errors.

      Current_Command := The_Command;

      --  Find the qualifiers

      Find_Qualifiers_Loop : loop
         Get_Line;
         exit Find_Qualifiers_Loop when The_Command = Undefined;

         if The_Line_Kind = Constant_S then
            --  We may return here if there is no empty line after a qualifier
            --  documentation, and in case of error.

            <<New_Qualifier>>
            if Status = First_Qualifier then
               Put_Line ("3  Qualifiers");
               New_Line (2);
            end if;

            --  Record the current qualifier, to be used in case of error

            Current_Qualifier_Last := Qualifier_Last;
            Current_Qualifier (1 .. Current_Qualifier_Last) :=
              Qualifier (1 .. Qualifier_Last);

            --  Skip the lines for the qualifier declaration

            Find_Doc_Loop : loop
               Get_Line;
               exit Find_Doc_Loop when The_Line_Kind /= Undefined;
            end loop Find_Doc_Loop;

            case The_Line_Kind is
               --  Should not happen

               when Undefined =>
                  null;

               --  If NODOC, there is nothing to do

               when No_Doc =>
                  null;

               --  If it is a comment, then it is the start of this qualifier
               --  documentation.

               when Comment =>
                  Put_Line (Qualifier (1 .. Qualifier_Last));
                  New_Line (1);

                  while The_Line_Kind = Comment loop
                     Put_Line (Line_In (6 .. Last_In));
                     Get_Line;
                  end loop;

                  New_Line (2);

                  if The_Line_Kind = Constant_S then
                     goto New_Qualifier;
                  end if;

               --  If we have either a new qualifier, a new command or
               --  the declaration of the switches for the command, there is
               --  no documentation and no indication of NODOC: it is an error.

               when others =>
                  Put_Line
                    (Standard_Output,
                     "No doc for qualifier " &
                     Current_Qualifier (1 .. Current_Qualifier_Last) &
                     " of command " &
                     Current_Command'Img);
                  Errors := True;

                  if The_Line_Kind = Switches_Command then
                     goto New_Command;

                  elsif The_Line_Kind = Constant_S then
                     goto New_Qualifier;

                  else
                     exit Find_Qualifiers_Loop;
                  end if;
            end case;
         end if;
      end loop Find_Qualifiers_Loop;
   end loop Big_Loop;

   Close (File_In);

   Set_Output (Standard_Output);
   Close (File_Out);

   for Com in Command_Type loop
      if not Visited (Com) then
         Put_Line ("*** ERROR: no switches for " & Com'Img);
         Errors := True;
      end if;
   end loop;

   if Errors then
      OS_Exit (1);
   end if;

exception
   when others =>
      Set_Output (Standard_Output);
      Put_Line ("Unexpected exception raised");
      raise;
end VMS_Help;
