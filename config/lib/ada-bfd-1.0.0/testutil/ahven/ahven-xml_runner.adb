--
-- Copyright (c) 2007-2009 Tero Koskinen <tero.koskinen@iki.fi>
--
-- Permission to use, copy, modify, and distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--

with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.IO_Exceptions;

with Ahven.Runner;

with Ahven_Compat;
with Ahven.AStrings;

package body Ahven.XML_Runner is
   use Ada.Text_IO;
   use Ada.Strings.Fixed;
   use Ada.Strings.Maps;

   use Ahven.Results;
   use Ahven.Framework;
   use Ahven.AStrings;

   function Filter_String (Str : String) return String;

   function Filter_String
     (Str : String;
      Map : Character_Mapping)
      return String;

   procedure Print_Test_Pass (File : File_Type;
                              Parent_Test : String;
                              Info : Result_Info);
   procedure Print_Test_Failure (File : File_Type;
                                 Parent_Test : String;
                                 Info : Result_Info);
   procedure Print_Test_Error (File : File_Type;
                               Parent_Test : String;
                               Info : Result_Info);

   procedure Print_Test_Case (Collection : Result_Collection;
                              Dir : String);

   procedure Print_Log_File (File : File_Type; Filename : String);

   procedure Print_Attribute (File : File_Type; Attr : String;
                              Value : String);

   procedure Start_Testcase_Tag (File : File_Type;
                                 Parent : String; Name : String;
                                 Execution_Time : String);

   procedure End_Testcase_Tag (File : File_Type);

   function Create_Name (Dir : String; Name : String) return String;

   function Filter_String (Str : String) return String is
      Result : String (Str'First .. Str'First + 6 * Str'Length);
      Pos    : Natural := Str'First;
   begin
      for I in Str'Range loop
         if Str (I) = ''' then
            Result (Pos .. Pos + 6 - 1) := "&apos;";
            Pos := Pos + 6;
         elsif Str (I) = '<' then
            Result (Pos .. Pos + 4 - 1) := "&lt;";
            Pos := Pos + 4;
         elsif Str (I) = '>' then
            Result (Pos .. Pos + 4 - 1) := "&gt;";
            Pos := Pos + 4;
         elsif Str (I) = '&' then
            Result (Pos .. Pos + 5 - 1) := "&amp;";
            Pos := Pos + 5;
         elsif Str (I) = '"' then
            Result (Pos) := ''';
            Pos := Pos + 1;
         else
            Result (Pos) := Str (I);
            Pos := Pos + 1;
         end if;
      end loop;

      return Result (Result'First .. Pos - 1);
   end Filter_String;

   function Filter_String
     (Str : String;
      Map : Character_Mapping)
      return String
   is
   begin
      return Translate (Source  => Str,
                        Mapping => Map);
   end Filter_String;

   procedure Print_Attribute (File : File_Type; Attr : String;
                              Value : String) is
   begin
      Put (File, Attr & "=" & '"' & Value & '"');
   end Print_Attribute;

   procedure Start_Testcase_Tag (File : File_Type;
                                 Parent : String; Name : String;
                                 Execution_Time : String) is
   begin
      Put (File, "<testcase ");
      Print_Attribute (File, "classname", Filter_String (Parent));
      Put (File, " ");
      Print_Attribute (File, "name", Filter_String (Name));
      Put (File, " ");
      Print_Attribute (File, "time", Filter_String (Execution_Time));
      Put_Line (File, ">");
   end Start_Testcase_Tag;

   procedure End_Testcase_Tag (File : File_Type) is
   begin
      Put_Line (File, "</testcase>");
   end End_Testcase_Tag;

   function Create_Name (Dir : String; Name : String) return String
   is
      function Filename (Test : String) return String is
         Map : Ada.Strings.Maps.Character_Mapping;
      begin
         Map := To_Mapping (From => " '/\<>:|?*()" & '"',
                            To   => "-___________" & '_');

         return "TEST-" & Filter_String (Test, Map) & ".xml";
      end Filename;
   begin
      if Dir'Length > 0 then
         return Dir & Ahven_Compat.Directory_Separator & Filename (Name);
      else
         return Filename (Name);
      end if;
   end Create_Name;

   procedure Print_Test_Pass (File : File_Type;
                              Parent_Test : String;
                              Info : Result_Info) is
      Exec_Time : constant String :=
        Trim (Duration'Image (Get_Execution_Time (Info)), Ada.Strings.Both);
   begin
      Start_Testcase_Tag
        (File           => File,
         Parent         => Parent_Test,
         Name           => Get_Routine_Name (Info),
         Execution_Time => Exec_Time);

      if Length (Get_Output_File (Info)) > 0 then
         Put (File, "<system-out>");
         Print_Log_File (File, To_String (Get_Output_File (Info)));
         Put_Line (File, "</system-out>");
      end if;
      End_Testcase_Tag (File);
   end Print_Test_Pass;

   procedure Print_Test_Skipped (File : File_Type;
                                 Parent_Test : String;
                                 Info : Result_Info) is
      Exec_Time : constant String :=
        Trim (Duration'Image (Get_Execution_Time (Info)), Ada.Strings.Both);
   begin
      Start_Testcase_Tag
        (File           => File,
         Parent         => Parent_Test,
         Name           => Get_Routine_Name (Info),
         Execution_Time => Exec_Time);

      Put (File, "<skipped ");

      Print_Attribute (File, "message",
                       Filter_String (Trim (Get_Message (Info), Ada.Strings.Both)));
      Put (File, ">");
      Put_Line (File, Filter_String (Get_Message (Info)));
      Put_Line (File, "</skipped>");
      End_Testcase_Tag (File);
   end Print_Test_Skipped;

   procedure Print_Test_Failure (File : File_Type;
                                 Parent_Test : String;
                                 Info : Result_Info) is
      Exec_Time : constant String :=
        Trim (Duration'Image (Get_Execution_Time (Info)), Ada.Strings.Both);
   begin
      Start_Testcase_Tag
        (File           => File,
         Parent         => Parent_Test,
         Name           => Get_Routine_Name (Info),
         Execution_Time => Exec_Time);

      Put (File, "<failure ");
      Print_Attribute (File, "type",
                       Filter_String (Trim (Get_Message (Info), Ada.Strings.Both)));
      Put (File, ">");
      Put_Line (File, Filter_String (Get_Message (Info)));
      Put_Line (File, "</failure>");
      if Length (Get_Output_File (Info)) > 0 then
         Put (File, "<system-out>");
         Print_Log_File (File, To_String (Get_Output_File (Info)));
         Put_Line (File, "</system-out>");
      end if;
      End_Testcase_Tag (File);
   end Print_Test_Failure;

   procedure Print_Test_Error (File : File_Type;
                               Parent_Test : String;
                               Info : Result_Info) is
      Exec_Time : constant String :=
        Trim (Duration'Image (Get_Execution_Time (Info)), Ada.Strings.Both);
   begin
      Start_Testcase_Tag
        (File           => File,
         Parent         => Parent_Test,
         Name           => Get_Routine_Name (Info),
         Execution_Time => Exec_Time);

      Put (File, "<error ");
      Print_Attribute (File, "type",
                       Filter_String (Trim (Get_Message (Info), Ada.Strings.Both)));
      Put (File, ">");
      Put_Line (File, Filter_String (Get_Message (Info)));
      Put_Line (File, "</error>");
      if Length (Get_Output_File (Info)) > 0 then
         Put (File, "<system-out>");
         Print_Log_File (File, To_String (Get_Output_File (Info)));
         Put_Line (File, "</system-out>");
      end if;
      End_Testcase_Tag (File);
   end Print_Test_Error;

   procedure Print_Test_Case (Collection : Result_Collection;
                              Dir : String) is
      procedure Print (Output : File_Type;
                       Result : Result_Collection);
      -- Internal procedure to print the testcase into given file.

      function Img (Value : Natural) return String is
      begin
         return Trim (Natural'Image (Value), Ada.Strings.Both);
      end Img;

      procedure Print (Output : File_Type;
                       Result : Result_Collection) is
         Position : Result_Info_Cursor;
      begin
         Put_Line (Output, "<?xml version=" & '"' & "1.0" & '"' &
                   " encoding=" & '"' & "iso-8859-1" & '"' &
                   "?>");
         Put (Output, "<testsuite ");
         Print_Attribute (Output, "errors", Img (Error_Count (Result)));
         Put (Output, " ");
         Print_Attribute (Output, "failures", Img (Failure_Count (Result)));
         Put (Output, " ");
         Print_Attribute (Output, "skips", Img (Skipped_Count (Result)));
         Put (Output, " ");
         Print_Attribute (Output, "tests", Img (Test_Count (Result)));
         Put (Output, " ");
         Print_Attribute (Output, "time",
           Trim (Duration'Image (Get_Execution_Time (Result)),
                 Ada.Strings.Both));
         Put (Output, " ");
         Print_Attribute (Output,
           "name", To_String (Get_Test_Name (Result)));
         Put_Line (Output, ">");

         Position := First_Error (Result);
         Error_Loop:
         loop
            exit Error_Loop when not Is_Valid (Position);
            Print_Test_Error (Output,
              To_String (Get_Test_Name (Result)), Data (Position));
            Position := Next (Position);
         end loop Error_Loop;

         Position := First_Failure (Result);
         Failure_Loop:
         loop
            exit Failure_Loop when not Is_Valid (Position);
            Print_Test_Failure (Output,
              To_String (Get_Test_Name (Result)), Data (Position));
            Position := Next (Position);
         end loop Failure_Loop;

         Position := First_Pass (Result);
         Pass_Loop:
         loop
            exit Pass_Loop when not Is_Valid (Position);
            Print_Test_Pass (Output,
              To_String (Get_Test_Name (Result)), Data (Position));
            Position := Next (Position);
         end loop Pass_Loop;

         Position := First_Skipped (Result);
         Skip_Loop:
         loop
            exit Skip_Loop when not Is_Valid (Position);

            Print_Test_Skipped (Output,
              To_String (Get_Test_Name (Result)), Data (Position));
            Position := Next (Position);
         end loop Skip_Loop;

         Put_Line (Output, "</testsuite>");
      end Print;

      File : File_Type;
   begin
      if Dir = "-" then
         Print (Standard_Output, Collection);
      else
         Create (File => File, Mode => Ada.Text_IO.Out_File,
           Name => Create_Name (Dir, To_String (Get_Test_Name (Collection))));
         Print (File, Collection);
         Ada.Text_IO.Close (File);
      end if;
   end Print_Test_Case;

   procedure Report_Results (Result : Result_Collection;
                             Dir    : String) is
      Position : Result_Collection_Cursor;
   begin
      Position := First_Child (Result);
      loop
         exit when not Is_Valid (Position);
         if Child_Depth (Data (Position).all) = 0 then
            Print_Test_Case (Data (Position).all, Dir);
         else
            Report_Results (Data (Position).all, Dir);

            -- Handle the test cases in this collection
            if Direct_Test_Count (Result) > 0 then
               Print_Test_Case (Result, Dir);
            end if;
         end if;
         Position := Next (Position);
      end loop;
   end Report_Results;

   -- Print the log by placing the data inside CDATA block.
   procedure Print_Log_File (File : File_Type; Filename : String) is
      type CData_End_State is (NONE, FIRST_BRACKET, SECOND_BRACKET);

      function State_Change (Old_State : CData_End_State)
        return CData_End_State;

      Handle : File_Type;
      Char   : Character := ' ';
      First  : Boolean := True;

      -- We need to escape ]]>, this variable tracks
      -- the characters, so we know when to do the escaping.
      CData_Ending : CData_End_State := NONE;

      function State_Change (Old_State : CData_End_State)
        return CData_End_State
      is
         New_State : CData_End_State := NONE;
         -- By default New_State will be NONE, so there is
         -- no need to set it inside when blocks.
      begin
         case Old_State is
            when NONE =>
               if Char = ']' then
                  New_State := FIRST_BRACKET;
               end if;
            when FIRST_BRACKET =>
               if Char = ']' then
                  New_State := SECOND_BRACKET;
               end if;
            when SECOND_BRACKET =>
               if Char = '>' then
                  Put (File, " ");
               end if;
         end case;
         return New_State;
      end State_Change;
   begin
      Open (Handle, In_File, Filename);
      begin
         loop
            exit when End_Of_File (Handle);
            Get (Handle, Char);
            if First then
               Put (File, "<![CDATA[");
               First := False;
            end if;
            CData_Ending := State_Change (CData_Ending);

            Put (File, Char);
            if End_Of_Line (Handle) then
               New_Line (File);
            end if;
         end loop;

         --  The End_Error exception is sometimes raised.
      exception
         when Ada.IO_Exceptions.End_Error =>
            null;
      end;

      Close (Handle);
      if not First then
         Put_Line (File, "]]>");
      end if;
   end Print_Log_File;

   procedure Do_Report (Test_Results : Results.Result_Collection;
                        Args         : Parameters.Parameter_Info) is
   begin
      Report_Results (Test_Results,
                      Parameters.Result_Dir (Args));
   end Do_Report;

   procedure Run (Suite : in out Framework.Test_Suite'Class) is
   begin
      Runner.Run_Suite (Suite, Do_Report'Access);
   end Run;

   procedure Run (Suite : Framework.Test_Suite_Access) is
   begin
      Run (Suite.all);
   end Run;
end Ahven.XML_Runner;
