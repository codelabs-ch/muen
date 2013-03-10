--
-- Copyright (c) 2008-2009 Tero Koskinen <tero.koskinen@iki.fi>
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
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;

with Ahven.Parameters;
with Ahven.AStrings;

package body Ahven.Tap_Runner is
   use Ada.Text_IO;
   use Ahven.Framework;
   use Ahven.AStrings;

   function Count_Image (Count : Test_Count_Type) return String is
      use Ada.Strings;
   begin
      return Fixed.Trim (Test_Count_Type'Image (Count), Both);
   end Count_Image;

   procedure Print_Data (Message : String; Prefix : String) is
      Start_Of_Line : Boolean := True;
   begin
      for I in Message'Range loop
         if Start_Of_Line then
            Put (Prefix);
            Start_Of_Line := False;
         end if;
         Put (Message (I));
         if Message (I) = Ada.Characters.Latin_1.LF then
            New_Line;
            Start_Of_Line := True;
         end if;
      end loop;
   end Print_Data;

   procedure Run (Suite : in out Framework.Test'Class) is
      Listener : Tap_Listener;
      Params   : Parameters.Parameter_Info;
   begin
      Parameters.Parse_Parameters (Parameters.TAP_PARAMETERS, Params);

      Listener.Verbose := Parameters.Verbose (Params);
      Listener.Capture_Output := Parameters.Capture (Params);

      if Parameters.Single_Test (Params) then
         Put_Line ("1.." & Count_Image (Test_Count
           (Suite, Parameters.Test_Name (Params))));
         Framework.Execute
           (T         => Suite,
            Test_Name => Parameters.Test_Name (Params),
            Listener  => Listener,
            Timeout   => Parameters.Timeout (Params));
      else
         Put_Line ("1.." & Count_Image (Test_Count (Suite)));
         Framework.Execute (Suite, Listener, Parameters.Timeout (Params));
      end if;
   exception
      when Parameters.Invalid_Parameter =>
         Parameters.Usage (Parameters.TAP_PARAMETERS);
   end Run;

   procedure Print_Info (Info : Context) is
   begin
      if Length (Info.Message) > 0 then
         Print_Data (Message => To_String (Info.Message), Prefix => "# ");
         New_Line;
      end if;
      if Length (Info.Long_Message) > 0 then
         Print_Data (Message => To_String (Info.Long_Message), Prefix => "# ");
         New_Line;
      end if;
   end Print_Info;

   procedure Print_Log_File (Filename : String; Prefix : String) is
      Handle : File_Type;
      Char   : Character := ' ';
      First  : Boolean := True;
      Start_Of_Line : Boolean := True;
   begin
      Open (Handle, In_File, Filename);
      loop
         exit when End_Of_File (Handle);
         Get (Handle, Char);
         if First then
            Put_Line (Prefix & "===== Output =======");
            First := False;
         end if;
         if Start_Of_Line then
            Put (Prefix);
            Start_Of_Line := False;
         end if;
         Put (Char);
         if End_Of_Line (Handle) then
            New_Line;
            Start_Of_Line := True;
         end if;
      end loop;
      Close (Handle);
      if not First then
         Put_Line (Prefix & "====================");
      end if;
   exception
      when Name_Error =>
         -- Missing output file is ok.
         Put_Line (Prefix & "no output");
   end Print_Log_File;

   procedure Add_Pass (Listener : in out Tap_Listener;
                       Info     :        Context) is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      if Listener.Capture_Output then
         Temporary_Output.Restore_Output;
         Temporary_Output.Close_Temp (Listener.Output_File);
      end if;

      Put ("ok ");
      Put (Count_Image (Listener.Current_Test) & " ");
      Put (To_String (Info.Test_Name) & ": " & To_String (Info.Routine_Name));
      New_Line;
   end Add_Pass;

   procedure Report_Not_Ok (Listener : in out Tap_Listener;
                            Info     :        Context;
                            Severity :        String) is
      pragma Unreferenced (Severity);

      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      if Listener.Capture_Output then
         Temporary_Output.Restore_Output;
         Temporary_Output.Close_Temp (Listener.Output_File);
      end if;

      Put ("not ok ");
      Put (Count_Image (Listener.Current_Test) & " ");
      Put (To_String (Info.Test_Name) & ": " & To_String (Info.Routine_Name));
      New_Line;

      if Listener.Verbose then
         Print_Info (Info);
         if Listener.Capture_Output then
            Print_Log_File
              (Filename => Temporary_Output.Get_Name (Listener.Output_File),
               Prefix   => "# ");
         end if;
      end if;
   end Report_Not_Ok;

   procedure Add_Failure (Listener : in out Tap_Listener;
                          Info     :        Context) is
   begin
      Report_Not_Ok (Listener, Info, "fail");
   end Add_Failure;

   procedure Add_Error (Listener : in out Tap_Listener;
                        Info     :        Context) is
   begin
      Report_Not_Ok (Listener, Info, "error");
   end Add_Error;

   procedure Add_Skipped (Listener : in out Tap_Listener;
                          Info     :        Context) is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      if Listener.Capture_Output then
         Temporary_Output.Restore_Output;
         Temporary_Output.Close_Temp (Listener.Output_File);
      end if;

      Put ("ok ");
      Put (Count_Image (Listener.Current_Test) & " ");
      Put (To_String (Info.Test_Name) & ": " & To_String (Info.Routine_Name));
      Put (" # SKIP " & To_String (Info.Message));
      New_Line;
   end Add_Skipped;

   procedure Start_Test (Listener : in out Tap_Listener;
                         Info     :        Context) is
   begin
      if Info.Test_Kind = ROUTINE then
         Listener.Current_Test := Listener.Current_Test + 1;
         if Listener.Capture_Output then
            Temporary_Output.Create_Temp (Listener.Output_File);
            Temporary_Output.Redirect_Output (Listener.Output_File);
         end if;
      end if;
   end Start_Test;

   procedure End_Test (Listener : in out Tap_Listener;
                       Info     :        Context) is
      pragma Unreferenced (Info);

      Handle : Ada.Text_IO.File_Type;
   begin
      if Listener.Capture_Output then
         Ada.Text_IO.Open (Handle, Ada.Text_IO.Out_File,
                           Temporary_Output.Get_Name (Listener.Output_File));
         Ada.Text_IO.Delete (Handle);
      end if;
   exception
      when Name_Error =>
         -- Missing file is safe to ignore, we are going to delete it anyway
         null;
   end End_Test;
end Ahven.Tap_Runner;
