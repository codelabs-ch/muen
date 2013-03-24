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
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;
with Ada.IO_Exceptions;

with Ahven.Runner;
with Ahven.XML_Runner;
with Ahven.AStrings;

use Ada.Text_IO;
use Ada.Strings.Fixed;

package body Ahven.Text_Runner is
   use Ahven.Results;
   use Ahven.Framework;
   use Ahven.AStrings;

   -- Local procedures
   procedure Pad (Level : Natural);

   procedure Print_Test (Info   : Result_Info;
                         Level  : Natural;
                         Result : String);

   procedure Print_Passes (Result : Result_Collection;
                           Level  : Natural);

   procedure Report_Results (Result  : Result_Collection;
                             Verbose : Boolean := False);

   procedure Print_Log_File (Filename : String);

   procedure Pad (Level : Natural) is
   begin
      for A in Integer range 1 .. Level loop
         Put (" ");
      end loop;
   end Pad;

   procedure Pad (Amount : in     Natural;
                  Total  : in out Natural) is
   begin
      for A in Natural range 1 .. Amount loop
         Put (" ");
      end loop;
      Total := Total + Amount;
   end Pad;

   procedure Multiline_Pad (Input  : String;
                            Level  : Natural) is
   begin
      Pad (Level);
      for A in Input'Range loop
         Put (Input (A));
         if (Input (A) = Ada.Characters.Latin_1.LF) and (A /= Input'Last) then
            Pad (Level);
         end if;
      end loop;
   end Multiline_Pad;

   procedure Print_Test (Info   : Result_Info;
                         Level  : Natural;
                         Result : String) is
      use Ada.Strings;

      Max_Output_Width : constant := 50;
      Max_Result_Width : constant := 7;
      Max_Time_Out_Width : constant := 12;

      subtype Result_Size is Integer range 1 .. Max_Result_Width;
      subtype Time_Out_Size is Integer range 1 .. Max_Time_Out_Width;

      procedure Print_Text (Str : String; Total : in out Natural) is
      begin
         Put (Str);
         Total := Total + Str'Length;
      end Print_Text;

      Msg        : constant String := Get_Message (Info);
      Result_Out : String (Result_Size) := (others => ' ');
      Time_Out   : String (Time_Out_Size) := (others => ' ');
      Total_Text : Natural := 0;

   begin
      Pad (Level + 1, Total_Text);
      Print_Text (Get_Routine_Name (Info), Total_Text);
      if Msg'Length > 0 then
         Print_Text (" - ", Total_Text);
         Print_Text (Msg, Total_Text);
      end if;

      if Total_Text < Max_Output_Width then
         Pad (Max_Output_Width - Total_Text, Total_Text);
      end if;

      -- If we know the name of the routine, we print it,
      -- the result, and the execution time.
      if Get_Routine_Name (Info)'Length > 0 then
         Move (Source  => Result,
               Target  => Result_Out,
               Drop    => Right,
               Justify => Left,
               Pad     => ' ');
         Move (Source  => Duration'Image (Get_Execution_Time (Info)),
               Target  => Time_Out,
               Drop    => Right,
               Justify => Right,
               Pad     => ' ');
         Put (" " & Result_Out);
         Put (" " & Time_Out & "s");
      end if;
      if Get_Long_Message (Info)'Length > 0 then
         New_Line;
         Multiline_Pad (Get_Long_Message (Info), Level + 2);
      end if;

      New_Line;
   end Print_Test;

   type Print_Child_Proc is access
     procedure (Result : Result_Collection; Level : Natural);

   type Child_Count_Proc is access
     function (Result : Result_Collection) return Natural;

   procedure Print_Children (Result : Result_Collection;
                             Level  : Natural;
                             Action : Print_Child_Proc;
                             Count  : Child_Count_Proc)
   is
      Child_Iter : Result_Collection_Cursor := First_Child (Result);
   begin
      loop
         exit when not Is_Valid (Child_Iter);
         if Count.all (Data (Child_Iter).all) > 0 then
            Action.all (Data (Child_Iter).all, Level + 1);
         end if;
         Child_Iter := Next (Child_Iter);
      end loop;
   end Print_Children;

   procedure Print_Statuses (Result    : Result_Collection;
                             Level     : Natural;
                             Start     : Result_Info_Cursor;
                             Action    : Print_Child_Proc;
                             Status    : String;
                             Count     : Child_Count_Proc;
                             Print_Log : Boolean) is
      Position : Result_Info_Cursor := Start;
   begin
      if Length (Get_Test_Name (Result)) > 0 then
         Pad (Level);
         Put_Line (To_String (Get_Test_Name (Result)) & ":");
      end if;

      Test_Loop :
      loop
         exit Test_Loop when not Is_Valid (Position);
         Print_Test (Data (Position), Level, Status);
         if Print_Log and
           (Length (Get_Output_File (Data (Position))) > 0) then
            Print_Log_File (To_String (Get_Output_File (Data (Position))));
         end if;
         Position := Next (Position);
      end loop Test_Loop;

      Print_Children (Result => Result,
                      Level  => Level,
                      Action => Action,
                      Count  => Count);
   end Print_Statuses;

   --
   -- Print all failures from the result collection
   -- and then recurse into child collections.
   --
   procedure Print_Failures (Result : Result_Collection;
                             Level  : Natural) is
   begin
      Print_Statuses
        (Result    => Result,
         Level     => Level,
         Start     => First_Failure (Result),
         Action    => Print_Failures'Access,
         Status    => "FAIL",
         Count     => Failure_Count'Access,
         Print_Log => True);
   end Print_Failures;

   --
   -- Print all skips from the result collection
   -- and then recurse into child collections.
   --
   procedure Print_Skips (Result : Result_Collection;
                          Level  : Natural) is
   begin
      Print_Statuses
        (Result    => Result,
         Level     => Level,
         Start     => First_Skipped (Result),
         Action    => Print_Skips'Access,
         Status    => "SKIPPED",
         Count     => Skipped_Count'Access,
         Print_Log => True);
   end Print_Skips;

   --
   -- Print all errors from the result collection
   -- and then recurse into child collections.
   --
   procedure Print_Errors (Result : Result_Collection;
                           Level  : Natural) is
   begin
      Print_Statuses
        (Result    => Result,
         Level     => Level,
         Start     => First_Error (Result),
         Action    => Print_Errors'Access,
         Status    => "ERROR",
         Count     => Error_Count'Access,
         Print_Log => True);
   end Print_Errors;

   --
   -- Print all passes from the result collection
   -- and then recurse into child collections.
   --
   procedure Print_Passes (Result : Result_Collection;
                           Level  : Natural) is
   begin
      Print_Statuses
        (Result    => Result,
         Level     => Level,
         Start     => First_Pass (Result),
         Action    => Print_Passes'Access,
         Status    => "PASS",
         Count     => Pass_Count'Access,
         Print_Log => False);
   end Print_Passes;

   --
   -- Report passes, skips, failures, and errors from the result collection.
   procedure Report_Results (Result  : Result_Collection;
                             Verbose : Boolean := False) is
   begin
      Put_Line ("Passed : " & Integer'Image (Pass_Count (Result)));
      if Verbose then
         Print_Passes (Result, 0);
      end if;
      New_Line;
      if Skipped_Count (Result) > 0 then
         Put_Line ("Skipped : " & Integer'Image (Skipped_Count (Result)));
         Print_Skips (Result, 0);
         New_Line;
      end if;
      if Failure_Count (Result) > 0 then
         Put_Line ("Failed : " & Integer'Image (Failure_Count (Result)));
         Print_Failures (Result, 0);
         New_Line;
      end if;
      if Error_Count (Result) > 0 then
         Put_Line ("Errors : " & Integer'Image (Error_Count (Result)));
         Print_Errors (Result, 0);
      end if;
   end Report_Results;

   procedure Print_Log_File (Filename : String) is
      Handle : File_Type;
      Char   : Character := ' ';
      First  : Boolean := True;
   begin
      Open (Handle, In_File, Filename);
      begin
         loop
            exit when End_Of_File (Handle);
            Get (Handle, Char);
            if First then
               Put_Line ("===== Output =======");
               First := False;
            end if;
            Put (Char);
            if End_Of_Line (Handle) then
               New_Line;
            end if;

         end loop;

         --  The End_Error exception is sometimes raised.
      exception
         when Ada.IO_Exceptions.End_Error =>
            null;

      end;
      Close (Handle);
      if not First then
         Put_Line ("====================");
      end if;
   end Print_Log_File;

   procedure Do_Report (Test_Results : Results.Result_Collection;
                        Args         : Parameters.Parameter_Info) is
   begin
      if Parameters.XML_Results (Args) then
         XML_Runner.Report_Results
           (Test_Results, Parameters.Result_Dir (Args));
      else
         Report_Results (Test_Results, Parameters.Verbose (Args));
      end if;
   end Do_Report;

   procedure Run (Suite : in out Framework.Test'Class) is
   begin
      Runner.Run_Suite (Suite, Do_Report'Access);
   end Run;

   procedure Run (Suite : Framework.Test_Suite_Access) is
   begin
      Run (Suite.all);
   end Run;
end Ahven.Text_Runner;
