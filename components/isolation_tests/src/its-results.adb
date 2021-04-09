--
--  Copyright (C) 2021  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2021  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with SK.Strings;

with Debuglog.Client;

package body ITS.Results
is

   package Log renames Debuglog.Client;

   Max_Tests : constant Natural := 32;

   type Test_Case_Type is record
      Title             : Bounded_String;
      Title_Len         : Bounded_String_Range;
      Description       : Bounded_String;
      Description_Len   : Bounded_String_Range;
      Expected_Desc     : Bounded_String;
      Expected_Desc_Len : Bounded_String_Range;
      Source_Info       : Bounded_String;
      Source_Info_Len   : Bounded_String_Range;
      Testsuite         : Bounded_String;
      Testsuite_Len     : Bounded_String_Range;
      Result            : Boolean;
      Start_Timestamp   : Interfaces.Unsigned_64;
      End_Timestamp     : Interfaces.Unsigned_64;
      Log_Entry         : Log_Buffer.Ext_Log_Entries_Range;
   end record;

   Null_Test_Case : constant Test_Case_Type
     := (Title             => Null_String,
         Title_Len         => Bounded_String_Range'First,
         Description       => Null_String,
         Description_Len   => Bounded_String_Range'First,
         Expected_Desc     => Null_String,
         Expected_Desc_Len => Bounded_String_Range'First,
         Source_Info       => Null_String,
         Source_Info_Len   => Bounded_String_Range'First,
         Testsuite         => Null_String,
         Testsuite_Len     => Bounded_String_Range'First,
         Result            => False,
         Start_Timestamp   => 0,
         End_Timestamp     => 0,
         Log_Entry         => Log_Buffer.Null_Entry_Index);

   type Ext_Test_Cases_Range is new Natural range 0 .. Max_Tests;
   subtype Test_Cases_Range is Ext_Test_Cases_Range range
     Ext_Test_Cases_Range'First + 1 .. Ext_Test_Cases_Range'Last;

   type Test_Suite_Type is array (Test_Cases_Range) of Test_Case_Type;

   Tests : Test_Suite_Type := (others => Null_Test_Case);

   Current_Test_Case : Ext_Test_Cases_Range := Ext_Test_Cases_Range'First;
   Success_Count     : Ext_Test_Cases_Range := Ext_Test_Cases_Range'First;

   -------------------------------------------------------------------------

   procedure Append
     (Title           : String;
      Description     : String;
      Expected        : String;
      Source_Info     : String;
      Testsuite       : String;
      Success         : Boolean;
      Start_Timestamp : Interfaces.Unsigned_64;
      End_Timestamp   : Interfaces.Unsigned_64;
      Log_Entry       : Log_Buffer.Ext_Log_Entries_Range)
   is
      New_Test : Test_Case_Type;
   begin
      if Current_Test_Case < Test_Cases_Range'Last then
         Current_Test_Case := Current_Test_Case + 1;
      end if;
      if Success then
         Success_Count := Success_Count + 1;
      end if;

      New_Test.Title
        (New_Test.Title'First .. New_Test.Title'First + Title'Length - 1)
        := Title;
      New_Test.Title_Len := Title'Length;
      New_Test.Description
        (New_Test.Description'First ..
           New_Test.Description'First + Description'Length - 1)
          := Description;
      New_Test.Description_Len := Description'Length;
      New_Test.Expected_Desc
        (New_Test.Expected_Desc'First ..
           New_Test.Expected_Desc'First + Expected'Length - 1)
          := Expected;
      New_Test.Expected_Desc_Len := Expected'Length;
      New_Test.Source_Info
        (New_Test.Source_Info'First ..
           New_Test.Source_Info'First + Source_Info'Length - 1)
          := Source_Info;
      New_Test.Source_Info_Len := Source_Info'Length;
      New_Test.Testsuite
        (New_Test.Testsuite'First ..
           New_Test.Testsuite'First + Testsuite'Length - 1)
          := Testsuite;
      New_Test.Testsuite_Len := Testsuite'Length;
      New_Test.Result := Success;
      New_Test.Start_Timestamp := Start_Timestamp;
      New_Test.End_Timestamp := End_Timestamp;
      New_Test.Log_Entry := Log_Entry;
      Tests (Current_Test_Case) := New_Test;
   end Append;

   -------------------------------------------------------------------------

   procedure Report
   is

      Cur_Testsuite     : Bounded_String       := Null_String;
      Cur_Testsuite_Len : Bounded_String_Range := Bounded_String_Range'First;

      ----------------------------------------------------------------------

      function To_Millis
        (Diff : Interfaces.Unsigned_64)
         return Interfaces.Unsigned_64
      is
         Khz : constant Interfaces.Unsigned_64 := Musinfo.Instance.TSC_Khz;
      begin
         return Diff / Khz;
      end To_Millis;

      ----------------------------------------------------------------------

      procedure Put_No_Leading_Zeros (Str : String)
      is
         Leading : Boolean := True;
      begin
         for I in Str'Range loop
            if Str (I) /= '0' then
               Leading := False;
            end if;

            if not Leading or else I = Str'Last then
               Log.Put (Str (I));
            end if;
         end loop;
      end Put_No_Leading_Zeros;

      ----------------------------------------------------------------------

      procedure Print_Build_Info
      is
      begin
         Log.Put_Line ("Build Information");
         Log.Put_Line ("*****************");
         Log.New_Line;
         Log.Put_Line (".. list-table::");
         Log.Put_Line ("   :widths: 20 80");
         Log.New_Line;
         Log.Put_Line ("   * - :blue:`Key`");
         Log.Put      ("     - :blue:`Value`");
         Log.New_Line;
         Log.Put_Line ("   * - Compiler");
         Log.Put      ("     - ");
         Log.Put_Line (Standard'Compiler_Version);
         Log.Put_Line ("   * - Compilation Date");
         Log.Put      ("     - ");
         Log.Put_Line (Compilation_ISO_Date);
         Log.Put_Line ("   * - Compilation Time");
         Log.Put      ("     - ");
         Log.Put_Line (Compilation_Time);
         Log.New_Line;
      end Print_Build_Info;

      ----------------------------------------------------------------------

      procedure Print_Summary
      is
         Note_SDM_Ref : constant String
           := "   Some test cases/results contain references to the Intel "
           & "Software Developer's Manual (SDM). The referenced document "
           & "version is May 2019.";
      begin
         Log.Put_Line ("Test Results");
         Log.Put_Line ("************");
         Log.New_Line;
         Log.Put_Line (".. list-table::");
         Log.Put_Line ("   :widths: 20 80");
         Log.New_Line;
         Log.Put_Line ("   * - :blue:`Tests Total`");
         Log.Put      ("     - ");
         Put_No_Leading_Zeros
           (SK.Strings.Img_Dec (Interfaces.Unsigned_64 (Current_Test_Case)));
         Log.New_Line;
         Log.Put_Line ("   * - :green:`Tests Passed`");
         Log.Put      ("     - ");
         Put_No_Leading_Zeros
           (SK.Strings.Img_Dec (Interfaces.Unsigned_64 (Success_Count)));
         Log.New_Line;
         Log.Put_Line ("   * - :red:`Tests Failed`");
         Log.Put      ("     - ");
         Put_No_Leading_Zeros
           (SK.Strings.Img_Dec
              (Interfaces.Unsigned_64 (Current_Test_Case - Success_Count)));
         Log.New_Line;
         Log.New_Line;
         if Current_Test_Case = Success_Count then
            Log.Put_Line ("**All isolation tests PASSED.**");
         else
            Log.Put_Line ("**Some isolation tests FAILED.**");
         end if;
         Log.New_Line;

         Log.Put_Line (".. note::");
         Log.Put_Line (Note_SDM_Ref);
         Log.New_Line;
         Log.New_Line;
      end Print_Summary;

      ----------------------------------------------------------------------

      procedure Print_Result (Test : Test_Case_Type)
      is
      begin
         if Test.Testsuite_Len /= Cur_Testsuite_Len
           or else Test.Testsuite (Test.Testsuite'First .. Test.Testsuite_Len)
           /= Cur_Testsuite (Cur_Testsuite'First .. Cur_Testsuite_Len)
         then
            Cur_Testsuite_Len := Test.Testsuite_Len;
            Cur_Testsuite := Test.Testsuite;
            Log.Put_Line
              (Cur_Testsuite (Cur_Testsuite'First .. Cur_Testsuite_Len));
            for I in Natural range 1 .. Cur_Testsuite_Len loop
               Log.Put ("=");
            end loop;
            Log.New_Line;
            Log.New_Line;
         end if;

         Log.Put_Line
           (Test.Title (Test.Title'First .. Test.Title_Len));
         for I in Natural range 1 .. Test.Title_Len loop
            Log.Put ("-");
         end loop;
         Log.New_Line;

         Log.Put_Line (".. list-table::");
         Log.Put_Line ("   :widths: 20 80");
         Log.New_Line;

         Log.Put_Line ("   * - :blue:`Description`");
         Log.Put      ("     - ");
         Log.Put_Line
           (Test.Description
              (Test.Description'First .. Test.Description_Len));

         Log.Put_Line ("   * - :blue:`Expected`");
         Log.Put      ("     - ");
         Log.Put_Line
           (Test.Expected_Desc
              (Test.Expected_Desc'First .. Test.Expected_Desc_Len));

         Log.Put_Line ("   * - :blue:`Duration`");
         Log.Put      ("     - ");
         Put_No_Leading_Zeros
           (Str => SK.Strings.Img_Dec
              (Item => To_Millis
                   (Diff => Test.End_Timestamp - Test.Start_Timestamp)));
         Log.Put_Line (" ms");

         Log.Put_Line ("   * - :blue:`Source`");
         Log.Put      ("     - ");
         Log.Put_Line
           (Test.Source_Info (Test.Source_Info'First .. Test.Source_Info_Len));

         Log.Put_Line ("   * - :blue:`Status`");
         Log.Put      ("     - ");
         if Test.Result then
            Log.Put_Line (":green:`Passed`");
         else
            Log.Put_Line (":red:`Failed`");
         end if;
         Log.New_Line;

         Log.Put_Line ("**Test Log**");
         Log.New_Line;
         Log.Put_Line (".. code-block:: none");
         Log.Put_Line ("   :linenos:");
         Log.New_Line;
         Log_Buffer.Print_Entry (ID     => Test.Log_Entry,
                                 Prefix => "   ");
         Log.New_Line;
         Log.New_Line;
      end Print_Result;
   begin
      Print_Build_Info;
      Print_Summary;

      for I in Test_Cases_Range'First .. Current_Test_Case loop
         Print_Result (Test => Tests (I));
      end loop;
   end Report;

end ITS.Results;
