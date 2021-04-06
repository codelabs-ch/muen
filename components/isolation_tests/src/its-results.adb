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

with Interfaces;

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
      Result            : Boolean;
      Log_Entry         : Log_Buffer.Ext_Log_Entries_Range;
   end record;

   Null_Test_Case : constant Test_Case_Type
     := (Title             => Null_String,
         Title_Len         => Bounded_String_Range'First,
         Description       => Null_String,
         Description_Len   => Bounded_String_Range'First,
         Expected_Desc     => Null_String,
         Expected_Desc_Len => Bounded_String_Range'First,
         Result            => False,
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
     (Title       : String;
      Description : String;
      Expected    : String;
      Success     : Boolean;
      Log_Entry   : Log_Buffer.Ext_Log_Entries_Range)
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
      New_Test.Result := Success;
      New_Test.Log_Entry := Log_Entry;
      Tests (Current_Test_Case) := New_Test;
   end Append;

   -------------------------------------------------------------------------

   procedure Report
   is

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

      procedure Print_Summary
      is
      begin
         Log.Put_Line ("Test Results");
         Log.Put_Line ("************");
         Log.New_Line;
         Log.Put ("Tests Total  : ");
         Put_No_Leading_Zeros
           (SK.Strings.Img_Dec (Interfaces.Unsigned_64 (Current_Test_Case)));
         Log.New_Line;
         Log.New_Line;
         Log.Put ("Tests Passed : ");
         Put_No_Leading_Zeros
           (SK.Strings.Img_Dec (Interfaces.Unsigned_64 (Success_Count)));
         Log.New_Line;
         Log.New_Line;
         Log.Put ("Tests Failed : ");
         Put_No_Leading_Zeros
           (SK.Strings.Img_Dec
              (Interfaces.Unsigned_64 (Current_Test_Case - Success_Count)));
         Log.New_Line;
         Log.New_Line;
      end Print_Summary;

      ----------------------------------------------------------------------

      procedure Print_Result (Test : Test_Case_Type)
      is
      begin
         Log.Put_Line
           (Test.Title (Test.Title'First .. Test.Title_Len));
         for I in Natural range 1 .. Test.Title_Len loop
            Log.Put ("=");
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

         Log.Put_Line ("   * - :blue:`Status`");
         Log.Put      ("     - ");
         if Test.Result then
            Log.Put_Line (":green:`Passed`");
         else
            Log.Put_Line (":red:`Failed`");
         end if;

         Log.Put_Line ("   * - :blue:`Output`");
         Log.Put_Line      ("     - ::");
         Log.New_Line;
         Log_Buffer.Print_Entry (ID => Test.Log_Entry);
         Log.New_Line;
         Log.New_Line;
      end Print_Result;
   begin
      Print_Summary;

      for I in Test_Cases_Range'First .. Current_Test_Case loop
         Print_Result (Test => Tests (I));
      end loop;
   end Report;

end ITS.Results;
