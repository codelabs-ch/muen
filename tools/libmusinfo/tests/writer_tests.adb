--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Directories;

with Interfaces;

with Musinfo.Utils;
with Musinfo.Writer;

with Test_Utils;

package body Writer_Tests
is

   use Ahven;
   use Musinfo;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Writer tests");
      T.Add_Test_Routine
        (Routine => Serialize'Access,
         Name    => "Subject info serialization");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Serialize
   is
      Info : Subject_Info_Type := Null_Subject_Info;
   begin
      Writer.Serialize
        (Info     => Info,
         Filename => "obj/null_info");

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/null_info",
               Filename2 => "obj/null_info"),
              Message   => "Null info mismatch");

      Utils.Append_Channel
        (Info    => Info,
         Channel => Utils.Create_Channel
           (Name       => Utils.Create_Name (Str => "channel1"),
            Address    => 0,
            Size       => 16#1000#,
            Writable   => False,
            Has_Event  => False,
            Has_Vector => False,
            Event      => 0,
            Vector     => 0));
      Utils.Append_Channel
        (Info    => Info,
         Channel => Utils.Create_Channel
           (Name       => Utils.Create_Name (Str => "channel2"),
            Address    => Interfaces.Unsigned_64'Last,
            Size       => Interfaces.Unsigned_64'Last,
            Writable   => False,
            Has_Event  => False,
            Has_Vector => True,
            Event      => 0,
            Vector     => 255));
      Utils.Append_Channel
        (Info    => Info,
         Channel => Utils.Create_Channel
           (Name       => Utils.Create_Name (Str => "channel3"),
            Address    => 16#beef_cafe_8080_1111#,
            Size       => 16#dead_beef_cafe_4321#,
            Writable   => True,
            Has_Event  => True,
            Has_Vector => False,
            Event      => 1,
            Vector     => 0));

      Writer.Serialize
        (Info     => Info,
         Filename => "obj/subject_info");

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/subject_info",
               Filename2 => "obj/subject_info"),
              Message   => "Subject info mismatch");

      Ada.Directories.Delete_File (Name => "obj/null_info");
      Ada.Directories.Delete_File (Name => "obj/subject_info");
   end Serialize;

end Writer_Tests;
