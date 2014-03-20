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
with Ada.Strings.Unbounded;

with Muxml;

with Pack.Image;
with Pack.Command_Line.Test;
with Pack.Content_Providers;

with Test_Utils;

package body Content_Provider_Tests
is

   use Ahven;
   use Pack;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Content provider tests");
      T.Add_Test_Routine
        (Routine => Process_Files'Access,
         Name    => "Process files");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Process_Files
   is
      Data : Content_Providers.Param_Type (16#126000#);
   begin
      Command_Line.Test.Set_Input_Dir  (Path => "data");
      Command_Line.Test.Set_Output_Dir (Path => "obj");
      Command_Line.Test.Set_Policy     (Path => "data/test_policy.xml");
      Muxml.Parse (Data => Data.XML_Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Data.Mmap_File := Ada.Strings.Unbounded.To_Unbounded_String
        (Source => "obj/mmap-testfile");

      Content_Providers.Process_Files (Data => Data);

      Image.Write (Image    => Data.Image,
                   Filename => "obj/muen.img");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/muen.img",
               Filename2 => "data/muen.img.ref"),
              Message   => "Image file differs");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/mmap-testfile",
               Filename2 => "data/mmap.ref"),
              Message   => "Memory map file differs");

      Ada.Directories.Delete_File (Name => "obj/muen.img");
      Ada.Directories.Delete_File (Name => "obj/mmap-testfile");
   end Process_Files;

end Content_Provider_Tests;
