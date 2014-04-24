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

with DOM.Core.Elements;

with Muxml.Utils;
with Mutools.XML_Utils;

with Pack.Image;
with Pack.Manifest;
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
      T.Add_Test_Routine
        (Routine => Process_Fills'Access,
         Name    => "Process fills");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Process_Files
   is
      Policy : Muxml.XML_Data_Type;
      Data   : Content_Providers.Param_Type (16#126000#);
   begin
      Command_Line.Test.Set_Input_Dir  (Path => "data");
      Command_Line.Test.Set_Output_Dir (Path => "obj");
      Command_Line.Test.Set_Policy     (Path => "data/test_policy.xml");

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Policy,
         Name        => "mboot",
         Address     => "16#0010_0000#",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         File_Name   => "mboot",
         File_Format => "bin_raw",
         File_Offset => "none");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Policy,
         Name        => "linux|acpi_rsdp",
         Address     => "16#0010_1000#",
         Size        => "16#0001_3000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         File_Name   => "sections.ref",
         File_Format => "acpi_rsdp",
         File_Offset => "none");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Policy,
         Name        => "linux|bin",
         Address     => "16#0011_4000#",
         Size        => "16#0001_3000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         File_Name   => "obj1.o",
         File_Format => "bin_raw",
         File_Offset => "16#0004#");

      Data.XML_Doc := Policy.Doc;
      Content_Providers.Process_Files (Data => Data);

      Image.Write (Image    => Data.Image,
                   Filename => "obj/process_files.img");
      Manifest.Write (Manifest => Data.Manifest,
                      Filename => "obj/process_files.manifest");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/process_files.img",
               Filename2 => "data/process_files.img"),
              Message   => "Image file differs");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/process_files.manifest",
               Filename2 => "data/process_files.manifest"),
              Message   => "Manifest file differs");

      Ada.Directories.Delete_File (Name => "obj/process_files.img");
      Ada.Directories.Delete_File (Name => "obj/process_files.manifest");
   end Process_Files;

   -------------------------------------------------------------------------

   procedure Process_Fills
   is
      Policy : Muxml.XML_Data_Type;
      Data   : Content_Providers.Param_Type (9);
   begin
      Command_Line.Test.Set_Input_Dir (Path => "data");
      Command_Line.Test.Set_Output_Dir (Path => "obj");
      Command_Line.Test.Set_Policy (Path => "data/test_policy.xml");
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/memory/memory[@name='filled']");
      begin

         --  Set size and address of memory region.

         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "physicalAddress",
            Value => "16#0000#");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "size",
            Value => "16#000a#");

         Data.XML_Doc := Policy.Doc;

         Content_Providers.Process_Fills (Data => Data);

         Image.Write (Image    => Data.Image,
                      Filename => "obj/fill.img");
         Manifest.Write (Manifest => Data.Manifest,
                         Filename => "obj/manifest");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "obj/fill.img",
                  Filename2 => "data/img.offset.ref"),
                 Message   => "Image file differs");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "obj/manifest",
                  Filename2 => "data/manifest.fill.ref"),
                 Message   => "Manifest file differs");

         Ada.Directories.Delete_File (Name => "obj/fill.img");
         Ada.Directories.Delete_File (Name => "obj/manifest");
      end;
   end Process_Fills;

end Content_Provider_Tests;
