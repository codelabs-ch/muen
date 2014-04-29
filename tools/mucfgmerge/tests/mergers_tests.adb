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

with DOM.Core.Nodes;

with Muxml.Utils;

with Mergers;

with Test_Utils;

package body Mergers_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Merger tests");
      T.Add_Test_Routine
        (Routine => Merge_Platform'Access,
         Name    => "Merge platform");
      T.Add_Test_Routine
        (Routine => Merge_Platform_Null'Access,
         Name    => "Merge platform (null)");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Merge_Platform
   is
      Filename     : constant String := "obj/merged_platform.xml";
      Ref_Filename : constant String := "data/merged_platform.ref.xml";

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Mergers.Merge_Platform (Policy        => Policy,
                              Platform_File => "data/platform.xml");
      Muxml.Write (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => Filename);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Filename,
               Filename2 => Ref_Filename),
              Message   => "Policy mismatch");

      Ada.Directories.Delete_File (Name => Filename);
   end Merge_Platform;

   -------------------------------------------------------------------------

   procedure Merge_Platform_Null
   is
      Filename     : constant String := "obj/merged_platform_null.xml";
      Ref_Filename : constant String := "data/merged_platform_null.ref.xml";

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Remove_Child
        (Node       => DOM.Core.Nodes.First_Child (N => Policy.Doc),
         Child_Name => "platform");

      Mergers.Merge_Platform (Policy        => Policy,
                              Platform_File => "data/platform.xml");
      Muxml.Write (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => Filename);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Filename,
               Filename2 => Ref_Filename),
              Message   => "Policy mismatch");

      Ada.Directories.Delete_File (Name => Filename);
   end Merge_Platform_Null;

end Mergers_Tests;
