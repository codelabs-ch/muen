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

with DOM.Core.Documents;

with Muxml.Utils;

with Mucfgvcpu;

with Test_Utils;

package body Mucfgvcpu_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Mucfgvcpu tests");
      T.Add_Test_Routine
        (Routine => Set_VCPU_Profile'Access,
         Name    => "Set VCPU profile");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Set_VCPU_Profile
   is
      Data : Muxml.XML_Data_Type;
      Impl : DOM.Core.DOM_Implementation;
      Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document
        (Implementation => Impl);
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "vcpu");
      Muxml.Utils.Append_Child
        (Node      => Data.Doc,
         New_Child => Node);
      Mucfgvcpu.Set_VCPU_Profile
        (Profile => Mucfgvcpu.Linux,
         Node    => Node);

      Muxml.Write (Data => Data,
                   Kind => Muxml.VCPU_Profile,
                   File => "obj/vcpu_profile_linux.xml");

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/vcpu_profile_linux.xml",
               Filename2 => "obj/vcpu_profile_linux.xml"),
              Message   => "VCPU profile differs");
      Ada.Directories.Delete_File (Name => "obj/vcpu_profile_linux.xml");
   end Set_VCPU_Profile;

end Mucfgvcpu_Tests;
