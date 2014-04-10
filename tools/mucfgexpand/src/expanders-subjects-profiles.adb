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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;

with Expanders.XML_Utils;

package body Expanders.Subjects.Profiles
is

   -------------------------------------------------------------------------

   procedure Handle_Linux_Profile
     (Data    : in out Muxml.XML_Data_Type;
      Subject :        DOM.Core.Node)
   is
      Subj_Name : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Subject,
           Name => "name");
      Subj_Mem_Node : constant DOM.Core.Node
        := DOM.Core.Nodes.Item
          (List  => McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "memory"),
           Index => 0);
   begin
      Mulog.Log
        (Msg => "Adding Linux zero-page for subject '" & Subj_Name & "'");

      XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => Subj_Name & "|zp",
         Address     => "",
         Size        => "16#2000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         File_Name   => Subj_Name & "_zp",
         File_Format => "zp",
         File_Offset => "none");
      Muxml.Utils.Append_Child
        (Node      => Subj_Mem_Node,
         New_Child => XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "zero_page",
            Physical_Name => Subj_Name & "|zp",
            Address       => "16#0000#",
            Writable      => True,
            Executable    => False));

      Mulog.Log (Msg => "Adding ACPI tables for subject '" & Subj_Name & "'");
      XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => Subj_Name & "|acpi_rsdp",
         Address     => "",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         File_Name   => Subj_Name & "_rsdp",
         File_Format => "acpi_rsdp",
         File_Offset => "none");
      Muxml.Utils.Append_Child
        (Node      => Subj_Mem_Node,
         New_Child => XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "acpi_rsdp",
            Physical_Name => Subj_Name & "|acpi_rsdp",
            Address       => "16#000e_0000#",
            Writable      => False,
            Executable    => False));
      XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => Subj_Name & "|acpi_xsdt",
         Address     => "",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         File_Name   => Subj_Name & "_xsdt",
         File_Format => "acpi_xsdt",
         File_Offset => "none");
      Muxml.Utils.Append_Child
        (Node      => Subj_Mem_Node,
         New_Child => XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "acpi_xsdt",
            Physical_Name => Subj_Name & "|acpi_xsdt",
            Address       => "16#000e_1000#",
            Writable      => False,
            Executable    => False));
      XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => Subj_Name & "|acpi_fadt",
         Address     => "",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         File_Name   => Subj_Name & "_fadt",
         File_Format => "acpi_fadt",
         File_Offset => "none");
      Muxml.Utils.Append_Child
        (Node      => Subj_Mem_Node,
         New_Child => XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "acpi_fadt",
            Physical_Name => Subj_Name & "|acpi_fadt",
            Address       => "16#000e_2000#",
            Writable      => False,
            Executable    => False));
   end Handle_Linux_Profile;

end Expanders.Subjects.Profiles;
