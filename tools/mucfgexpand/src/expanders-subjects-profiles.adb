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

with DOM.Core.Elements;

with Mulog;
with Muxml.Utils;
with Mutools.XML_Utils;

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
        := Muxml.Utils.Get_Element
          (Doc   => Subject,
           XPath => "memory");
   begin
      Mulog.Log
        (Msg => "Adding Linux zero-page for subject '" & Subj_Name & "'");

      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => Subj_Name & "|zp",
         Address     => "",
         Size        => "16#2000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_zeropage",
         File_Name   => Subj_Name & "_zp",
         File_Offset => "none");
      Muxml.Utils.Append_Child
        (Node      => Subj_Mem_Node,
         New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "zero_page",
            Physical_Name => Subj_Name & "|zp",
            Address       => "16#0000#",
            Writable      => True,
            Executable    => False));

      Mulog.Log (Msg => "Adding info region for subject '" & Subj_Name & "'");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => Subj_Name & "|sinfo",
         Address     => "",
         Size        => "16#6000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_info",
         File_Name   => Subj_Name & "_sinfo",
         File_Offset => "none");
      Muxml.Utils.Append_Child
        (Node      => Subj_Mem_Node,
         New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "sinfo",
            Physical_Name => Subj_Name & "|sinfo",
            Address       => "16#0001_4000#",
            Writable      => False,
            Executable    => False));

      Mulog.Log (Msg => "Adding ACPI tables for subject '" & Subj_Name & "'");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => Subj_Name & "|acpi_rsdp",
         Address     => "",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_acpi_rsdp",
         File_Name   => Subj_Name & "_rsdp",
         File_Offset => "none");
      Muxml.Utils.Append_Child
        (Node      => Subj_Mem_Node,
         New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "acpi_rsdp",
            Physical_Name => Subj_Name & "|acpi_rsdp",
            Address       => "16#000e_0000#",
            Writable      => False,
            Executable    => False));
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => Subj_Name & "|acpi_xsdt",
         Address     => "",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_acpi_xsdt",
         File_Name   => Subj_Name & "_xsdt",
         File_Offset => "none");
      Muxml.Utils.Append_Child
        (Node      => Subj_Mem_Node,
         New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "acpi_xsdt",
            Physical_Name => Subj_Name & "|acpi_xsdt",
            Address       => "16#000e_1000#",
            Writable      => False,
            Executable    => False));
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => Subj_Name & "|acpi_fadt",
         Address     => "",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_acpi_fadt",
         File_Name   => Subj_Name & "_fadt",
         File_Offset => "none");
      Muxml.Utils.Append_Child
        (Node      => Subj_Mem_Node,
         New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "acpi_fadt",
            Physical_Name => Subj_Name & "|acpi_fadt",
            Address       => "16#000e_2000#",
            Writable      => False,
            Executable    => False));
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => Subj_Name & "|acpi_dsdt",
         Address     => "",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_acpi_dsdt",
         File_Name   => Subj_Name & "_dsdt.aml",
         File_Offset => "none");
      Muxml.Utils.Append_Child
        (Node      => Subj_Mem_Node,
         New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "acpi_dsdt",
            Physical_Name => Subj_Name & "|acpi_dsdt",
            Address       => "16#000e_3000#",
            Writable      => False,
            Executable    => False));
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => Subj_Name & "|acpi_empty",
         Address     => "",
         Size        => "16#c000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_bios");
      Muxml.Utils.Append_Child
        (Node      => Subj_Mem_Node,
         New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "acpi_free",
            Physical_Name => Subj_Name & "|acpi_empty",
            Address       => "16#000e_4000#",
            Writable      => False,
            Executable    => False));

      Mulog.Log (Msg => "Adding BIOS regions for subject '" & Subj_Name & "'");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => Subj_Name & "|bios",
         Address     => "",
         Size        => "16#0001_0000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_bios");
      Muxml.Utils.Append_Child
        (Node      => Subj_Mem_Node,
         New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "bios",
            Physical_Name => Subj_Name & "|bios",
            Address       => "16#000c_0000#",
            Writable      => False,
            Executable    => False));
      Muxml.Utils.Append_Child
        (Node      => Subj_Mem_Node,
         New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "bios",
            Physical_Name => Subj_Name & "|bios",
            Address       => "16#000d_0000#",
            Writable      => False,
            Executable    => False));
   end Handle_Linux_Profile;

end Expanders.Subjects.Profiles;
