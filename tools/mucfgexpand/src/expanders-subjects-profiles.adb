--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Fixed;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;

with Mulog;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.XML_Utils;

with Expanders.Subjects.Config;
with Expanders.XML_Utils;

package body Expanders.Subjects.Profiles
is

   -------------------------------------------------------------------------

   procedure Handle_Linux_Profile
     (Data    : in out Muxml.XML_Data_Type;
      Subject :        DOM.Core.Node)
   is
      use type DOM.Core.Node;

      Subj_Name : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Subject,
           Name => "name");
      Subj_Mem_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Subject,
           XPath => "memory");
      Boot_Params_Node : DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Subject,
           XPath => "bootparams");
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

      declare
         use type Interfaces.Unsigned_64;

         BIOS_Region_Size   : constant Interfaces.Unsigned_64
           := 16#0001_0000#;
         Lo_BIOS_Addr_Start : constant Interfaces.Unsigned_64
           := 16#000c_0000#;
         Hi_BIOS_Addr_Start : constant Interfaces.Unsigned_64
           := Lo_BIOS_Addr_Start + BIOS_Region_Size;

         Map_Low_BIOS : constant Boolean
           := XML_Utils.Is_Free_To_Map
             (Subject         => Subject,
              Virtual_Address => Lo_BIOS_Addr_Start,
              Region_Size     => BIOS_Region_Size);
         Map_High_BIOS : constant Boolean
           := XML_Utils.Is_Free_To_Map
             (Subject         => Subject,
              Virtual_Address => Hi_BIOS_Addr_Start,
              Region_Size     => BIOS_Region_Size);
      begin
         if Map_Low_BIOS or Map_High_BIOS then
            Mulog.Log
              (Msg => "Adding BIOS region(s) for subject '" & Subj_Name & "'");
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => Subj_Name & "|bios",
               Address     => "",
               Size        => Mutools.Utils.To_Hex
                 (Number => BIOS_Region_Size),
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "subject_bios");
         end if;

         if Map_Low_BIOS then
            Muxml.Utils.Append_Child
              (Node      => Subj_Mem_Node,
               New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "bios",
                  Physical_Name => Subj_Name & "|bios",
                  Address       => Mutools.Utils.To_Hex
                    (Number => Lo_BIOS_Addr_Start),
                  Writable      => False,
                  Executable    => False));
         end if;

         if Map_High_BIOS then
            Muxml.Utils.Append_Child
              (Node      => Subj_Mem_Node,
               New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "bios",
                  Physical_Name => Subj_Name & "|bios",
                  Address       => Mutools.Utils.To_Hex
                    (Number => Hi_BIOS_Addr_Start),
                  Writable      => False,
                  Executable    => False));
         end if;
      end;

      --  Append sinfo address to boot parameters.

      if Boot_Params_Node = null then
         declare
            Doc_Node : constant DOM.Core.Document
              := DOM.Core.Nodes.Owner_Document (N => Subject);
         begin
            Boot_Params_Node := DOM.Core.Nodes.Insert_Before
              (N         => Subject,
               New_Child => DOM.Core.Documents.Create_Element
                 (Doc      => Doc_Node,
                  Tag_Name => "bootparams"),
               Ref_Child => Subj_Mem_Node);
         end;
      end if;

      declare
         Text_Node : DOM.Core.Node
           := DOM.Core.Nodes.First_Child (N => Boot_Params_Node);
         Sinfo_Str : constant String := " muen_sinfo=0x"
           & Mutools.Utils.To_Hex
           (Number    => Config.Subject_Info_Virtual_Addr,
            Normalize => False);
      begin
         if Text_Node = null then
            declare
               Doc_Node : constant DOM.Core.Document
                 := DOM.Core.Nodes.Owner_Document (N => Subject);
            begin
               Text_Node := DOM.Core.Nodes.Append_Child
                 (N         => Boot_Params_Node,
                  New_Child => DOM.Core.Documents.Create_Text_Node
                    (Doc  => Doc_Node,
                     Data => ""));
            end;
         end if;

         DOM.Core.Nodes.Set_Node_Value
           (N     => Text_Node,
            Value => Ada.Strings.Fixed.Trim
              (Source => DOM.Core.Nodes.Node_Value
                   (N => Text_Node) & Sinfo_Str,
               Side   => Ada.Strings.Left));
      end;
   end Handle_Linux_Profile;

end Expanders.Subjects.Profiles;
