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

with Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Files;
with Mutools.Utils;

with Interfaces;

with Paging.EPT;
with Paging.IA32e;
with Paging.Layouts;

package body Pt.Generator
is

   use Ada.Strings.Unbounded;

   --  Write pagetable files for each kernel/CPU as specified by the policy.
   procedure Write_Kernel_Pagetable
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   --  Write pagetable files for all subjects as specified by the policy.
   procedure Write_Subject_Pagetable
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   --  Create paging structures from given memory regions and write them to the
   --  specified file. The PML4 address parameter specifies the physical start
   --  address of the PML4 paging structure. Depending on the given pagetable
   --  type IA-32e or EPT pagetables will be generated.
   procedure Write_Pagetable
     (Policy       : Muxml.XML_Data_Type;
      Memory       : DOM.Core.Node_List;
      Devices      : DOM.Core.Node_List;
      Pml4_Address : Interfaces.Unsigned_64;
      Filename     : String;
      PT_Type      : Paging.Paging_Mode_Type := Paging.IA32e_Mode);

   --  Write memory layout of with specified paging mode to file specified by
   --  name.
   procedure Write_To_File
     (Mem_Layout : Paging.Layouts.Memory_Layout_Type;
      PT_Type    : Paging.Paging_Mode_Type;
      Filename   : String);

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
   begin
      Write_Kernel_Pagetable (Output_Dir => Output_Dir,
                              Policy     => Policy);
      Write_Subject_Pagetable (Output_Dir => Output_Dir,
                               Policy     => Policy);
   end Write;

   -------------------------------------------------------------------------

   procedure Write_Kernel_Pagetable
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Phys_Mem : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      CPUs     : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/kernel/memory/cpu");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => CPUs) - 1 loop
         declare
            ID_Str : constant String := I'Img (I'Img'First + 1 .. I'Img'Last);

            PT_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes => Phys_Mem,
                 Refs  => ((Name  => To_Unbounded_String ("type"),
                            Value => To_Unbounded_String ("system_pt")),
                           (Name  => To_Unbounded_String ("name"),
                            Value => To_Unbounded_String ("kernel_"
                              & ID_Str & "|pt"))));
            Filename  : constant String := Muxml.Utils.Get_Attribute
              (Doc   => PT_Node,
               XPath => "file",
               Name  => "filename");
            PML4_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => PT_Node,
                    Name => "physicalAddress"));
            Nodes     : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Policy.Doc,
                 XPath => "/system/kernel/memory/cpu[@id='" & ID_Str
                 & "']/memory");
            Devices   : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Policy.Doc,
                 XPath => "/system/kernel/devices/device[count(memory)>0]");
         begin
            Mulog.Log (Msg => "Writing kernel pagetable of CPU" & I'Img
                       & " to '" & Output_Dir & "/" & Filename & "'");
            Write_Pagetable
              (Policy       => Policy,
               Memory       => Nodes,
               Devices      => Devices,
               Pml4_Address => PML4_Addr,
               Filename     => Output_Dir & "/" & Filename,
               PT_Type      => Paging.IA32e_Mode);
         end;
      end loop;
   end Write_Kernel_Pagetable;

   -------------------------------------------------------------------------

   procedure Write_Pagetable
     (Policy       : Muxml.XML_Data_Type;
      Memory       : DOM.Core.Node_List;
      Devices      : DOM.Core.Node_List;
      Pml4_Address : Interfaces.Unsigned_64;
      Filename     : String;
      PT_Type      : Paging.Paging_Mode_Type := Paging.IA32e_Mode)
   is
      Phys_Mem  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      Phys_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/hardware/devices/device");
      Vmem      : Paging.Layouts.Memory_Layout_Type (Levels => 4);

      --  Add mapping of given logical to physical memory.
      procedure Add_Mapping
        (Physical : DOM.Core.Node;
         Logical  : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Add_Mapping
        (Physical : DOM.Core.Node;
         Logical  : DOM.Core.Node)
      is
         Size    : constant Interfaces.Unsigned_64
           := Interfaces.Unsigned_64'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => Physical,
                 Name => "size"));
         PMA     : constant Interfaces.Unsigned_64
           := Interfaces.Unsigned_64'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => Physical,
                 Name => "physicalAddress"));
         VMA     : constant Interfaces.Unsigned_64
           := Interfaces.Unsigned_64'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => Logical,
                 Name => "virtualAddress"));
         Write   : constant Boolean
           := Boolean'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => Logical,
                 Name => "writable"));
         Exec    : constant Boolean
           := Boolean'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => Logical,
                 Name => "executable"));
         Caching : constant Paging.Caching_Type
           := Paging.Caching_Type'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => Physical,
                 Name => "caching"));
      begin
         Mulog.Log (Msg => "Mapping PMA " & Mutools.Utils.To_Hex (PMA)
                    & " to VMA " & Mutools.Utils.To_Hex (VMA)
                    & ", size " & Mutools.Utils.To_Hex (Size)
                    & ", caching " & Caching'Img & ", R"
                    & (if Write then "W" else "-")
                    & (if Exec then "X" else "-"));
         Paging.Layouts.Add_Memory_Region
           (Mem_Layout       => Vmem,
            Physical_Address => PMA,
            Virtual_Address  => VMA,
            Size             => Size,
            Caching          => Caching,
            Writable         => Write,
            Executable       => Exec);
      end Add_Mapping;
   begin
      Paging.Layouts.Set_Large_Page_Support
        (Mem_Layout => Vmem,
         State      => False);
      Paging.Layouts.Set_Address (Mem_Layout => Vmem,
                                  Address    => Pml4_Address);

      for I in 0 .. DOM.Core.Nodes.Length (List => Memory) - 1 loop
         declare
            Logical_Mem   : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Memory,
                 Index => I);
            Logical_Name  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Logical_Mem,
                 Name => "logical");
            Physical_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Logical_Mem,
                 Name => "physical");
            Physical_Mem  : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Phys_Mem,
                 Ref_Attr  => "name",
                 Ref_Value => Physical_Name);
         begin
            Mulog.Log (Msg => "Adding region " & Logical_Name
                       & "[" & Physical_Name & "]");
            Add_Mapping (Physical => Physical_Mem,
                         Logical  => Logical_Mem);
         end;
      end loop;

      for I in 0 .. DOM.Core.Nodes.Length (List => Devices) - 1 loop
         declare
            Device   : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Devices,
               Index => I);
            Dev_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Device,
               Name => "physical");
            Dev_Mem  : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Device,
                 XPath => "memory");
         begin
            for D in 0 .. DOM.Core.Nodes.Length (List => Dev_Mem) - 1 loop
               declare
                  Logical_Mem   : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Dev_Mem,
                       Index => D);
                  Logical_Name  : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Logical_Mem,
                       Name => "logical");
                  Physical_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Logical_Mem,
                       Name => "physical");
                  Physical_Dev  : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Nodes     => Phys_Devs,
                       Ref_Attr  => "name",
                       Ref_Value => Dev_Name);
                  Physical_Mem  : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Doc   => Physical_Dev,
                       XPath => "memory[@name='" & Physical_Name & "']");
               begin
                  Mulog.Log (Msg => "Adding region " & Logical_Name
                             & "[" & Physical_Name & "] of device "
                             & Dev_Name);
                  Add_Mapping (Physical => Physical_Mem,
                               Logical  => Logical_Mem);
               end;
            end loop;
         end;
      end loop;

      Paging.Layouts.Update_References (Mem_Layout => Vmem);

      Write_To_File (Mem_Layout => Vmem,
                     PT_Type    => PT_Type,
                     Filename   => Filename);
   end Write_Pagetable;

   -------------------------------------------------------------------------

   procedure Write_Subject_Pagetable
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Phys_Mem : constant DOM.Core.Node_List
         := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/memory/memory");
      Subjects : constant DOM.Core.Node_List
         := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Subj_Node,
               Name => "name");
            PT_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes => Phys_Mem,
                 Refs  => ((Name  => To_Unbounded_String ("type"),
                            Value => To_Unbounded_String ("system_pt")),
                           (Name  => To_Unbounded_String ("name"),
                            Value => To_Unbounded_String (Name & "|pt"))));
            Filename : constant String := Muxml.Utils.Get_Attribute
              (Doc   => PT_Node,
               XPath => "file",
               Name  => "filename");
            PML4_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => PT_Node,
                    Name => "physicalAddress"));
            Nodes : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subj_Node,
                 XPath => "memory/memory");
            Devices : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subj_Node,
                 XPath => "devices/device[count(memory)>0]");

            Paging_Mode : Paging.Paging_Mode_Type;
         begin
            if Muxml.Utils.Get_Element_Value
              (Doc   => DOM.Core.Nodes.Item (List  => Subjects,
                                             Index => I),
               XPath => "vcpu/vmx/controls/proc2/EnableEPT") = "1"
            then
               Paging_Mode := Paging.EPT_Mode;
            else
               Paging_Mode := Paging.IA32e_Mode;
            end if;

            Mulog.Log (Msg => "Writing " & Paging_Mode'Img & " pagetable of "
                       & "subject '" & Name & "' to '" & Output_Dir & "/"
                       & Filename & "'");
            Write_Pagetable
              (Policy       => Policy,
               Memory       => Nodes,
               Devices      => Devices,
               Pml4_Address => PML4_Addr,
               Filename     => Output_Dir & "/" & Filename,
               PT_Type      => Paging_Mode);
         end;
      end loop;
   end Write_Subject_Pagetable;

   -------------------------------------------------------------------------

   procedure Write_To_File
     (Mem_Layout : Paging.Layouts.Memory_Layout_Type;
      PT_Type    : Paging.Paging_Mode_Type;
      Filename   : String)
   is
      use Ada.Streams.Stream_IO;

      File : File_Type;
   begin
      Mutools.Files.Open (Filename => Filename,
                          File     => File);

      case PT_Type is
         when Paging.IA32e_Mode =>
            Paging.Layouts.Serialize
              (Stream      => Stream (File),
               Mem_Layout  => Mem_Layout,
               Serializers =>
                 (1 => Paging.IA32e.Serialize_PML4'Access,
                  2 => Paging.IA32e.Serialize_PDPT'Access,
                  3 => Paging.IA32e.Serialize_PD'Access,
                  4 => Paging.IA32e.Serialize_PT'Access));
         when Paging.EPT_Mode =>
            Paging.Layouts.Serialize
              (Stream      => Stream (File),
               Mem_Layout  => Mem_Layout,
               Serializers =>
                 (1 => Paging.EPT.Serialize_PML4'Access,
                  2 => Paging.EPT.Serialize_PDPT'Access,
                  3 => Paging.EPT.Serialize_PD'Access,
                  4 => Paging.EPT.Serialize_PT'Access));
      end case;

      Close (File => File);
   end Write_To_File;

end Pt.Generator;
