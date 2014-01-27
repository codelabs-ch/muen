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

with Ada.Streams.Stream_IO;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Files;

with Interfaces;

with Paging.EPT;
with Paging.IA32e;
with Paging.Memory;

package body Pt.Generator
is

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
   --  adddress of the PML4 paging structure. Depending on the given pagetable
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
     (Mem_Layout : Paging.Memory.Memory_Layout_Type;
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
      CPUs : DOM.Core.Node_List;
   begin
      CPUs := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/kernel/memory/cpu");

      for I in 0 .. DOM.Core.Nodes.Length (List => CPUs) - 1 loop
         declare
            ID_Str : constant String := I'Img (I'Img'First + 1 .. I'Img'Last);

            Filename  : constant String := Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/memory/memory[@name='kernel_" & ID_Str
               & "|pt']/file[@format='pt']",
               Name  => "filename");
            PML4_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (Muxml.Utils.Get_Attribute
                     (Doc   => Policy.Doc,
                      XPath => "/system/memory/memory[@name='kernel_" & ID_Str
                      & "|pt']",
                      Name  => "physicalAddress"));
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
      Vmem : Paging.Memory.Memory_Layout_Type;

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
         Paging.Memory.Add_Memory_Region
           (Mem_Layout       => Vmem,
            Physical_Address => PMA,
            Virtual_Address  => VMA,
            Size             => Size,
            Caching          => Caching,
            Writable         => Write,
            Executable       => Exec);
      end Add_Mapping;
   begin
      Paging.Memory.Set_Address (Mem_Layout => Vmem,
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
              := Muxml.Utils.Get_Attribute
                (Doc   => Logical_Mem,
                 XPath => "physical",
                 Name  => "name");
            Physical_Mem  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => McKae.XML.XPath.XIA.XPath_Query
                     (N     => Policy.Doc,
                      XPath => "/system/memory/memory[@name='" & Physical_Name
                      & "']"),
                 Index => 0);
         begin
            Mulog.Log (Msg => "Adding region " & Logical_Name
                       & "[" & Physical_Name & "]");
            Add_Mapping (Physical => Physical_Mem,
                         Logical  => Logical_Mem);
         end;
      end loop;

      for I in 0 .. DOM.Core.Nodes.Length (List => Devices) - 1 loop
         declare
            Device   : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
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
                    := DOM.Core.Nodes.Item
                      (List  => McKae.XML.XPath.XIA.XPath_Query
                           (N     => Policy.Doc,
                            XPath => "/system/platform/device[@name='"
                            & Dev_Name & "']/memory[@name='" & Physical_Name
                            & "']"),
                       Index => 0);
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

      Paging.Memory.Update_References (Mem_Layout => Vmem);

      Write_To_File (Mem_Layout => Vmem,
                     PT_Type    => PT_Type,
                     Filename   => Filename);
   end Write_Pagetable;

   -------------------------------------------------------------------------

   procedure Write_Subject_Pagetable
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Subjects : DOM.Core.Node_List;
   begin
      Subjects := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject");

      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Name      : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => DOM.Core.Nodes.Item (List  => Subjects,
                                            Index => I),
               Name => "name");
            Filename  : constant String := Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/memory/memory[@name='" & Name & "|pt']/"
               & "file[@format='pt']",
               Name  => "filename");
            PML4_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (Muxml.Utils.Get_Attribute
                     (Doc   => Policy.Doc,
                      XPath => "/system/memory/memory[@name='" & Name
                      & "|pt']",
                      Name  => "physicalAddress"));
            Nodes     : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => DOM.Core.Nodes.Item
                     (List  => Subjects,
                      Index => I),
                 XPath => "memory/memory");
            Devices   : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => DOM.Core.Nodes.Item
                     (List  => Subjects,
                      Index => I),
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
                       & Name & " to '" & Output_Dir & "/" & Filename & "'");
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
     (Mem_Layout : Paging.Memory.Memory_Layout_Type;
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
            Paging.Memory.Serialize
              (Stream         => Stream (File),
               Mem_Layout     => Mem_Layout,
               Serialize_PML4 => Paging.IA32e.Serialize'Access,
               Serialize_PDPT => Paging.IA32e.Serialize'Access,
               Serialize_PD   => Paging.IA32e.Serialize'Access,
               Serialize_PT   => Paging.IA32e.Serialize'Access);
         when Paging.EPT_Mode =>
            Paging.Memory.Serialize
              (Stream         => Stream (File),
               Mem_Layout     => Mem_Layout,
               Serialize_PML4 => Paging.EPT.Serialize'Access,
               Serialize_PDPT => Paging.EPT.Serialize'Access,
               Serialize_PD   => Paging.EPT.Serialize'Access,
               Serialize_PT   => Paging.EPT.Serialize'Access);
      end case;

      Close (File => File);
   end Write_To_File;

end Pt.Generator;
