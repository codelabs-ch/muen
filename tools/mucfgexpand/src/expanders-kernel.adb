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

with Ada.Strings.Fixed;

with Interfaces;

with McKae.XML.XPath.XIA;

with DOM.Core.Nodes;
with DOM.Core.Documents;
with DOM.Core.Elements;

with Mulog;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.Constants;

with Expanders.XML_Utils;

package body Expanders.Kernel
is

   -------------------------------------------------------------------------

   procedure Add_Binary_Mappings (Data : in out Muxml.XML_Data_Type)
   is
      CPU_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/kernel/memory/cpu");
   begin
      Mulog.Log (Msg => "Adding binary memory mappings for"
                 & DOM.Core.Nodes.Length (List => CPU_Nodes)'Img
                 & " kernel(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => CPU_Nodes) - 1 loop
         declare
            CPU_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => CPU_Nodes,
                 Index => I);
            CPU_Str  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => CPU_Node,
                 Name => "id");
         begin
            Muxml.Utils.Append_Child
              (Node      => CPU_Node,
               New_Child => XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "text",
                  Physical_Name => "kernel_text",
                  Address       => "16#0010_0000#",
                  Writable      => False,
                  Executable    => True));
            Muxml.Utils.Append_Child
              (Node      => CPU_Node,
               New_Child => XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "data",
                  Physical_Name => "kernel_data",
                  Address       => "16#0011_0000#",
                  Writable      => True,
                  Executable    => False));
            Muxml.Utils.Append_Child
              (Node      => CPU_Node,
               New_Child => XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "bss",
                  Physical_Name => "kernel_bss",
                  Address       => "16#0011_1000#",
                  Writable      => True,
                  Executable    => False));
            Muxml.Utils.Append_Child
              (Node      => CPU_Node,
               New_Child => XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "ro",
                  Physical_Name => "kernel_ro",
                  Address       => "16#0011_f000#",
                  Writable      => False,
                  Executable    => False));
            Muxml.Utils.Append_Child
              (Node      => CPU_Node,
               New_Child => XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "stack",
                  Physical_Name => "kernel_stack_" & CPU_Str,
                  Address       => "16#0011_3000#",
                  Writable      => True,
                  Executable    => False));
            Muxml.Utils.Append_Child
              (Node      => CPU_Node,
               New_Child => XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "store",
                  Physical_Name => "kernel_store_" & CPU_Str,
                  Address       => "16#0011_6000#",
                  Writable      => True,
                  Executable    => False));
         end;
      end loop;
   end Add_Binary_Mappings;

   -------------------------------------------------------------------------

   procedure Add_Devices (Data : in out Muxml.XML_Data_Type)
   is
      Devices_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/kernel/devices");
      Ioapic       : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Data.Doc,
           Tag_Name => "device");
   begin
      Mulog.Log (Msg => "Adding devices to kernel");

      DOM.Core.Elements.Set_Attribute
        (Elem  => Ioapic,
         Name  => "logical",
         Value => "ioapic");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Ioapic,
         Name  => "physical",
         Value => "ioapic");

      Muxml.Utils.Append_Child
        (Node      => Ioapic,
         New_Child => XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "mmio",
            Physical_Name => "mmio",
            Address       => "16#001f_c000#",
            Writable      => True,
            Executable    => False));
      Muxml.Utils.Append_Child
        (Node      => Devices_Node,
         New_Child => Ioapic);
   end Add_Devices;

   -------------------------------------------------------------------------

   procedure Add_Section_Skeleton (Data : in out Muxml.XML_Data_Type)
   is
      CPU_Count     : constant Positive
        := Positive'Value
          (Muxml.Utils.Get_Attribute
             (Doc   => Data.Doc,
              XPath => "/system/platform/processor",
              Name  => "logicalCpus"));
      Kernel_Node   : DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Data.Doc,
           Tag_Name => "kernel");
      Memory_Node   : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Data.Doc,
           Tag_Name => "memory");
      Subjects_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/subjects");
   begin
      Kernel_Node := DOM.Core.Nodes.Insert_Before
        (N         => DOM.Core.Nodes.Parent_Node (N => Subjects_Node),
         New_Child => Kernel_Node,
         Ref_Child => Subjects_Node);

      Muxml.Utils.Append_Child
        (Node      => Kernel_Node,
         New_Child => Memory_Node);
      Muxml.Utils.Append_Child
        (Node      => Kernel_Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "devices"));

      for I in 0 .. CPU_Count - 1 loop
         declare
            CPU_Str  : constant String
              := Ada.Strings.Fixed.Trim
                (Source => I'Img,
                 Side   => Ada.Strings.Left);
            CPU_Node : constant DOM.Core.Node
              := DOM.Core.Documents.Create_Element
                (Doc      => Data.Doc,
                 Tag_Name => "cpu");
         begin
            DOM.Core.Elements.Set_Attribute
              (Elem  => CPU_Node,
               Name  => "id",
               Value => CPU_Str);
            Muxml.Utils.Append_Child
              (Node      => Memory_Node,
               New_Child => CPU_Node);
         end;
      end loop;
   end Add_Section_Skeleton;

   -------------------------------------------------------------------------

   procedure Add_Subj_State_Mappings (Data : in out Muxml.XML_Data_Type)
   is
      State_Start : constant := 16#001e_0000#;
      CPU_Nodes   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/kernel/memory/cpu");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => CPU_Nodes) - 1 loop
         declare
            CPU      : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => CPU_Nodes,
                 Index => I);
            CPU_Id   : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => CPU,
                 Name => "id");
            Subjects : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Data.Doc,
                 XPath => "/system/subjects/subject[@cpu='" & CPU_Id & "']");
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
               declare
                  use type Interfaces.Unsigned_64;

                  Subj      : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Subjects,
                       Index => J);
                  Subj_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Subj,
                       Name => "name");
                  Subj_Id   : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Subj,
                       Name => "id");
                  Address   : constant Interfaces.Unsigned_64
                    := State_Start + Interfaces.Unsigned_64'Value (Subj_Id)
                    * Mutools.Constants.Page_Size;
               begin
                  Mulog.Log (Msg => "Mapping state of subject '" & Subj_Name
                             & "' to address " & Mutools.Utils.To_Hex
                               (Number => Address) & " on CPU " & CPU_Id);
                  Muxml.Utils.Append_Child
                    (Node      => CPU,
                     New_Child => XML_Utils.Create_Virtual_Memory_Node
                       (Policy        => Data,
                        Logical_Name  => Subj_Name & "_state",
                        Physical_Name => Subj_Name & "_state",
                        Address       => Mutools.Utils.To_Hex
                          (Number => Address),
                        Writable      => True,
                        Executable    => False));
               end;
            end loop;
         end;
      end loop;
   end Add_Subj_State_Mappings;

   -------------------------------------------------------------------------

   procedure Map_Tau0_Interface (Data : in out Muxml.XML_Data_Type)
   is
      BSP : constant DOM.Core.Node := Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu[@id='0']");
   begin
      Mulog.Log (Msg => "Mapping tau0 system interface on CPU 0");

      Muxml.Utils.Append_Child
        (Node      => BSP,
         New_Child => XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "tau0_interface",
            Physical_Name => "sys_interface",
            Address       => "16#001f_f000#",
            Writable      => False,
            Executable    => False));
   end Map_Tau0_Interface;

end Expanders.Kernel;
