--
--  Copyright (C) 2019  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2019  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Muxml.Utils;

with Cmd_Stream.Constants;
with Cmd_Stream.Roots.Utils;

package body Cmd_Stream.Roots.Kernels
is

   package CU renames Cmd_Stream.Utils;

   --  Assign given logical devices to specified kernel.
   procedure Assign_Devices
     (Stream_Doc    : in out CU.Stream_Document_Type;
      Physical_Devs :        DOM.Core.Node_List;
      Logical_Devs  :        DOM.Core.Node_List;
      Kernel_Attr   :        CU.Attribute_Type);

   -------------------------------------------------------------------------

   procedure Assign_Devices
     (Stream_Doc    : in out CU.Stream_Document_Type;
      Physical_Devs :        DOM.Core.Node_List;
      Logical_Devs  :        DOM.Core.Node_List;
      Kernel_Attr   :        CU.Attribute_Type)
   is
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Logical_Devs) - 1 loop
         declare
            Logical_Dev : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Logical_Devs,
                 Index => I);
            Physical_Dev : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Physical_Devs,
                 Ref_Attr  => "name",
                 Ref_Value => DOM.Core.Elements.Get_Attribute
                   (Elem => Logical_Dev,
                    Name => "physical"));
            Dev_Attr : constant CU.Attribute_Type
              := (Attr  => U ("device"),
                  Value => U (DOM.Core.Elements.Get_Attribute
                    (Elem => Physical_Dev,
                     Name => Constants.Dev_ID_Attr_Name)));
         begin
            CU.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "assignDeviceKernel",
               Attrs      => (Kernel_Attr, Dev_Attr));
         end;
      end loop;
   end Assign_Devices;

   -------------------------------------------------------------------------

   procedure Create_Per_CPU_Kernel
     (Policy     : in out Muxml.XML_Data_Type;
      Stream_Doc : in out CU.Stream_Document_Type)
   is
      Phys_Mem : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      Phys_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/hardware/devices/device");
      Krnl_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/kernel/devices/device");
      Kernels : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/kernel/memory/cpu");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Kernels) - 1 loop
         declare
            Kernel : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Kernels,
                 Index => I);
            CPU : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Kernel,
                 Name => "id");
            Root_ID : constant Natural := Allocate_Root;
            Krnl_Attr : constant CU.Attribute_Type
              := (Attr  => U ("kernel"),
                  Value => U (Trim (Root_ID'Img)));
         begin
            CU.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "createKernel",
               Attrs      => (Krnl_Attr,
                              (Attr  => U ("cpu"),
                               Value => U (CPU))));

            Assign_Devices
              (Stream_Doc    => Stream_Doc,
               Physical_Devs => Phys_Devs,
               Logical_Devs  => Krnl_Devs,
               Kernel_Attr   => Krnl_Attr);

            Utils.Assign_Memory
              (Stream_Doc    => Stream_Doc,
               Physical_Mem  => Phys_Mem,
               Physical_Devs => Phys_Devs,
               Logical_Mem   => McKae.XML.XPath.XIA.XPath_Query
                 (N     => Kernel,
                  XPath => "memory"),
               Logical_Devs  => McKae.XML.XPath.XIA.XPath_Query
                 (N     => Kernel,
                  XPath => "../../devices/device[memory]"),
               Object_Attr   => Krnl_Attr,
               Object_Kind   => "Kernel",
               Entity_Name   => "kernel_" & CPU & "|pt",
               Paging_Levels => 4);

            CU.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "activateKernel",
               Attrs      => (1 => Krnl_Attr));
         end;
      end loop;
   end Create_Per_CPU_Kernel;

end Cmd_Stream.Roots.Kernels;
