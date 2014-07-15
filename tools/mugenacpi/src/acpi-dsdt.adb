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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mutools.Utils;
with Mutools.Templates;

with Muxml.Utils;

with Acpi.Asl;

with String_Templates;

package body Acpi.DSDT
is

   use Ada.Strings.Unbounded;

   function Indent
     (N         : Positive := 1;
      Unit_Size : Positive := 4)
      return String renames Mutools.Utils.Indent;

   -------------------------------------------------------------------------

   procedure Write
     (Policy   : Muxml.XML_Data_Type;
      Subject  : DOM.Core.Node;
      Filename : String)
   is
      Devices  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "/system/platform/devices/device[memory]");
      Dsl_File : String := Filename;
      Tmpl     : Mutools.Templates.Template_Type;
      Buffer   : Unbounded_String;

      --  Add resources of given subject device memory to string buffer.
      procedure Add_Device_Memory_Resources (Dev_Mem : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Add_Device_Memory_Resources (Dev_Mem : DOM.Core.Node)
      is
         Log_Mem_Name  : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Dev_Mem,
              Name => "logical");
         Phys_Mem_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Dev_Mem,
              Name => "physical");
         Virtual_Addr  : constant Interfaces.Unsigned_32
           := Interfaces.Unsigned_32'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => Dev_Mem,
                 Name => "virtualAddress"));
         Log_Dev_Name  : constant String
           := DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Parent_Node (N => Dev_Mem),
            Name => "logical");
         Phys_Dev_Name : constant String
           := DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Parent_Node (N => Dev_Mem),
            Name => "physical");
         Physical_Dev  : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Nodes     => Devices,
              Ref_Attr  => "name",
              Ref_Value => Phys_Dev_Name);
         Mem_Size      : constant Interfaces.Unsigned_32
           := Interfaces.Unsigned_32'Value
             (Muxml.Utils.Get_Attribute
                (Doc   => Physical_Dev,
                 XPath => "memory[@name='" & Phys_Mem_Name & "']",
                 Name  => "size"));
      begin
         Buffer := Buffer & Indent (N => 5)
           & "/* " & Log_Dev_Name & "->" & Log_Mem_Name & " */";
         Buffer := Buffer & ASCII.LF & Indent (N => 5);
         Buffer := Buffer & Asl.DWordMemory
           (Base_Address => Virtual_Addr,
            Size         => Mem_Size) & ASCII.LF;
      end Add_Device_Memory_Resources;
   begin
      Dsl_File (Dsl_File'Last - 3 .. Dsl_File'Last) := ".dsl";

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.linux_dsdt_dsl);

      Set_PCI_Cfg_Space_Address :
      declare
         PCI_Cfg_Addr_Str : constant String := Muxml.Utils.Get_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/platform/devices",
            Name  => "pciConfigAddress");
         PCI_Cfg_Addr     : Interfaces.Unsigned_64 := 0;
      begin
         if PCI_Cfg_Addr_Str'Length > 0 then
            PCI_Cfg_Addr := Interfaces.Unsigned_64'Value (PCI_Cfg_Addr_Str);
         end if;

         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__config_base_address__",
            Content  => Mutools.Utils.To_Hex
              (Number     => PCI_Cfg_Addr,
               Normalize  => False));
      end Set_PCI_Cfg_Space_Address;

      Add_Device_Memory :
      declare
         Dev_Mem : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "devices/device/memory[@physical!='mmconf']");
         Count   : constant Natural
           := DOM.Core.Nodes.Length (List => Dev_Mem);
      begin
         for I in 0 .. Count - 1 loop
            Add_Device_Memory_Resources
              (Dev_Mem => DOM.Core.Nodes.Item
                 (List  => Dev_Mem,
                  Index => I));
         end loop;

         Buffer := Buffer & Indent (N => 4);

         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__reserved_memory__",
            Content  => To_String (Buffer));
      end Add_Device_Memory;

      Mutools.Templates.Write (Template => Tmpl,
                               Filename => Dsl_File);
   end Write;

end Acpi.DSDT;
