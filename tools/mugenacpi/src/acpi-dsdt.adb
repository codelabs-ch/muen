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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mutools.OS;
with Mutools.Utils;
with Mutools.XML_Utils;
with Mutools.Templates;

with Muxml.Utils;

with Acpi.Asl;
with Acpi.Utils;

with String_Templates;

package body Acpi.DSDT
is

   use Ada.Strings.Unbounded;

   Linux_Irq_Offset   : constant := 48;

   --  The size encompasses two PCI buses.
   PCI_Cfg_Space_Size : constant := 16#0100_0000#;

   --  Compile the source dsl file and store it in the specified target AML
   --  file.
   procedure Compile_Dsl
     (Source : String;
      Target : String);

   -------------------------------------------------------------------------

   procedure Compile_Dsl
     (Source : String;
      Target : String)
   is
   begin
      Mutools.OS.Execute (Command => "iasl -p" & Target & " " & Source);
   end Compile_Dsl;

   -------------------------------------------------------------------------

   procedure Write
     (Policy   : Muxml.XML_Data_Type;
      Subject  : DOM.Core.Node;
      Filename : String)
   is

      --  PCI interrupt pins.
      type Interrupt_Pin_Type is (INT_A, INT_B, INT_C, INT_D);

      --  Interrupt pin to number mapping, see ACPI spec section 6.2.1.
      Pin_Map : constant array (Interrupt_Pin_Type) of Natural
        := (INT_A => 0,
            INT_B => 1,
            INT_C => 2,
            INT_D => 3);

      Devices    : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "/system/platform/devices/device");
      Dsl_File   : String := Filename;
      Tmpl       : Mutools.Templates.Template_Type;
      Buffer     : Unbounded_String;
      Cur_Serial : Natural;

      --  Add resources of given subject device memory to string buffer.
      procedure Add_Device_Memory_Resources (Dev_Mem : DOM.Core.Node);

      --  Add PCI LSI interrupt resources of given subject logical device to
      --  string buffer. The Irq_Count is incremented by the number of created
      --  entries.
      procedure Add_Device_LSI_Resource
        (Logical_Dev :        DOM.Core.Node;
         Irq_Count   : in out Natural);

      --  Add PCI MSI interrupt resources of given subject logical device to
      --  string buffer. The Irq_Count is incremented by the number of created
      --  entries.
      procedure Add_Device_MSI_Resource
        (Logical_Dev :        DOM.Core.Node;
         Irq_Count   : in out Natural);

      --  Add resources of given subject legacy device to string buffer.
      procedure Add_Legacy_Device_Resources (Legacy_Dev : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Add_Device_LSI_Resource
        (Logical_Dev :        DOM.Core.Node;
         Irq_Count   : in out Natural)
      is
         Log_Dev_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Logical_Dev,
              Name => "logical");
         PCI_Node     : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Logical_Dev,
              XPath => "pci");
         Bus_Nr       : constant Interfaces.Unsigned_64
           := Interfaces.Unsigned_64'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => PCI_Node,
                 Name => "bus"));
         Device_Nr    : constant Interfaces.Unsigned_64
           := Interfaces.Unsigned_64'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => PCI_Node,
                 Name => "device"));
         Irqs         : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Logical_Dev,
              XPath => "irq");
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Irqs) - 1 loop
            declare
               Irq_Node     : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => Irqs,
                    Index => I);
               Log_Irq_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Irq_Node,
                    Name => "logical");
               Virtual_Irq  : constant Interfaces.Unsigned_64
                 := Interfaces.Unsigned_64'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Irq_Node,
                       Name => "vector")) - Linux_Irq_Offset;
            begin
               Buffer := Buffer & Utils.Indent (N => 5) & "/* " & Log_Dev_Name
                 & "->" & Log_Irq_Name & " */" & ASCII.LF;

               for P in Interrupt_Pin_Type loop
                  Utils.Add_Dev_IRQ_Resource
                    (Buffer  => Buffer,
                     Bus_Nr  => Bus_Nr,
                     Dev_Nr  => Device_Nr,
                     Irq_Nr  => Virtual_Irq,
                     Int_Pin => Pin_Map (P));
                  Irq_Count := Irq_Count + 1;
               end loop;
            end;
         end loop;
      end Add_Device_LSI_Resource;

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
         Buffer := Buffer & Utils.Indent (N => 5)
           & "/* " & Log_Dev_Name & "->" & Log_Mem_Name & " */";
         Buffer := Buffer & ASCII.LF & Utils.Indent (N => 5);
         Buffer := Buffer & Asl.DWordMemory
           (Base_Address => Virtual_Addr,
            Size         => Mem_Size,
            Cacheable    => True) & ASCII.LF;
      end Add_Device_Memory_Resources;

      ----------------------------------------------------------------------

      procedure Add_Device_MSI_Resource
        (Logical_Dev :        DOM.Core.Node;
         Irq_Count   : in out Natural)
      is
         Log_Dev_Name  : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Logical_Dev,
              Name => "logical");
         Phys_Dev_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Logical_Dev,
              Name => "physical");
         PCI_Node      : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Logical_Dev,
              XPath => "pci");
         Bus_Nr        : constant Interfaces.Unsigned_64
           := Interfaces.Unsigned_64'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => PCI_Node,
                 Name => "bus"));
         Device_Nr     : constant Interfaces.Unsigned_64
           := Interfaces.Unsigned_64'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => PCI_Node,
                 Name => "device"));
         Irqs          : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Logical_Dev,
              XPath => "irq");

         Lowest_Irq       : DOM.Core.Node;
         Lowest_Vector_Nr : Natural := Natural'Last;
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Irqs) - 1 loop
            declare
               Irq_Node  : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => Irqs,
                    Index => I);
               Vector_Nr : constant Natural
                 := Natural'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Irq_Node,
                       Name => "vector"));
            begin
               if Vector_Nr < Lowest_Vector_Nr then
                  Lowest_Irq       := Irq_Node;
                  Lowest_Vector_Nr := Vector_Nr;
               end if;
            end;
         end loop;

         declare
            Log_Irq_Name  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Lowest_Irq,
                 Name => "logical");
            Phys_Irq_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Lowest_Irq,
                 Name => "physical");
            Virtual_Irq   : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64 (Lowest_Vector_Nr) - Linux_Irq_Offset;
            Phys_Irq_Nr   : constant Natural
              := Natural'Value
                (Muxml.Utils.Get_Attribute
                   (Doc   => Policy.Doc,
                    XPath => "/system/platform/devices"
                    & "/device[@name='" & Phys_Dev_Name & "']"
                    & "/irq[@name='" & Phys_Irq_Name & "']",
                    Name  => "number"));
         begin
            Buffer := Buffer & Utils.Indent (N => 5) & "/* " & Log_Dev_Name
              & "->" & Log_Irq_Name & " (MSI) */" & ASCII.LF;

            for P in Interrupt_Pin_Type loop
               Utils.Add_Dev_IRQ_Resource
                 (Buffer  => Buffer,
                  Bus_Nr  => Bus_Nr,
                  Dev_Nr  => Device_Nr,
                  Irq_Nr  => Virtual_Irq,
                  Int_Pin => Pin_Map (P));
               Irq_Count := Irq_Count + 1;
            end loop;

            --  Use the PIN field to specify the physical IRQ, which
            --  corresponds to the Interrupt Remapping Table entry index of the
            --  base (lowest) MSI.

            Utils.Add_Dev_IRQ_Resource
              (Buffer  => Buffer,
               Bus_Nr  => Bus_Nr,
               Dev_Nr  => Device_Nr,
               Irq_Nr  => Virtual_Irq,
               Int_Pin => Phys_Irq_Nr);
            Irq_Count := Irq_Count + 1;
         end;
      end Add_Device_MSI_Resource;

      ----------------------------------------------------------------------

      procedure Add_Legacy_Device_Resources (Legacy_Dev : DOM.Core.Node)
      is
         Log_Dev_Name  : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Legacy_Dev,
              Name => "logical");
         Phys_Dev_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Legacy_Dev,
              Name => "physical");
         Physical_Dev  : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Nodes     => Devices,
              Ref_Attr  => "name",
              Ref_Value => Phys_Dev_Name);
         Logical_Ports : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Legacy_Dev,
              XPath => "ioPort");
         Phys_Ports    : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Physical_Dev,
              XPath => "ioPort");
      begin
         Buffer := Buffer & Utils.Indent (N => 4) & "Device (SER"
           & Ada.Characters.Handling.To_Upper
           (Item => Mutools.Utils.To_Hex
              (Number     => Interfaces.Unsigned_64 (Cur_Serial),
               Normalize  => False)) & ")" & ASCII.LF;
         Buffer := Buffer & Utils.Indent (N => 4) & "{" & ASCII.LF;
         Buffer := Buffer & Utils.Indent (N => 5)
           & "Name (_HID, EisaId (""PNP0501""))" & ASCII.LF;
         Buffer := Buffer & Utils.Indent (N => 5)
           & "Name (_UID, """ & Log_Dev_Name & """)" & ASCII.LF;
         Buffer := Buffer & Utils.Indent (N => 5)
           & "Method (_STA) { Return (0x0f) }" & ASCII.LF;
         Buffer := Buffer & Utils.Indent (N => 5) & "Method (_CRS)" & ASCII.LF
           & Utils.Indent (N => 5) & "{" & ASCII.LF;
         Buffer := Buffer & Utils.Indent (N => 6)
           & "Return (ResourceTemplate () {" & ASCII.LF;

         for I in 0 .. DOM.Core.Nodes.Length (List => Logical_Ports) - 1 loop
            declare
               use type Interfaces.Unsigned_16;

               Log_Port       : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => Logical_Ports,
                    Index => I);
               Phys_Port_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Log_Port,
                    Name => "physical");
               Phys_Port      : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Nodes     => Phys_Ports,
                    Ref_Attr  => "name",
                    Ref_Value => Phys_Port_Name);
               Start_Port     : constant Interfaces.Unsigned_16
                 := Interfaces.Unsigned_16'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Phys_Port,
                       Name => "start"));
               End_Port       : constant Interfaces.Unsigned_16
                 := Interfaces.Unsigned_16'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Phys_Port,
                       Name => "end"));
            begin
               Buffer := Buffer & Utils.Indent (N => 7) & Asl.IO
                 (Start_Port => Start_Port,
                  Port_Range => End_Port - Start_Port + 1) & ASCII.LF;
            end;
         end loop;

         Buffer := Buffer & Utils.Indent (N => 6) & "})" & ASCII.LF;
         Buffer := Buffer & Utils.Indent (N => 5) & "}" & ASCII.LF
           & Utils.Indent (N => 4) & "}" & ASCII.LF;
      end Add_Legacy_Device_Resources;
   begin
      Dsl_File (Dsl_File'Last - 3 .. Dsl_File'Last) := ".dsl";

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.linux_dsdt_dsl);

      PCI_Cfg_Space :
      declare
         PCI_Cfg_Addr_Str : constant String := Muxml.Utils.Get_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/platform/devices",
            Name  => "pciConfigAddress");
         PCI_Cfg_Addr     : Interfaces.Unsigned_64 := 0;
      begin
         if PCI_Cfg_Addr_Str'Length > 0 then
            PCI_Cfg_Addr := Interfaces.Unsigned_64'Value (PCI_Cfg_Addr_Str);

            Buffer := Buffer & Utils.Indent (N => 4)
              & Asl.DWordMemory
              (Base_Address => Interfaces.Unsigned_32 (PCI_Cfg_Addr),
               Size         => Interfaces.Unsigned_32 (PCI_Cfg_Space_Size),
               Cacheable    => False);
            Buffer := Buffer & ASCII.LF;
         end if;

         Buffer := Buffer & Utils.Indent (N => 3);

         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__pci_config_space__",
            Content  => To_String (Buffer));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__config_base_address__",
            Content  => Mutools.Utils.To_Hex
              (Number     => PCI_Cfg_Addr,
               Normalize  => False));
      end PCI_Cfg_Space;

      Buffer := Null_Unbounded_String;

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

         Buffer := Buffer & Utils.Indent (N => 4);

         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__reserved_memory__",
            Content  => To_String (Buffer));
      end Add_Device_Memory;

      Buffer := Null_Unbounded_String;

      Add_Device_Irq :
      declare
         PCI_Devices : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "devices/device[pci and irq]");
         Irq_Count   : Natural := 0;
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => PCI_Devices) - 1 loop
            declare
               Dev_Ref   : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => PCI_Devices,
                    Index => I);
               Phys_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Dev_Ref,
                    Name => "physical");
               Dev       : constant DOM.Core.Node
                   := Muxml.Utils.Get_Element
                     (Nodes     => Devices,
                      Ref_Attr  => "name",
                      Ref_Value => Phys_Name);
            begin
               case Mutools.XML_Utils.Get_IRQ_Kind (Dev => Dev) is
                  when Mutools.XML_Utils.IRQ_PCI_LSI =>
                     Add_Device_LSI_Resource
                       (Logical_Dev => Dev_Ref,
                        Irq_Count   => Irq_Count);
                  when Mutools.XML_Utils.IRQ_PCI_MSI =>
                     Add_Device_MSI_Resource
                       (Logical_Dev => Dev_Ref,
                        Irq_Count   => Irq_Count);
                  when others => null;
               end case;
            end;
         end loop;

         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__interrupt_count__",
            Content  => Ada.Strings.Fixed.Trim
              (Source => Irq_Count'Img,
               Side   => Ada.Strings.Left));

         Buffer := Buffer & Utils.Indent (N => 4);

         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__pci_routing_table__",
            Content  => To_String (Buffer));
      end Add_Device_Irq;

      Buffer := Null_Unbounded_String;

      Add_Legacy_Devices :
      declare
         Legacy_Devs : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "devices/device[starts-with(@logical,'serial')]");
         Count       : constant Natural
           := DOM.Core.Nodes.Length (List => Legacy_Devs);
      begin
         if Count > 0 then
            Buffer := Buffer & Utils.Indent (N => 3) & "Device (ISA)"
              & ASCII.LF & Utils.Indent (N => 3) & "{" & ASCII.LF;
         end if;

         for I in 0 .. Count - 1 loop
            Cur_Serial := I;
            Add_Legacy_Device_Resources
              (Legacy_Dev => DOM.Core.Nodes.Item
                 (List  => Legacy_Devs,
                  Index => I));
         end loop;

         if Count > 0 then
            Buffer := Buffer & Utils.Indent (N => 3) & "}" & ASCII.LF;
         end if;

         Buffer := Buffer & Utils.Indent (N => 2);

         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__legacy_devices__",
            Content  => To_String (Buffer));
      end Add_Legacy_Devices;

      Mutools.Templates.Write (Template => Tmpl,
                               Filename => Dsl_File);

      Compile_Dsl (Source => Dsl_File,
                   Target => Filename);
   end Write;

end Acpi.DSDT;
