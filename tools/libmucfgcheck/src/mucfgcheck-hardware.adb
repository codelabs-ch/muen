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

with Ada.Strings.Unbounded;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.XML_Utils;
with Mutools.Constants;

with Mucfgcheck.Utils;
with Mucfgcheck.Validation_Errors;

package body Mucfgcheck.Hardware
is

   use McKae.XML.XPath.XIA;

   --  Checks that devices of given type identified by the specified capability
   --  name exist and declare a device memory resource. Min_Count defines the
   --  minimum, Max_Count the maximum number of devices that must be present in
   --  the system. A Max_Count of 0 means that there is no upper limit.
   procedure Check_Hardware_Device_Presence
     (XML_Data    : Muxml.XML_Data_Type;
      Device_Type : String;
      Cap_Name    : String;
      Min_Count   : Natural;
      Max_Count   : Natural);

   -------------------------------------------------------------------------

   procedure Check_Hardware_Device_Presence
     (XML_Data    : Muxml.XML_Data_Type;
      Device_Type : String;
      Cap_Name    : String;
      Min_Count   : Natural;
      Max_Count   : Natural)
   is
      Devices   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device/capabilities/"
           & "capability[@name='" & Cap_Name & "']/../..");
      Dev_Count : constant Natural := DOM.Core.Nodes.Length (List => Devices);
   begin
      Mulog.Log (Msg => "Checking presence of" & Dev_Count'Img
                 & " " & Device_Type & " memory region(s)");

      if Dev_Count < Min_Count then
         Validation_Errors.Insert
           (Msg => Device_Type & " count is"
            & Dev_Count'Img & " but must be at least" & Min_Count'Img);
      elsif Max_Count /= 0 and then Dev_Count > Max_Count then
         Validation_Errors.Insert
           (Msg => Device_Type & " count is"
            & Dev_Count'Img & " but must not be larger than" & Max_Count'Img);
      end if;

      for I in 0 .. Dev_Count - 1 loop
         declare
            Dev_Node  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Devices,
                 Index => I);
            Dev_Name  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Dev_Node,
                 Name => "name");
            Memory    : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Dev_Node,
                 XPath => "memory");
            Mem_Count : constant Natural
              := DOM.Core.Nodes.Length (List => Memory);
         begin
            if Mem_Count < 1 then
               Validation_Errors.Insert
                 (Msg => Device_Type & " device '"
                  & Dev_Name & "' has no memory region");
            elsif Mem_Count > 1 then
               Validation_Errors.Insert
                 (Msg => Device_Type & " device '"
                  & Dev_Name & "' has multiple memory regions");
            end if;
         end;
      end loop;
   end Check_Hardware_Device_Presence;

   -------------------------------------------------------------------------

   procedure CPU_Count (XML_Data : Muxml.XML_Data_Type)
   is
      Active_CPUs   : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => XML_Data);
      Physical_CPUs : constant Positive
        := Positive'Value
          (Muxml.Utils.Get_Attribute
             (Doc   => XML_Data.Doc,
              XPath => "/system/hardware/processor",
              Name  => "cpuCores"));
   begin
      Mulog.Log (Msg => "Checking CPU core count");

      if Active_CPUs > Physical_CPUs then
         Validation_Errors.Insert
           (Msg => "System requires" & Active_CPUs'Img
            & " but hardware only provides" & Physical_CPUs'Img
            & " CPU(s)");
      end if;
   end CPU_Count;

   -------------------------------------------------------------------------

   procedure CPU_Sub_Elements (XML_Data : Muxml.XML_Data_Type)
   is
      Physical_CPUs : constant Positive
        := Positive'Value
          (Muxml.Utils.Get_Attribute
             (Doc   => XML_Data.Doc,
              XPath => "/system/hardware/processor",
              Name  => "cpuCores"));
      Sub_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/processor/cpu");
      Sub_Node_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Sub_Nodes);
   begin
      Mulog.Log (Msg => "Checking CPU configuration and BSP presence");

      --D @Text Section => system_src.xsd:processorType, Priority => 0
      --D The \texttt{cpu} elements must fulfill the following constraints to
      --D be valid:
      --D @UL Id => cpu_subs_checks, Section => system_src.xsd:processorType, Priority => 0

      --D @Item List => cpu_subs_checks, Priority => 0
      --D A node exists for every physical core of the system

      if Sub_Node_Count /= Physical_CPUs then
         Validation_Errors.Insert
           (Msg => "Hardware processor element requires"
            & Physical_CPUs'Img & " CPU sub-elements, but" & Sub_Node_Count'Img
            & " given");
      end if;

      --D @Item List => cpu_subs_checks, Priority => 0
      --D The optional \texttt{cpuId} attribute of all elements must be
      --D consecutive

      Consecutive_CPU_IDs :
      declare

         --  Returns the error message for a given reference node.
         procedure Error_Msg
           (Node    :     DOM.Core.Node;
            Err_Str : out Ada.Strings.Unbounded.Unbounded_String;
            Fatal   : out Boolean);

         --  Returns True if the left and right numbers are adjacent.
         function Is_Adjacent (Left, Right : DOM.Core.Node) return Boolean;

         -------------------------------------------------------------------

         procedure Error_Msg
           (Node    :     DOM.Core.Node;
            Err_Str : out Ada.Strings.Unbounded.Unbounded_String;
            Fatal   : out Boolean)
         is
            pragma Unreferenced (Node);
         begin
            Err_Str := Ada.Strings.Unbounded.To_Unbounded_String
              ("Processor CPU IDs not consecutive");
            Fatal := False;
         end Error_Msg;

         -------------------------------------------------------------------

         function Is_Adjacent (Left, Right : DOM.Core.Node) return Boolean
         is
         begin
            return Utils.Is_Adjacent_Number
              (Left  => Left,
               Right => Right,
               Attr  => "cpuId");
         end Is_Adjacent;
      begin
         if Sub_Node_Count > 1 then
            For_Each_Match
              (Source_Nodes => Sub_Nodes,
               Ref_Nodes    => Sub_Nodes,
               Log_Message  => "Allocated CPU IDs for consecutiveness",
               Error        => Error_Msg'Access,
               Match        => Is_Adjacent'Access);
         end if;
      end Consecutive_CPU_IDs;

      --D @Item List => cpu_subs_checks, Priority => 0
      --D If specified, a node with \texttt{cpuId} value \texttt{0} must exist

      CPU_ID_0 :
      declare
         use type DOM.Core.Node;

         Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => XML_Data.Doc,
              XPath => "/system/hardware/processor/cpu[@cpuId='0']");
      begin
         if Node = null then
            Validation_Errors.Insert
              (Msg => "CPU sub-element with CPU ID 0 not found");
         end if;
      end CPU_ID_0;

      --D @Item List => cpu_subs_checks, Priority => 0
      --D A node with \texttt{apicId} value \texttt{0} must exist and, if
      --D specified, it must have a \texttt{cpuId} value within the active CPU
      --D range, i.e. the BSP is part of the system scheduling plan

      BSP_Presence :
      declare
         use type DOM.Core.Node;

         Active_CPUs : constant Positive
           := Mutools.XML_Utils.Get_Active_CPU_Count (Data => XML_Data);
         BSP : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => XML_Data.Doc,
              XPath => "/system/hardware/processor/cpu[@apicId='0' and "
              & "@cpuId <" & Active_CPUs'Img & "]");
      begin
         if BSP = null then
            Validation_Errors.Insert
              (Msg => "CPU with APIC ID 0 not present in active CPU set");
         end if;
      end BSP_Presence;

      --D @Item List => cpu_subs_checks, Priority => 0
      --D All \texttt{apicId} attributes must have even numbers

      Even_APIC_ID :
      for I in 0 .. Sub_Node_Count - 1 loop
         declare
            Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Sub_Nodes,
                 Index => I);
            CPU_ID : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Node,
                 Name => "cpuId");
            APIC_ID : constant String
              := DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "apicId");
         begin
            if Natural'Value (APIC_ID) mod 2 /= 0 then
               Validation_Errors.Insert
                 (Msg => "Processor CPU sub-element with "
                  & "CPU ID " & CPU_ID & " has uneven APIC ID " & APIC_ID);
            end if;
         end;
      end loop Even_APIC_ID;
   end CPU_Sub_Elements;

   -------------------------------------------------------------------------

   procedure IOAPIC_Cap_SID (XML_Data : Muxml.XML_Data_Type)
   is
      IOAPICs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device[capabilities/"
           & "capability/@name='ioapic']");
      Count : constant Natural := DOM.Core.Nodes.Length (List => IOAPICs);
   begin
      Mulog.Log (Msg => "Checking Source ID capability for" & Count'Img
                 & " I/O APIC(s)");

      for I in 0 .. Count - 1 loop
         declare
            use type DOM.Core.Node;

            IOAPIC  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => IOAPICs,
                 Index => I);
            Name    : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => IOAPIC,
                 Name => "name");
            SID_Cap : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => IOAPIC,
                 XPath => "capabilities/capability[@name='source_id']");
         begin
            if SID_Cap = null or else DOM.Core.Nodes.Node_Value
              (N => DOM.Core.Nodes.First_Child (N => SID_Cap))'Length = 0
            then
               Validation_Errors.Insert
                 (Msg => "Source ID capability of I/O APIC '"
                  & Name & "' is not set");
               return;
            end if;

            declare
               SID_Str : constant String := DOM.Core.Nodes.Node_Value
                 (N => DOM.Core.Nodes.First_Child (N => SID_Cap));
               Unused_SID_Value : Interfaces.Unsigned_16;
            begin
               Unused_SID_Value := Interfaces.Unsigned_16'Value (SID_Str);

            exception
               when others =>
                  Validation_Errors.Insert
                    (Msg => "Source ID capability of I/O "
                     & "APIC '" & Name & "' set to invalid value '"
                     & SID_Str & "'");
            end;
         end;
      end loop;
   end IOAPIC_Cap_SID;

   -------------------------------------------------------------------------

   procedure IOAPIC_Presence (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Hardware_Device_Presence
        (XML_Data    => XML_Data,
         Device_Type => "I/O APIC",
         Cap_Name    => "ioapic",
         Min_Count   => 1,
         Max_Count   => 0);
   end IOAPIC_Presence;

   -------------------------------------------------------------------------

   procedure IOMMU_Cap_Agaw (XML_Data : Muxml.XML_Data_Type)
   is
      use Ada.Strings.Unbounded;

      IOMMUs    : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device[capabilities/"
           & "capability/@name='iommu']");
      Last_Agaw : Unbounded_String;
   begin
      Mulog.Log (Msg => "Checking AGAW capability for"
                 & DOM.Core.Nodes.Length (List => IOMMUs)'Img & " IOMMU(s)");

      for I in 0 .. DOM.Core.Nodes.Length (IOMMUs) - 1 loop
         declare
            use type DOM.Core.Node;

            IOMMU : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => IOMMUs,
                 Index => I);
            Name  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => IOMMU,
                 Name => "name");
            Agaw  : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => IOMMU,
                 XPath => "capabilities/capability[@name='agaw']");
         begin
            if Agaw = null or else DOM.Core.Nodes.Node_Value
              (N => DOM.Core.Nodes.First_Child (N => Agaw))'Length = 0
            then
               Validation_Errors.Insert
                 (Msg => "AGAW capability of IOMMU '" & Name & "' is not set");
            end if;

            declare
               Agaw_Str : constant String := DOM.Core.Nodes.Node_Value
                 (N => DOM.Core.Nodes.First_Child (N => Agaw));
            begin
               if Agaw_Str /= "39" and then Agaw_Str /= "48" then
                  Validation_Errors.Insert
                    (Msg => "AGAW capability of IOMMU '"
                     & Name & "' set to invalid value '" & Agaw_Str & "'");
               end if;

               if Last_Agaw = Null_Unbounded_String then
                  Last_Agaw := To_Unbounded_String (Agaw_Str);
               elsif Last_Agaw /= Agaw_Str then
                  Validation_Errors.Insert
                    (Msg => "IOMMUs have different AGAW "
                     & "capabilities set ('" & To_String (Last_Agaw)
                     & "' vs. '" & Agaw_Str & "')");
               end if;
            end;
         end;
      end loop;
   end IOMMU_Cap_Agaw;

   -------------------------------------------------------------------------

   procedure IOMMU_Cap_Register_Offsets (XML_Data : Muxml.XML_Data_Type)
   is
      IOMMUs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device[capabilities/"
           & "capability/@name='iommu']");
   begin
      Mulog.Log (Msg => "Checking register offset capabilities for"
                 & DOM.Core.Nodes.Length (List => IOMMUs)'Img & " IOMMU(s)");

      for I in 0 .. DOM.Core.Nodes.Length (IOMMUs) - 1 loop
         declare
            use type DOM.Core.Node;

            IOMMU : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => IOMMUs,
                 Index => I);
            Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => IOMMU,
                 Name => "name");
            Fro_Cap : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => IOMMU,
                 XPath => "capabilities/capability[@name='fr_offset']");
            Iotlb_Inv_Cap : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => IOMMU,
                 XPath => "capabilities/capability"
                 & "[@name='iotlb_invalidate_offset']");

            type Fr_Offset_Range        is range 0 .. (2 ** 10 - 1) * 16;
            type Iotlb_Inv_Offset_Range is range 0 .. (2 ** 10 - 1) * 16 + 8;

            Fr_Offset_Value_Unused        : Fr_Offset_Range;
            Iotlb_Inv_Offset_Value_Unused : Iotlb_Inv_Offset_Range;
         begin
            if Fro_Cap = null or else DOM.Core.Nodes.Node_Value
              (N => DOM.Core.Nodes.First_Child (N => Fro_Cap))'Length = 0
            then
               Validation_Errors.Insert
                 (Msg => "Capability 'fr_offset' of IOMMU '"
                  & Name & "' is not set");
            end if;

            begin
               Fr_Offset_Value_Unused := Fr_Offset_Range'Value
                 (DOM.Core.Nodes.Node_Value
                    (N => DOM.Core.Nodes.First_Child (N => Fro_Cap)));

            exception
               when others =>
                  Validation_Errors.Insert
                    (Msg => "Capability 'fr_offset' of "
                     & "IOMMU '" & Name & "' not in allowed range"
                     & Fr_Offset_Range'First'Img & " .."
                     & Fr_Offset_Range'Last'Img);
            end;

            if Iotlb_Inv_Cap = null or else DOM.Core.Nodes.Node_Value
              (N => DOM.Core.Nodes.First_Child (N => Iotlb_Inv_Cap))'Length = 0
            then
               Validation_Errors.Insert
                 (Msg => "Capability "
                  & "'iotlb_invalidate_offset' of IOMMU '" & Name
                  & "' is not set");
            end if;

            begin
               Iotlb_Inv_Offset_Value_Unused := Iotlb_Inv_Offset_Range'Value
                 (DOM.Core.Nodes.Node_Value
                    (N => DOM.Core.Nodes.First_Child (N => Iotlb_Inv_Cap)));

            exception
               when others =>
                  Validation_Errors.Insert
                    (Msg => "Capability "
                     & "'iotlb_invalidate_offset' of IOMMU '" & Name
                     & "' not in allowed range"
                     & Iotlb_Inv_Offset_Range'First'Img & " .."
                     & Iotlb_Inv_Offset_Range'Last'Img);
            end;
         end;
      end loop;
   end IOMMU_Cap_Register_Offsets;

   -------------------------------------------------------------------------

   procedure IOMMU_Presence (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Hardware_Device_Presence
        (XML_Data    => XML_Data,
         Device_Type => "IOMMU",
         Cap_Name    => "iommu",
         Min_Count   => 1,
         Max_Count   => 8);
   end IOMMU_Presence;

   -------------------------------------------------------------------------

   procedure Memory_Block_Overlap (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/hardware/memory/memoryBlock");
   begin
      Check_Memory_Overlap (Nodes        => Nodes,
                            Region_Type  => "hardware memory block",
                            Address_Attr => "physicalAddress");
   end Memory_Block_Overlap;

   -------------------------------------------------------------------------

   procedure Memory_Block_Size (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/hardware/memory/memoryBlock");
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "hardware memory block",
                       Attr      => "size",
                       Name_Attr => "name",
                       Test      => Mod_Equal_Zero'Access,
                       B         => Mutools.Constants.Page_Size,
                       Error_Msg => "not multiple of page size (4K)");
   end Memory_Block_Size;

   -------------------------------------------------------------------------

   procedure Memory_Space (XML_Data : Muxml.XML_Data_Type)
   is
      Blocks    : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/hardware/memory/memoryBlock/@size");
      Mems      : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory/@size");
      Block_Sum : constant Interfaces.Unsigned_64
        := Muxml.Utils.Sum
          (Nodes  => Blocks,
           Getter => DOM.Core.Nodes.Node_Value'Access);
      Mem_Sum   : constant Interfaces.Unsigned_64
        := Muxml.Utils.Sum
          (Nodes  => Mems,
           Getter => DOM.Core.Nodes.Node_Value'Access);
   begin
      Mulog.Log (Msg => "Checking size of" & DOM.Core.Nodes.Length
                 (List => Mems)'Img & " physical memory region(s) against"
                 & DOM.Core.Nodes.Length (List => Blocks)'Img
                 & " memory block(s)");

      if Mem_Sum > Block_Sum then
         Validation_Errors.Insert
           (Msg => "Allocated " & Mutools.Utils.To_Hex
              (Number => Mem_Sum) & " bytes of physical memory but only "
            & Mutools.Utils.To_Hex (Number => Block_Sum)
            & " bytes available by the hardware");
      end if;
   end Memory_Space;

   -------------------------------------------------------------------------

   procedure PCI_Config_Space_Address (XML_Data : Muxml.XML_Data_Type)
   is
      Cfg_Address   : constant String := Muxml.Utils.Get_Attribute
        (Doc   => XML_Data.Doc,
         XPath => "/system/hardware/devices",
         Name  => "pciConfigAddress");
      PCI_Dev_Count : constant Natural
        := DOM.Core.Nodes.Length
          (List => XPath_Query
             (N     => XML_Data.Doc,
              XPath => "/system/hardware/devices/device/pci"));
   begin
      Mulog.Log (Msg => "Checking PCI configuration space address");
      if PCI_Dev_Count > 0 and then Cfg_Address'Length = 0 then
         Validation_Errors.Insert
           (Msg => "Missing PCI configuration space address");
      end if;
   end PCI_Config_Space_Address;

   -------------------------------------------------------------------------

   procedure System_Board_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      Devices : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device[capabilities/"
           & "capability/@name='systemboard']");
   begin
      Mulog.Log (Msg => "Checking presence of system board device");

      if DOM.Core.Nodes.Length (List => Devices) = 1 then
         declare
            Dev : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Devices,
                 Index => 0);
            Ports : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Dev,
                 XPath => "ioPort[(@start='16#0cf9#' and @end='16#0cf9#') or "
                 & "@name='pm1a_cnt']");
            Caps : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Dev,
                 XPath => "capabilities/capability[@name='pm1a_cnt_slp_typ']");
         begin
            if DOM.Core.Nodes.Length (List => Ports) = 2
              or else DOM.Core.Nodes.Length (List => Caps) = 1
            then
               return;
            end if;
         end;
      end if;

      Validation_Errors.Insert
        (Msg => "System board device with reset/poweroff configuration "
         & "missing or incomplete");
   end System_Board_Presence;

end Mucfgcheck.Hardware;
