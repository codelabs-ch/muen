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

with Interfaces;

with Ada.Containers.Ordered_Sets;

with DOM.Core;

with Muxml;
with Mutools.Strings;

package Mutools.XML_Utils
is

   --  Add physical device resource designated by Physical_Resource node to
   --  specified logical device with given logical resource name. If no name is
   --  specified, the logical name is set to the physical resource name.
   --  The logical address of the memory resources is set if
   --  Set_Logical_Mem_Addr is True.
   procedure Add_Resource
     (Logical_Device         : DOM.Core.Node;
      Physical_Resource      : DOM.Core.Node;
      Logical_Resource_Name  : String                 := "";
      Mmconf_Devices_Node    : DOM.Core.Node          := null;
      Mmconf_Device_PCI_Node : DOM.Core.Node          := null;
      Mmconf_Virt_Base       : Interfaces.Unsigned_64 := 0;
      Set_Logical_Mem_Addr   : Boolean                := True);

   --  Add physical memory region element with given parameters to policy.
   procedure Add_Memory_Region
     (Policy      : in out Muxml.XML_Data_Type;
      Name        :        String;
      Address     :        String;
      Size        :        String;
      Caching     :        String;
      Alignment   :        String;
      Memory_Type :        String);

   --  Add file-backed physical memory region element with given parameters to
   --  policy.
   procedure Add_Memory_Region
     (Policy      : in out Muxml.XML_Data_Type;
      Name        :        String;
      Address     :        String;
      Size        :        String;
      Caching     :        String;
      Alignment   :        String;
      Memory_Type :        String;
      File_Name   :        String;
      File_Offset :        String);

   --  Add pattern-filled physical memory region element with given parameters
   --  to policy.
   procedure Add_Memory_Region
     (Policy       : in out Muxml.XML_Data_Type;
      Name         :        String;
      Address      :        String;
      Size         :        String;
      Caching      :        String;
      Alignment    :        String;
      Memory_Type  :        String;
      Fill_Pattern :        String);

   --  Create memory node element with given parameters.
   function Create_Memory_Node
     (Policy      : in out Muxml.XML_Data_Type;
      Name        :        String;
      Address     :        String;
      Size        :        String;
      Caching     :        String;
      Alignment   :        String;
      Memory_Type :        String)
      return DOM.Core.Node;

   --  Create virtual memory node with given parameters.
   function Create_Virtual_Memory_Node
     (Policy        : in out Muxml.XML_Data_Type;
      Logical_Name  :        String;
      Physical_Name :        String;
      Address       :        String;
      Writable      :        Boolean;
      Executable    :        Boolean)
      return DOM.Core.Node;

   --  Create component memory node.
   function Create_Component_Memory_Node
     (Policy       : in out Muxml.XML_Data_Type;
      Logical_Name :        String;
      Address      :        String;
      Size         :        String;
      Executable   :        Boolean;
      Writable     :        Boolean)
      return DOM.Core.Node;

   --  Returns the node of the logical memory region that encloses the given
   --  virtual address. Null is returned if the virtual address is not located
   --  in any of the given logical regions.
   function Get_Enclosing_Virtual_Region
     (Virtual_Address : Interfaces.Unsigned_64;
      Physical_Memory : DOM.Core.Node_List;
      Logical_Memory  : DOM.Core.Node_List)
      return DOM.Core.Node;

   --  Returns True if the given VMX controls specify that the DEBUGCTL MSR is
   --  saved/loaded automatically on VM-exits and entries.
   function Has_Managed_DEBUGCTL (Controls : DOM.Core.Node) return Boolean;

   --  Returns True if the given VMX controls specify that the
   --  PERFGLOBALCTRL MSR is loaded automatically on VM-entries.
   function Has_Managed_PERFGLOBALCTRL
     (Controls : DOM.Core.Node)
      return Boolean;

   --  Returns True if the given VMX controls specify that the PAT MSR is
   --  saved/loaded automatically on VM-exits and entries.
   function Has_Managed_PAT (Controls : DOM.Core.Node) return Boolean;

   --  Returns True if the given VMX controls specify that the EFER MSR is
   --  saved/loaded automatically on VM-exits and entries.
   function Has_Managed_EFER (Controls : DOM.Core.Node) return Boolean;

   --  Returns the number of Model-Specific registers that must be managed by
   --  the MSR store mechanism given the list of MSR nodes and considering the
   --  specified control flags.
   function Calculate_MSR_Count
     (MSRs                   : DOM.Core.Node_List;
      DEBUGCTL_Control       : Boolean;
      PAT_Control            : Boolean;
      PERFGLOBALCTRL_Control : Boolean;
      EFER_Control           : Boolean)
      return Natural;

   type PCI_Bus_Range is range 0 .. 255;

   package PCI_Bus_Set is new Ada.Containers.Ordered_Sets
     (Element_Type => PCI_Bus_Range);

   --  Return set of occupied PCI bus numbers for given system policy.
   function Get_Occupied_PCI_Buses
     (Data : Muxml.XML_Data_Type)
      return PCI_Bus_Set.Set;

   --  Return the list of subjects that can trigger a switch to the given
   --  target subject.
   function Get_Switch_Sources
     (Data   : Muxml.XML_Data_Type;
      Target : DOM.Core.Node)
      return DOM.Core.Node_List;

   --  Returns the number of CPUs that are active in a given system policy.
   function Get_Active_CPU_Count (Data : Muxml.XML_Data_Type) return Positive;

   --  Returns the ID of the CPU that can execute the specified subject of a
   --  given XML policy. If no CPU can execute the given subject -1 is
   --  returned.
   function Get_Executing_CPU
     (Data    : Muxml.XML_Data_Type;
      Subject : DOM.Core.Node)
      return Integer;

   --  Returns True if the given node references a PCI device.
   function Is_PCI_Device_Reference
     (Data       : Muxml.XML_Data_Type;
      Device_Ref : DOM.Core.Node)
      return Boolean;

   --  Minor frame and its exit time in ticks measured from the start of the
   --  corresponding major frame.
   type Deadline_Type is record
      Exit_Time   : Interfaces.Unsigned_64;
      Minor_Frame : DOM.Core.Node;
   end record;

   type Deadline_Array is array (Positive range <>) of Deadline_Type;

   --  Returns the minor frame deadlines for the given major frame.
   function Get_Minor_Frame_Deadlines
     (Major : DOM.Core.Node)
      return Deadline_Array;

   subtype IOMMU_Paging_Level is Positive range 3 .. 4;

   --  Return supported paging-levels of IOMMUs. Since all IOMMUs must have the
   --  same AGAW capability, the paging-levels of the first IOMMU is returned.
   --  For an explanation of the IOMMU AGAW support and levels of page-table
   --  walks see the Intel VT-d specification, section 10.4.2, figure 10-45.
   function Get_IOMMU_Paging_Levels
     (Data : Muxml.XML_Data_Type)
      return IOMMU_Paging_Level;

   --  Legacy IRQ range (PIC cascade IRQ 2 excluded).
   type Legacy_IRQ_Range is range 0 .. 23
     with Static_Predicate => Legacy_IRQ_Range /= 2;

   --  I/O APIC RTE index.
   type IOAPIC_RTE_Range is range 1 .. 23;

   --  Return I/O APIC RTE index for given legacy IRQ. See Intel 82093AA I/O
   --  Advanced Programmable Interrupt Controller (IOAPIC) specification,
   --  section 2.4 for more details.
   function Get_IOAPIC_RTE_Idx
     (IRQ : Legacy_IRQ_Range)
      return IOAPIC_RTE_Range;

   --  Supported IRQ types.
   type IRQ_Kind is
     (IRQ_ISA,
      IRQ_PCI_LSI,
      IRQ_PCI_MSI);

   --  Return IRQ kind supported by device given as node.
   function Get_IRQ_Kind (Dev : DOM.Core.Node) return IRQ_Kind;

   --  Returns the list of PCI device nodes sorted by BDF in ascending order.
   function Sort_By_BDF
     (PCI_Devs : DOM.Core.Node_List)
      return DOM.Core.Node_List;

   --  Set size of given virtual memory node by looking up the corresponding
   --  physical memory region present in the 'Ref_Nodes' list.
   procedure Set_Memory_Size
     (Virtual_Mem_Node : DOM.Core.Node;
      Ref_Nodes        : DOM.Core.Node_List);

   --  Set size of given virtual memory nodes by looking up the corresponding
   --  physical memory regions present in the 'Ref_Nodes' list.
   procedure Set_Memory_Size
     (Virtual_Mem_Nodes : DOM.Core.Node_List;
      Ref_Nodes         : DOM.Core.Node_List);

   --  Array type used to specify subject/scheduling group ID mappings.
   type ID_Map_Array is array (Natural range <>) of Natural;

   No_Group : constant Natural := 0;

   --  Returns an array that represents the mapping of scheduling group to
   --  initial subject ID.
   function Get_Initial_Scheduling_Group_Subjects
     (Data : Muxml.XML_Data_Type)
      return ID_Map_Array;

   Missing_Subject    : exception;
   Invalid_Subject_ID : exception;

   --  Returns an array that represent the mapping of subject to scheduling
   --  group ID.
   function Get_Subject_To_Scheduling_Group_Map
     (Data : Muxml.XML_Data_Type)
      return ID_Map_Array;

   --  Process XML Inclusions in the given XML policy. Inclusions are searched
   --  relative to the given include directories.
   procedure Merge_XIncludes
     (Policy       : in out Muxml.XML_Data_Type;
      Include_Dirs :        Strings.String_Array);

   --  Return size of image specified by policy in bytes.
   function Get_Image_Size
     (Policy : Muxml.XML_Data_Type)
      return Interfaces.Unsigned_64;

   --  Calculate the PCI config space window address of the device with BDF as
   --  specified by the PCI node and the given base address.
   function Calculate_PCI_Cfg_Address
     (Base_Address : Interfaces.Unsigned_64;
      PCI_Node     : DOM.Core.Node)
      return Interfaces.Unsigned_64;

   --  Return True if the given address is within the PCI configuration space
   --  of the specified devices node, otherwise return False. Also return
   --  False if the devices node does not define a PCI configuration space base
   --  address.
   function Is_Physical_Mmconf_Region
     (Devices_Node : DOM.Core.Node;
      Addr         : Interfaces.Unsigned_64)
      return Boolean;

end Mutools.XML_Utils;
