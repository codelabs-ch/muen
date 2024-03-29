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
with Ada.Containers.Ordered_Multisets;
with Ada.Containers.Generic_Constrained_Array_Sort;

with DOM.Core.Nodes;
with DOM.Core.Documents.Local;
with DOM.Core.Elements;
with DOM.Core.Append_Node;

with McKae.XML.XPath.XIA;

with Muxml.Utils;

with Mutools.Utils;

package body Mutools.XML_Utils
is

   --  Create virtual memory node with given parameters.
   function Create_Virtual_Memory_Node
     (Policy        : in out Muxml.XML_Data_Type;
      Logical_Name  :        String;
      Address       :        String;
      Writable      :        Boolean;
      Executable    :        Boolean)
      return DOM.Core.Node;

   -------------------------------------------------------------------------

   procedure Add_Memory_Region
     (Policy      : in out Muxml.XML_Data_Type;
      Name        :        String;
      Address     :        String;
      Size        :        String;
      Caching     :        String;
      Alignment   :        String;
      Memory_Type :        String)
   is
      Section     : constant DOM.Core.Node := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/memory");
   begin
      Muxml.Utils.Append_Child
        (Node      => Section,
         New_Child => Create_Memory_Node
           (Policy      => Policy,
            Name        => Name,
            Address     => Address,
            Size        => Size,
            Caching     => Caching,
            Alignment   => Alignment,
            Memory_Type => Memory_Type));
   end Add_Memory_Region;

   -------------------------------------------------------------------------

   procedure Add_Memory_Region
     (Policy      : in out Muxml.XML_Data_Type;
      Name        :        String;
      Address     :        String;
      Size        :        String;
      Caching     :        String;
      Alignment   :        String;
      Memory_Type :        String;
      File_Name   :        String;
      File_Offset :        String;
      File_Size   :        String := "")
   is
      Section   : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Policy.Doc,
           XPath => "/system/memory");
      Mem_Node  : constant DOM.Core.Node
        := Create_Memory_Node
          (Policy      => Policy,
           Name        => Name,
           Address     => Address,
           Size        => Size,
           Caching     => Caching,
           Alignment   => Alignment,
           Memory_Type => Memory_Type);
      File_Node : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Policy.Doc,
           Tag_Name => "file");
   begin
      Muxml.Utils.Append_Child (Node      => Section,
                                New_Child => Mem_Node);
      Muxml.Utils.Append_Child (Node      => Mem_Node,
                                New_Child => File_Node);

      DOM.Core.Elements.Set_Attribute
        (Elem  => File_Node,
         Name  => "filename",
         Value => File_Name);
      DOM.Core.Elements.Set_Attribute
        (Elem  => File_Node,
         Name  => "offset",
         Value => File_Offset);
      if File_Size'Length > 0 then
         DOM.Core.Elements.Set_Attribute
           (Elem  => File_Node,
            Name  => "size",
            Value => File_Size);
      end if;
   end Add_Memory_Region;

   -------------------------------------------------------------------------

   procedure Add_Memory_Region
     (Policy       : in out Muxml.XML_Data_Type;
      Name         :        String;
      Address      :        String;
      Size         :        String;
      Caching      :        String;
      Alignment    :        String;
      Memory_Type  :        String;
      Fill_Pattern :        String)
   is
      Section   : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Policy.Doc,
           XPath => "/system/memory");
      Mem_Node  : constant DOM.Core.Node
        := Create_Memory_Node
          (Policy      => Policy,
           Name        => Name,
           Address     => Address,
           Size        => Size,
           Caching     => Caching,
           Alignment   => Alignment,
           Memory_Type => Memory_Type);
      Fill_Node : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Policy.Doc,
           Tag_Name => "fill");
   begin
      Muxml.Utils.Append_Child (Node      => Section,
                                New_Child => Mem_Node);
      Muxml.Utils.Append_Child (Node      => Mem_Node,
                                New_Child => Fill_Node);

      DOM.Core.Elements.Set_Attribute
        (Elem  => Fill_Node,
         Name  => "pattern",
         Value => Fill_Pattern);
   end Add_Memory_Region;

   -------------------------------------------------------------------------

   function Add_Resource
     (Logical_Device         : DOM.Core.Node;
      Physical_Resource      : DOM.Core.Node;
      Logical_Resource_Name  : String                 := "";
      Mmconf_Devices_Node    : DOM.Core.Node          := null;
      Mmconf_Device_PCI_Node : DOM.Core.Node          := null;
      Mmconf_Virt_Base       : Interfaces.Unsigned_64 := 0;
      Set_Logical_Mem_Addr   : Boolean                := True)
      return DOM.Core.Node
   is
      Owner_Doc : constant DOM.Core.Document
        := DOM.Core.Nodes.Owner_Document (N => Logical_Device);
      Res_Name  : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Physical_Resource,
           Name => "name");
      Res_Type  : constant String
        := DOM.Core.Nodes.Node_Name (N => Physical_Resource);
      Res_Ref   : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Owner_Doc,
           Tag_Name => Res_Type);
      Log_Name  : constant String
        := (if Logical_Resource_Name'Length > 0 then Logical_Resource_Name
            else Res_Name);
   begin
      DOM.Core.Elements.Set_Attribute
        (Elem  => Res_Ref,
         Name  => "physical",
         Value => Res_Name);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Res_Ref,
         Name  => "logical",
         Value => Log_Name);

      if Res_Type = "memory" then
         if Set_Logical_Mem_Addr then
            declare
               Phys_Addr    : constant Interfaces.Unsigned_64
                 := Interfaces.Unsigned_64'Value
                   (DOM.Core.Elements.Get_Attribute
                        (Elem => Physical_Resource,
                         Name => "physicalAddress"));
               Mapping_Addr : Interfaces.Unsigned_64 := Phys_Addr;
            begin

               --  Mmconf regions are mapped to virtual PCI config space, other
               --  regions are identity mapped.

               if Is_Physical_Mmconf_Region
                 (Devices_Node => Mmconf_Devices_Node,
                  Addr         => Phys_Addr)
               then
                  Mapping_Addr := Calculate_PCI_Cfg_Address
                    (Base_Address => Mmconf_Virt_Base,
                     PCI_Node     => Mmconf_Device_PCI_Node);
               end if;

               DOM.Core.Elements.Set_Attribute
                 (Elem  => Res_Ref,
                  Name  => "virtualAddress",
                  Value => Mutools.Utils.To_Hex (Number => Mapping_Addr));
            end;
         end if;

         DOM.Core.Elements.Set_Attribute
           (Elem  => Res_Ref,
            Name  => "writable",
            Value => "true");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Res_Ref,
            Name  => "executable",
            Value => "false");
      end if;

      return DOM.Core.Nodes.Append_Child
        (N         => Logical_Device,
         New_Child => Res_Ref);
   end Add_Resource;

   -------------------------------------------------------------------------

   procedure Add_Resource
     (Logical_Device         : DOM.Core.Node;
      Physical_Resource      : DOM.Core.Node;
      Logical_Resource_Name  : String                 := "";
      Mmconf_Devices_Node    : DOM.Core.Node          := null;
      Mmconf_Device_PCI_Node : DOM.Core.Node          := null;
      Mmconf_Virt_Base       : Interfaces.Unsigned_64 := 0;
      Set_Logical_Mem_Addr   : Boolean                := True)
   is
      Dummy : DOM.Core.Node;
   begin
      Dummy := Add_Resource
        (Logical_Device         => Logical_Device,
         Physical_Resource      => Physical_Resource,
         Logical_Resource_Name  => Logical_Resource_Name,
         Mmconf_Devices_Node    => Mmconf_Devices_Node,
         Mmconf_Device_PCI_Node => Mmconf_Device_PCI_Node,
         Mmconf_Virt_Base       => Mmconf_Virt_Base,
         Set_Logical_Mem_Addr   => Set_Logical_Mem_Addr);
   end Add_Resource;

   -------------------------------------------------------------------------

   function Calculate_MSR_Count
     (MSRs                   : DOM.Core.Node_List;
      DEBUGCTL_Control       : Boolean;
      PAT_Control            : Boolean;
      PERFGLOBALCTRL_Control : Boolean;
      EFER_Control           : Boolean)
      return Natural
   is
      MSR_Count : Natural := 0;
   begin
      for J in 0 .. DOM.Core.Nodes.Length (List => MSRs) - 1 loop
         declare
            MSR_Node  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => MSRs,
                                      Index => J);
            MSR_Start : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => MSR_Node,
                    Name => "start"));
            MSR_End   : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => MSR_Node,
                    Name => "end"));
         begin
            for Register in MSR_Start .. MSR_End loop
               if not Utils.Is_Managed_By_VMX
                 (MSR                    => Register,
                  DEBUGCTL_Control       => DEBUGCTL_Control,
                  PAT_Control            => PAT_Control,
                  PERFGLOBALCTRL_Control => PERFGLOBALCTRL_Control,
                  EFER_Control           => EFER_Control)
                 and not Utils.Is_Global_MSR (MSR => Register)
               then
                  MSR_Count := MSR_Count + 1;
               end if;
            end loop;
         end;
      end loop;

      return MSR_Count;
   end Calculate_MSR_Count;

   -------------------------------------------------------------------------

   function Calculate_PCI_Cfg_Address
     (Base_Address : Interfaces.Unsigned_64;
      PCI_Node     : DOM.Core.Node)
      return Interfaces.Unsigned_64
   is
      use type Interfaces.Unsigned_64;

      Bus_Nr    : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => PCI_Node,
              Name => "bus"));
      Device_Nr : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => PCI_Node,
              Name => "device"));
      Func_Nr   : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => PCI_Node,
              Name => "function"));
   begin
      return Base_Address +
        (Bus_Nr * 2 ** 20 + Device_Nr * 2 ** 15 + Func_Nr * 2 ** 12);
   end Calculate_PCI_Cfg_Address;

   -------------------------------------------------------------------------

   function Create_Component_Memory_Node
     (Policy       : in out Muxml.XML_Data_Type;
      Logical_Name :        String;
      Address      :        String;
      Size         :        String;
      Executable   :        Boolean;
      Writable     :        Boolean)
      return DOM.Core.Node
   is
      Mem_Node : constant DOM.Core.Node
        := Create_Virtual_Memory_Node
          (Policy        => Policy,
           Logical_Name  => Logical_Name,
           Address       => Address,
           Writable      => Writable,
           Executable    => Executable);
   begin
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "size",
         Value => Size);
      return Mem_Node;
   end Create_Component_Memory_Node;

   -------------------------------------------------------------------------

   function Create_Memory_Node
     (Policy      : in out Muxml.XML_Data_Type;
      Name        :        String;
      Address     :        String;
      Size        :        String;
      Caching     :        String;
      Alignment   :        String;
      Memory_Type :        String)
      return DOM.Core.Node
   is
      Mem_Node : constant DOM.Core.Node := DOM.Core.Documents.Create_Element
        (Doc      => Policy.Doc,
         Tag_Name => "memory");
   begin
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "name",
         Value => Name);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "size",
         Value => Size);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "caching",
         Value => Caching);

      if Alignment'Length > 0 then
         DOM.Core.Elements.Set_Attribute
           (Elem  => Mem_Node,
            Name  => "alignment",
            Value => Alignment);
      end if;

      if Address'Length > 0 then
         DOM.Core.Elements.Set_Attribute
           (Elem  => Mem_Node,
            Name  => "physicalAddress",
            Value => Address);
      end if;

      if Memory_Type'Length > 0 then
         DOM.Core.Elements.Set_Attribute
           (Elem  => Mem_Node,
            Name  => "type",
            Value => Memory_Type);
      end if;

      return Mem_Node;
   end Create_Memory_Node;

   -------------------------------------------------------------------------

   function Create_Virtual_Memory_Node
     (Policy        : in out Muxml.XML_Data_Type;
      Logical_Name  :        String;
      Address       :        String;
      Writable      :        Boolean;
      Executable    :        Boolean)
      return DOM.Core.Node
   is
      Mem_Node : constant DOM.Core.Node := DOM.Core.Documents.Create_Element
        (Doc      => Policy.Doc,
         Tag_Name => "memory");
   begin
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "logical",
         Value => Logical_Name);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "virtualAddress",
         Value => Address);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "writable",
         Value => (if Writable then "true" else "false"));
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "executable",
         Value => (if Executable then "true" else "false"));

      return Mem_Node;
   end Create_Virtual_Memory_Node;

   -------------------------------------------------------------------------

   function Create_Virtual_Memory_Node
     (Policy        : in out Muxml.XML_Data_Type;
      Logical_Name  :        String;
      Physical_Name :        String;
      Address       :        String;
      Writable      :        Boolean;
      Executable    :        Boolean)
      return DOM.Core.Node
   is
      Mem_Node : constant DOM.Core.Node
        := Create_Virtual_Memory_Node
          (Policy       => Policy,
           Logical_Name => Logical_Name,
           Address      => Address,
           Writable     => Writable,
           Executable   => Executable);
   begin
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "physical",
         Value => Physical_Name);
      return Mem_Node;
   end Create_Virtual_Memory_Node;

   -------------------------------------------------------------------------

   function Equal_BDFs (Left, Right : DOM.Core.Node) return Boolean
   is
      Left_Bus  : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Left,
         Name => "bus");
      Left_Dev  : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Left,
         Name => "device");
      Left_Fn   : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Left,
         Name => "function");
      Right_Bus : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Right,
         Name => "bus");
      Right_Dev : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Right,
         Name => "device");
      Right_Fn  : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Right,
         Name => "function");
   begin
      return Left_Bus = Right_Bus
        and then Left_Dev = Right_Dev
        and then Left_Fn  = Right_Fn;
   end Equal_BDFs;

   -------------------------------------------------------------------------

   function Get_Active_CPU_Count (Data : Muxml.XML_Data_Type) return Positive
   is
      First_Major_Frame : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/scheduling/majorFrame");
      CPUs              : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => First_Major_Frame,
           XPath => "cpu");
   begin
      return DOM.Core.Nodes.Length (List => CPUs);
   end Get_Active_CPU_Count;

   -------------------------------------------------------------------------

   function Get_Enclosing_Virtual_Region
     (Virtual_Address : Interfaces.Unsigned_64;
      Physical_Memory : DOM.Core.Node_List;
      Logical_Memory  : DOM.Core.Node_List)
      return DOM.Core.Node
   is
      use type Interfaces.Unsigned_64;

      Log_Mem_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Logical_Memory);
   begin
      for I in 0 .. Log_Mem_Count - 1 loop
         declare
            Log_Mem_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Logical_Memory,
                                      Index => I);
            Log_Addr     : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Log_Mem_Node,
                    Name => "virtualAddress"));
         begin
            if Log_Addr <= Virtual_Address then
               declare
                  Phys_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Log_Mem_Node,
                       Name => "physical");
                  Size_Str  : constant String
                    := Muxml.Utils.Get_Attribute
                         (Nodes     => Physical_Memory,
                          Ref_Attr  => "name",
                          Ref_Value => Phys_Name,
                          Attr_Name => "size");
               begin
                  if Size_Str'Length > 0 and
                  then Log_Addr + Interfaces.Unsigned_64'Value
                    (Size_Str) > Virtual_Address
                  then
                     return Log_Mem_Node;
                  end if;
               end;
            end if;
         end;
      end loop;

      return null;
   end Get_Enclosing_Virtual_Region;

   -------------------------------------------------------------------------

   function Get_Initial_Scheduling_Group_Subjects
     (Data : Muxml.XML_Data_Type)
      return ID_Map_Array
   is
      Subjects      : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
      Subject_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Subjects);

      Scheduling_Groups : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/scheduling/partitions/partition/group");
      Sched_Group_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Scheduling_Groups);

      subtype Subject_ID_Range is Natural range 0 .. Subject_Count - 1;
      subtype Sched_Group_ID_Range is Natural range 1 .. Sched_Group_Count;

      Group_To_Subject_ID_Map : ID_Map_Array (Sched_Group_ID_Range)
        := (others => Natural'Last);
   begin
      for I in 1 .. DOM.Core.Nodes.Length (List => Scheduling_Groups) loop
         declare
            Sched_Group : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Scheduling_Groups,
                                      Index => I - 1);

            --  Name of first element, i.e. initially scheduled subject.
            Subject_Name : constant String
              := Muxml.Utils.Get_Attribute (Doc   => Sched_Group,
                                            XPath => "subject",
                                            Name  => "name");
            Subject_ID_Str : constant String
              := Muxml.Utils.Get_Attribute
                (Nodes     => Subjects,
                 Ref_Attr  => "name",
                 Ref_Value => Subject_Name,
                 Attr_Name => "globalId");
            Subject_ID : Natural;
         begin
            if Subject_ID_Str'Length = 0 then
               raise Missing_Subject with "Subject '" & Subject_Name
                 & "' referenced in scheduling plan not present";
            end if;

            Subject_ID := Natural'Value (Subject_ID_Str);
            if Subject_ID not in Subject_ID_Range then
               raise Invalid_Subject_ID with "Subject '" & Subject_Name
                 & "' referenced in scheduling plan has invalid ID "
                 & Subject_ID_Str & ", not in range"
                 & Subject_ID_Range'First'Img & ".."
                 & Subject_ID_Range'Last'Img;
            end if;
            Group_To_Subject_ID_Map (I) := Natural'Value (Subject_ID_Str);
         end;
      end loop;

      return Group_To_Subject_ID_Map;
   end Get_Initial_Scheduling_Group_Subjects;

   -------------------------------------------------------------------------

   function Get_IOAPIC_RTE_Idx
     (IRQ : IOAPIC_IRQ_Range)
      return IOAPIC_RTE_Range
   is
      Res : IOAPIC_RTE_Range;
   begin
      case IRQ is
         when 0      => Res := 2;
         when others => Res := IOAPIC_RTE_Range (IRQ);
      end case;

      return Res;
   end Get_IOAPIC_RTE_Idx;

   -------------------------------------------------------------------------

   function Get_IOAPIC_RTE_Index_Max
     (Data : Muxml.XML_Data_Type)
      return Natural
   is
      IOAPICs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/hardware/devices/device/capabilities/"
           & "capability[@name='ioapic']/..");
      Max_RTE_Idx : Natural := 0;
   begin
      for I in Natural range 0 .. DOM.Core.Nodes.Length (List => IOAPICs) - 1
      loop
         declare
            IOAPIC_Caps : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => IOAPICs,
                                      Index => I);
            GSI_Base : constant Natural
              := Natural'Value
                (Muxml.Utils.Get_Element_Value
                                (Doc   => IOAPIC_Caps,
                                 XPath => "capability[@name='gsi_base']"));
            Max_Redir : constant Natural
              := Natural'Value
                (Muxml.Utils.Get_Element_Value
                   (Doc   => IOAPIC_Caps,
                    XPath => "capability[@name='max_redirection_entry']"));
         begin
            Max_RTE_Idx := Natural'Max (Max_RTE_Idx, GSI_Base + Max_Redir);
         end;
      end loop;

      if Max_RTE_Idx = 0 then
         raise IOAPIC_Not_Found with "Unable to determine maximum I/O APIC RTE"
           & " index: no I/O APICs found";
      end if;

      return Max_RTE_Idx;
   end Get_IOAPIC_RTE_Index_Max;

   -------------------------------------------------------------------------

   function Get_IOMMU_Paging_Levels
     (Data : Muxml.XML_Data_Type)
      return IOMMU_Paging_Level
   is
      Agaw   : constant String
        := Muxml.Utils.Get_Element_Value
          (Doc   => Data.Doc,
           XPath => "/system/hardware/devices/device/capabilities/"
           & "capability[@name='iommu']/../capability[@name='agaw']");
      Levels : IOMMU_Paging_Level := 3;
   begin
      if Agaw = "48" then
         Levels := 4;
      end if;

      return Levels;
   end Get_IOMMU_Paging_Levels;

   -------------------------------------------------------------------------

   function Get_IRQ_Kind (Dev : DOM.Core.Node) return IRQ_Kind
   is
      MSI : constant String := Muxml.Utils.Get_Attribute
        (Doc   => Dev,
         XPath => "pci",
         Name  => "msi");
   begin
      if MSI'Length = 0 then
         return IRQ_ISA;
      end if;

      if MSI = "true" then
         return IRQ_PCI_MSI;
      else
         return IRQ_PCI_LSI;
      end if;
   end Get_IRQ_Kind;

   -------------------------------------------------------------------------

   function Get_Minor_Frame_Deadlines
     (Major : DOM.Core.Node)
      return Deadline_Array
   is
      use type Interfaces.Unsigned_64;

      function "<"
        (Left, Right : Deadline_Type)
         return Boolean
      is (Left.Exit_Time < Right.Exit_Time);

      package Map_Of_Minor_Frame_Deadlines is
        new Ada.Containers.Ordered_Multisets (Element_Type => Deadline_Type);

      package MOMFD renames Map_Of_Minor_Frame_Deadlines;

      --  Convert a minor frame deadline set to the corresponding deadline
      --  array.
      function To_Deadline_Array
        (Deadline_Set : MOMFD.Set)
         return Deadline_Array;

      ----------------------------------------------------------------------

      function To_Deadline_Array
        (Deadline_Set : MOMFD.Set)
         return Deadline_Array
      is
         Size    : constant Natural
           := Natural (MOMFD.Length (Container => Deadline_Set));
         Result  : Deadline_Array (1 .. Size);
         Pos     : MOMFD.Cursor := MOMFD.First (Container => Deadline_Set);
         Cur_Idx : Natural      := Result'First;
      begin
         while MOMFD.Has_Element (Position => Pos) loop
            Result (Cur_Idx) := MOMFD.Element (Position => Pos);
            Pos              := MOMFD.Next (Position => Pos);
            Cur_Idx          := Cur_Idx + 1;
         end loop;

         return Result;
      end To_Deadline_Array;

      ----------------------------------------------------------------------

      CPU_Nodes        : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Major,
           XPath => "cpu");
      Minor_Exit_Times : MOMFD.Set;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => CPU_Nodes) - 1 loop
         declare
            CPU_Node      : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => CPU_Nodes,
                 Index => I);
            Minor_Frames  : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => CPU_Node,
                 XPath => "minorFrame");
            Current_Ticks : Interfaces.Unsigned_64 := 0;
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Minor_Frames) - 1
            loop
               declare
                  Minor_Frame : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Minor_Frames,
                       Index => J);
                  Minor_Ticks : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Minor_Frame,
                          Name => "ticks"));
               begin
                  Current_Ticks := Current_Ticks + Minor_Ticks;
                  Minor_Exit_Times.Insert
                    (New_Item => (Exit_Time   => Current_Ticks,
                                  Minor_Frame => Minor_Frame));
               end;
            end loop;
         end;
      end loop;

      return To_Deadline_Array (Deadline_Set => Minor_Exit_Times);
   end Get_Minor_Frame_Deadlines;

   -------------------------------------------------------------------------

   function Get_Occupied_PCI_Buses
     (Data : Muxml.XML_Data_Type)
      return PCI_Bus_Set.Set
   is
      Subj_Dev_Refs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/devices/device");
      Dom_Dev_Refs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/deviceDomains/domain/devices/device");
      PCI_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/hardware/devices/device/pci");
      Result    : PCI_Bus_Set.Set;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => PCI_Nodes) - 1 loop
         declare
            use type DOM.Core.Node;

            PCI      : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => PCI_Nodes,
                 Index => I);
            Bus      : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => PCI,
                 Name => "bus");
            Dev_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Parent_Node (N => PCI),
                 Name => "name");
            Subj_Assigned : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Subj_Dev_Refs,
                 Ref_Attr  => "physical",
                 Ref_Value => Dev_Name);
         begin
            if Subj_Assigned /= null
              or else
                Muxml.Utils.Get_Element
                  (Nodes     => Dom_Dev_Refs,
                   Ref_Attr  => "physical",
                   Ref_Value => Dev_Name) /= null
            then
               Result.Insert
                 (New_Item => PCI_Bus_Range'Value (Bus));
            end if;

         exception
            when Constraint_Error => null;
         end;
      end loop;

      return Result;
   end Get_Occupied_PCI_Buses;

   -------------------------------------------------------------------------

   function Get_Subject_To_Scheduling_Group_Map
     (Data : Muxml.XML_Data_Type)
      return ID_Map_Array
   is
      Physical_Events : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/events/event[@mode='switch']");
      Source_Events : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/events/source/group"
           & "/*[self::event or self::default]");
      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
      Subject_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Subjects);
      Group_To_Subj : constant ID_Map_Array
        := Get_Initial_Scheduling_Group_Subjects (Data => Data);

      Subject_To_Group_ID : ID_Map_Array (0 .. Subject_Count - 1)
        := (others => No_Group);

      Visited_Subjects : array (0 .. Subject_Count - 1) of Boolean
        := (others => False);

      --  Determine scheduling group for given subject and set corresponding
      --  entry in subject to group ID array.
      procedure Determine_Sched_Group (Subject : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Determine_Sched_Group (Subject : DOM.Core.Node)
      is
         Subject_ID  : constant Natural
           := Natural'Value
             (DOM.Core.Elements.Get_Attribute
                  (Elem => Subject,
                   Name => "globalId"));
      begin
         if Subject_To_Group_ID (Subject_ID) /= No_Group then
            return;
         end if;

         if Visited_Subjects (Subject_ID) then
            return;
         end if;

         --  Backtrack through all switch source subjects.

         declare
            Switch_Srcs : constant DOM.Core.Node_List
              := Get_Switch_Sources (Physical_Events => Physical_Events,
                                     Source_Events   => Source_Events,
                                     Target          => Subject);
         begin
            Visited_Subjects (Subject_ID) := True;
            for I in 0 .. DOM.Core.Nodes.Length (List => Switch_Srcs) - 1 loop
               declare
                  Switch_Src  : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item (List  => Switch_Srcs,
                                            Index => I);
                  Src_Subj_ID : constant Natural
                    := Natural'Value
                      (DOM.Core.Elements.Get_Attribute
                           (Elem => Switch_Src,
                            Name => "globalId"));
               begin

                  --  Recursively determine scheduling group.

                  Determine_Sched_Group (Subject => Switch_Src);

                  if Subject_To_Group_ID (Src_Subj_ID) /= No_Group then
                     Subject_To_Group_ID (Subject_ID)
                       := Subject_To_Group_ID (Src_Subj_ID);
                     Visited_Subjects (Subject_ID) := False;
                     return;
                  end if;
               end;
            end loop;
            Visited_Subjects (Subject_ID) := False;
         end;
      end Determine_Sched_Group;
   begin

      --  Add initial subject mappings.

      for I in Group_To_Subj'Range loop
         Subject_To_Group_ID (Group_To_Subj (I)) := I;
      end loop;

      --  Determine scheduling group of remaining subjects.

      for I in Subject_To_Group_ID'Range loop
         if Subject_To_Group_ID (I) = No_Group then
            declare
               Subject : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Nodes     => Subjects,
                    Ref_Attr  => "globalId",
                    Ref_Value => Ada.Strings.Fixed.Trim
                      (Source => I'Img,
                       Side   => Ada.Strings.Left));
            begin
               Determine_Sched_Group (Subject => Subject);
            end;
         end if;
      end loop;

      return Subject_To_Group_ID;
   end Get_Subject_To_Scheduling_Group_Map;

   -------------------------------------------------------------------------

   function Get_Switch_Sources
     (Physical_Events : DOM.Core.Node_List;
      Source_Events   : DOM.Core.Node_List;
      Target          : DOM.Core.Node)
      return DOM.Core.Node_List
   is
      Target_Events : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Target,
           XPath => "events/target/event/@physical");
      Subjects      : DOM.Core.Node_List;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Target_Events) - 1 loop
         declare
            use type DOM.Core.Node;

            Ev_Name : constant String
              := DOM.Core.Nodes.Node_Value
                (N => DOM.Core.Nodes.Item
                   (List  => Target_Events,
                    Index => I));
            Event   : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Physical_Events,
                 Ref_Attr  => "name",
                 Ref_Value => Ev_Name);
         begin
            if Event /= null then
               DOM.Core.Append_Node
                 (List => Subjects,
                  N    => Muxml.Utils.Ancestor_Node
                    (Node  => Muxml.Utils.Get_Element
                         (Nodes     => Source_Events,
                          Ref_Attr  => "physical",
                          Ref_Value => Ev_Name),
                     Level => 4));
            end if;
         end;
      end loop;
      return Subjects;
   end Get_Switch_Sources;

   -------------------------------------------------------------------------

   function Has_Managed_DEBUGCTL (Controls : DOM.Core.Node) return Boolean
   is
      Load : constant Boolean
        := "1" = Muxml.Utils.Get_Element_Value
          (Doc   => Controls,
           XPath => "entry/LoadDebugControls");
      Save : constant Boolean
        := "1" = Muxml.Utils.Get_Element_Value
          (Doc   => Controls,
           XPath => "exit/SaveDebugControls");
   begin
      return Load and Save;
   end Has_Managed_DEBUGCTL;

   -------------------------------------------------------------------------

   function Has_Managed_EFER (Controls : DOM.Core.Node) return Boolean
   is
      Load : constant Boolean
        := "1" = Muxml.Utils.Get_Element_Value
          (Doc   => Controls,
           XPath => "entry/LoadIA32EFER");
      Save : constant Boolean
        := "1" = Muxml.Utils.Get_Element_Value
          (Doc   => Controls,
           XPath => "exit/SaveIA32EFER");
   begin
      return Load and Save;
   end Has_Managed_EFER;

   -------------------------------------------------------------------------

   function Has_Managed_PAT (Controls : DOM.Core.Node) return Boolean
   is
      Load : constant Boolean
        := "1" = Muxml.Utils.Get_Element_Value
          (Doc   => Controls,
           XPath => "entry/LoadIA32PAT");
      Save : constant Boolean
        := "1" = Muxml.Utils.Get_Element_Value
          (Doc   => Controls,
           XPath => "exit/SaveIA32PAT");
   begin
      return Load and Save;
   end Has_Managed_PAT;

   -------------------------------------------------------------------------

   function Has_Managed_PERFGLOBALCTRL
     (Controls : DOM.Core.Node)
      return Boolean
   is
      Load : constant Boolean
        := "1" = Muxml.Utils.Get_Element_Value
          (Doc   => Controls,
           XPath => "entry/LoadIA32PERFGLOBALCTRL");
   begin
      return Load;
   end Has_Managed_PERFGLOBALCTRL;

   -------------------------------------------------------------------------

   function Has_Multiple_Major_Frames
     (Data : Muxml.XML_Data_Type)
      return Boolean
   is
      Major_Frames : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/scheduling/majorFrame");
   begin
      return DOM.Core.Nodes.Length (List => Major_Frames) > 1;
   end Has_Multiple_Major_Frames;

   -------------------------------------------------------------------------

   function Is_MSR_Accessible
     (MSR  : Interfaces.Unsigned_64;
      MSRs : DOM.Core.Node_List)
      return Boolean
   is
      use type Interfaces.Unsigned_64;

      MSR_Count : constant Natural := DOM.Core.Nodes.Length (List => MSRs);
   begin
      for I in Natural range 0 .. MSR_Count - 1 loop
         declare
            Cur_MSR : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => MSRs,
                                      Index => I);
            MSR_Start : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                     (Elem => Cur_MSR,
                      Name => "start"));
            MSR_End : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                     (Elem => Cur_MSR,
                      Name => "end"));
         begin
            if MSR >= MSR_Start and then MSR <= MSR_End then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Is_MSR_Accessible;

   -------------------------------------------------------------------------

   function Is_PCI_Device_Reference
     (Data       : Muxml.XML_Data_Type;
      Device_Ref : DOM.Core.Node)
      return Boolean
   is
      use type DOM.Core.Node;

      Physical_Dev_Name : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Device_Ref,
           Name => "physical");
      Physical_Dev_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/hardware/devices/device[@name='"
           & Physical_Dev_Name & "']");
   begin
      return Muxml.Utils.Get_Element
        (Doc   => Physical_Dev_Node,
         XPath => "pci") /= null;
   end Is_PCI_Device_Reference;

   -------------------------------------------------------------------------

   function Is_Physical_Mmconf_Region
     (Devices_Node : DOM.Core.Node;
      Addr         : Interfaces.Unsigned_64)
      return Boolean
   is
      use type Interfaces.Unsigned_64;
      use type DOM.Core.Node;

      Base     : Interfaces.Unsigned_64;
      Size     : Interfaces.Unsigned_64;

      Base_Str : constant String
        := (if Devices_Node = null then "" else
               DOM.Core.Elements.Get_Attribute
              (Elem => Devices_Node,
               Name => "pciConfigAddress"));
      Size_Str : constant String
        := (if Devices_Node = null then "" else
               DOM.Core.Elements.Get_Attribute
              (Elem => Devices_Node,
               Name => "pciConfigSize"));
   begin
      if Base_Str'Length = 0 or else Size_Str'Length = 0 then
         return False;
      end if;

      Base := Interfaces.Unsigned_64'Value (Base_Str);
      Size := Interfaces.Unsigned_64'Value (Size_Str);
      return Addr >= Base and then Addr < Base + Size;
   end Is_Physical_Mmconf_Region;

   -------------------------------------------------------------------------

   function Is_Tau0_Scheduled (Data : Muxml.XML_Data_Type) return Boolean
   is
      Tau0_Scheduling_Group_Refs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/scheduling/partitions/partition/group/"
           & "subject[@name='tau0']");
   begin
      return DOM.Core.Nodes.Length (List => Tau0_Scheduling_Group_Refs) > 0;
   end Is_Tau0_Scheduled;

   -------------------------------------------------------------------------

   procedure Merge_XIncludes
     (Policy       : in out Muxml.XML_Data_Type;
      Include_Dirs :        Strings.String_Array;
      Add_Location :        Boolean := False)
   is
      Inc_Group_Tag_Name : constant String := "include";

      Includes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "//include[@href]");
   begin
      if DOM.Core.Nodes.Length (List => Includes) = 0 then
         return;
      end if;

      for I in 0 .. DOM.Core.Nodes.Length (List => Includes) - 1 loop
         declare
            Inc_Node : DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Includes,
                 Index => I);
            Parent_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Parent_Node (N => Inc_Node);
            Filename : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Inc_Node,
                 Name => "href");
            Path : constant String
              := Mutools.Utils.Lookup_File
                (Filename    => Filename,
                 Directories => Include_Dirs);
            Content : Muxml.XML_Data_Type;
            Content_Node : DOM.Core.Node;
         begin
            Muxml.Parse (Data         => Content,
                         Kind         => Muxml.None,
                         File         => Path,
                         Add_Location => Add_Location);

            Merge_XIncludes (Policy       => Content,
                             Include_Dirs => Include_Dirs,
                             Add_Location => Add_Location);
            declare
               use type DOM.Core.Node;

               Src_Node : DOM.Core.Node
                 := DOM.Core.Documents.Get_Element (Doc => Content.Doc);
            begin
               if DOM.Core.Elements.Get_Tag_Name
                 (Elem => Src_Node) = Inc_Group_Tag_Name
               then
                  Src_Node := DOM.Core.Nodes.First_Child (N => Src_Node);
               end if;

               while Src_Node /= null loop
                  Content_Node := DOM.Core.Documents.Local.Adopt_Node
                    (Doc    => Policy.Doc,
                     Source => DOM.Core.Documents.Local.Clone_Node
                       (N    => Src_Node,
                        Deep => True));
                  Content_Node := DOM.Core.Nodes.Insert_Before
                    (N         => Parent_Node,
                     New_Child => Content_Node,
                     Ref_Child => Inc_Node);

                  Src_Node := DOM.Core.Nodes.Next_Sibling (N => Src_Node);
               end loop;

               Inc_Node := DOM.Core.Nodes.Remove_Child
                 (N         => Parent_Node,
                  Old_Child => Inc_Node);
               DOM.Core.Nodes.Free (N => Inc_Node);
            end;
         end;
      end loop;
   end Merge_XIncludes;

   -------------------------------------------------------------------------

   procedure Set_Memory_Size
     (Virtual_Mem_Node : DOM.Core.Node;
      Ref_Nodes        : DOM.Core.Node_List)
   is
      Phy_Name : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Virtual_Mem_Node,
           Name => "physical");
      Cur_Size : constant String
        := Muxml.Utils.Get_Attribute
          (Nodes     => Ref_Nodes,
           Ref_Attr  => "name",
           Ref_Value => Phy_Name,
           Attr_Name => "size");
   begin
      DOM.Core.Elements.Set_Attribute
        (Elem  => Virtual_Mem_Node,
         Name  => "size",
         Value => Cur_Size);
   end Set_Memory_Size;

   -------------------------------------------------------------------------

   procedure Set_Memory_Size
     (Virtual_Mem_Nodes : DOM.Core.Node_List;
      Ref_Nodes         : DOM.Core.Node_List)
   is
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Virtual_Mem_Nodes) - 1 loop
         Set_Memory_Size
           (Virtual_Mem_Node => DOM.Core.Nodes.Item
              (List  => Virtual_Mem_Nodes,
               Index => I),
            Ref_Nodes        => Ref_Nodes);
      end loop;
   end Set_Memory_Size;

   -------------------------------------------------------------------------

   function Sort_By_BDF
     (PCI_Devs : DOM.Core.Node_List)
      return DOM.Core.Node_List
   is
      Dev_Count : constant Natural
        := DOM.Core.Nodes.Length (List => PCI_Devs);

      type Dev_Range is new Natural range 1 .. Dev_Count;
      type Dev_Array_Type is array (Dev_Range) of DOM.Core.Node;

      Sort_Devs : Dev_Array_Type;

      --  Returns True if PCI BDF of Left is smaller than Right.
      function "<" (Left, Right : DOM.Core.Node) return Boolean;

      ----------------------------------------------------------------------

      function "<" (Left, Right : DOM.Core.Node) return Boolean
      is
         L_Bus_Nr : constant Natural := Natural'Value
           (Muxml.Utils.Get_Attribute
              (Doc   => Left,
               XPath => "pci",
               Name  => "bus"));
         L_Dev_Nr : constant Natural := Natural'Value
           (Muxml.Utils.Get_Attribute
              (Doc   => Left,
               XPath => "pci",
               Name  => "device"));
         L_Fn_Nr  : constant Natural := Natural'Value
           (Muxml.Utils.Get_Attribute
              (Doc   => Left,
               XPath => "pci",
               Name  => "function"));
         R_Bus_Nr : constant Natural := Natural'Value
           (Muxml.Utils.Get_Attribute
              (Doc   => Right,
               XPath => "pci",
               Name  => "bus"));
         R_Dev_Nr : constant Natural := Natural'Value
           (Muxml.Utils.Get_Attribute
              (Doc   => Right,
               XPath => "pci",
               Name  => "device"));
         R_Fn_Nr  : constant Natural := Natural'Value
           (Muxml.Utils.Get_Attribute
              (Doc   => Right,
               XPath => "pci",
               Name  => "function"));
      begin
         return L_Bus_Nr < R_Bus_Nr
           or else (L_Bus_Nr = R_Bus_Nr and L_Dev_Nr < R_Dev_Nr)
           or else (L_Bus_Nr = R_Bus_Nr and L_Dev_Nr = R_Dev_Nr
                    and L_Fn_Nr < R_Fn_Nr);
      end "<";

      ----------------------------------------------------------------------

      procedure Sort_Devices is
        new Ada.Containers.Generic_Constrained_Array_Sort
          (Index_Type   => Dev_Range,
           Element_Type => DOM.Core.Node,
           Array_Type   => Dev_Array_Type,
           "<"          => "<");
   begin
      for I in Dev_Range loop
         Sort_Devs (I) := DOM.Core.Nodes.Item
           (List  => PCI_Devs,
            Index => Integer (I) - 1);
      end loop;

      Sort_Devices (Container => Sort_Devs);

      return Sorted_Devs : DOM.Core.Node_List do
         for Dev of Sort_Devs loop
            DOM.Core.Append_Node (List => Sorted_Devs,
                                  N    => Dev);
         end loop;
      end return;
   end Sort_By_BDF;

   -------------------------------------------------------------------------

   function To_APIC_ID
     (Policy : Muxml.XML_Data_Type;
      CPU_ID : Natural)
      return Natural
   is
      use type DOM.Core.Node;

      ID_Str : constant String
        := Ada.Strings.Fixed.Trim
          (Source => CPU_ID'Img,
           Side   => Ada.Strings.Left);
      Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Policy.Doc,
           XPath => "/system/hardware/processor/cpu[@cpuId='" & ID_Str & "']");
   begin
      if Node = null then
         raise APIC_ID_Not_Found with "APIC ID for CPU ID " & ID_Str
           & " not found";
      end if;

      return Natural'Value (DOM.Core.Elements.Get_Attribute
                            (Elem => Node,
                             Name => "apicId"));
   end To_APIC_ID;

end Mutools.XML_Utils;
