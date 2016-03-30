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

with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Multisets;
with Ada.Containers.Generic_Constrained_Array_Sort;

with DOM.Core.Nodes;
with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Append_Node;

with McKae.XML.XPath.XIA;

with Muxml.Utils;

with Mutools.Utils;

package body Mutools.XML_Utils
is

   --  Feature enum to element name mapping.
   Feature_Names : constant array
     (Features_Type) of Ada.Strings.Unbounded.Unbounded_String
     := (Feature_IOMMU => Ada.Strings.Unbounded.To_Unbounded_String ("iommu"));

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
      File_Offset :        String)
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

   procedure Add_Resource
     (Logical_Device        : DOM.Core.Node;
      Physical_Resource     : DOM.Core.Node;
      Logical_Resource_Name : String := "")
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
         DOM.Core.Elements.Set_Attribute
           (Elem  => Res_Ref,
            Name  => "writable",
            Value => "true");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Res_Ref,
            Name  => "executable",
            Value => "false");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Res_Ref,
            Name  => "virtualAddress",
            Value => DOM.Core.Elements.Get_Attribute
              (Elem => Physical_Resource,
               Name => "physicalAddress"));
      end if;

      Muxml.Utils.Append_Child
        (Node      => Logical_Device,
         New_Child => Res_Ref);
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
               then
                  MSR_Count := MSR_Count + 1;
               end if;
            end loop;
         end;
      end loop;

      return MSR_Count;
   end Calculate_MSR_Count;

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
      Physical_Name :        String;
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
         Name  => "physical",
         Value => Physical_Name);
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

   function Get_Executing_CPU
     (Data    : Muxml.XML_Data_Type;
      Subject : DOM.Core.Node)
      return Integer
   is
      package Subject_CPU_Package is new Ada.Containers.Hashed_Maps
        (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
         Element_Type    => Integer,
         Hash            => Ada.Strings.Unbounded.Hash,
         Equivalent_Keys => Ada.Strings.Unbounded."=");

      use type Subject_CPU_Package.Cursor;

      Minor_Frames : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/scheduling/majorFrame/cpu/minorFrame");

      --  Subject to CPU mapping.
      Subj_CPU_Map : Subject_CPU_Package.Map;

      --  Find CPU that runs the given subject. A value of -1 is returned if no
      --  CPU can execute the subject.
      function Find_CPU (Subject : DOM.Core.Node) return Integer;

      ----------------------------------------------------------------------

      function Find_CPU (Subject : DOM.Core.Node) return Integer
      is
         Subj_Name : constant Ada.Strings.Unbounded.Unbounded_String
           := Ada.Strings.Unbounded.To_Unbounded_String
             (DOM.Core.Elements.Get_Attribute
                (Elem => Subject,
                 Name => "name"));
         Pos       : constant Subject_CPU_Package.Cursor
           := Subject_CPU_Package.Find
             (Container => Subj_CPU_Map,
              Key       => Subj_Name);
         CPU       : Integer := -1;
      begin
         if Pos = Subject_CPU_Package.No_Element then
            declare
               use type DOM.Core.Node;

               Minor_Frame : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Nodes     => Minor_Frames,
                    Ref_Attr  => "subject",
                    Ref_Value => Ada.Strings.Unbounded.To_String (Subj_Name));
            begin
               if Minor_Frame = null then

                  --  Recursively check if subject is switch target of
                  --  a scheduled subject.

                  declare
                     Src_Subjs    : constant DOM.Core.Node_List
                       := Mutools.XML_Utils.Get_Switch_Sources
                         (Data   => Data,
                          Target => Subject);
                     Switch_Count : constant Integer
                       := DOM.Core.Nodes.Length (List => Src_Subjs);
                  begin
                     Subj_CPU_Map.Insert
                       (Key      => Subj_Name,
                        New_Item => -1);

                     for J in 0 .. Switch_Count - 1 loop
                        CPU := Find_CPU
                          (Subject => DOM.Core.Nodes.Item
                             (List  => Src_Subjs,
                              Index => J));
                        exit when CPU /= -1;
                     end loop;

                     if CPU /= -1 then
                        Subj_CPU_Map.Replace
                          (Key      => Subj_Name,
                           New_Item => CPU);
                     end if;
                  end;
               else
                  CPU := Integer'Value
                    (DOM.Core.Elements.Get_Attribute
                       (Elem => DOM.Core.Nodes.Parent_Node
                            (N => Minor_Frame),
                        Name => "id"));
                  Subj_CPU_Map.Insert
                    (Key      => Subj_Name,
                     New_Item => CPU);
               end if;
            end;
         else
            CPU := Subject_CPU_Package.Element (Position => Pos);
         end if;

         return CPU;
      end Find_CPU;
   begin
      return Find_CPU (Subject => Subject);
   end Get_Executing_CPU;

   -------------------------------------------------------------------------

   function Get_IOAPIC_RTE_Idx
     (IRQ : Legacy_IRQ_Range)
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
      Dev_Refs  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/devices/device");
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
            Assigned : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Dev_Refs,
                 Ref_Attr  => "physical",
                 Ref_Value => Dev_Name);
         begin
            if Assigned /= null then
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

   function Get_Switch_Sources
     (Data   : Muxml.XML_Data_Type;
      Target : DOM.Core.Node)
      return DOM.Core.Node_List
   is
      Events        : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/events/event[@mode='switch']");
      Source_Events : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/events/source/group/*/notify");
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
                (Nodes     => Events,
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
                     Level => 5));
            end if;
         end;
      end loop;
      return Subjects;
   end Get_Switch_Sources;

   -------------------------------------------------------------------------

   function Has_Feature_Enabled
     (Data : Muxml.XML_Data_Type;
      F    : Features_Type)
      return Boolean
   is
      use Ada.Strings.Unbounded;
      use type DOM.Core.Node;

      F_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/features/" & To_String (Feature_Names (F))
           & "[@enabled='true']");
   begin
      return F_Node /= null;
   end Has_Feature_Enabled;

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

end Mutools.XML_Utils;
