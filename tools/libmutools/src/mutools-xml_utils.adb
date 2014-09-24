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

with Interfaces;

with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;

with DOM.Core.Nodes;
with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Append_Node;

with McKae.XML.XPath.XIA;

with Muxml.Utils;

with Mutools.Utils;

package body Mutools.XML_Utils
is

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
           XPath => "/system/platform/devices/device/pci");
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
           XPath => "/system/platform/devices/device[@name='"
           & Physical_Dev_Name & "']");
   begin
      return Muxml.Utils.Get_Element
        (Doc   => Physical_Dev_Node,
         XPath => "pci") /= null;
   end Is_PCI_Device_Reference;

end Mutools.XML_Utils;
