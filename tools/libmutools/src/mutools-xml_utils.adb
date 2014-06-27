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

with DOM.Core.Nodes;
with DOM.Core.Documents;
with DOM.Core.Elements;

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

   function Get_Occupied_PCI_Buses
     (Data : Muxml.XML_Data_Type)
      return PCI_Bus_Set.Set
   is
      Buses  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/platform/devices/device/pci/@bus");
      Result : PCI_Bus_Set.Set;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Buses) - 1 loop
         declare
            Bus : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Buses,
                 Index => I);
         begin
            Result.Insert
              (New_Item => PCI_Bus_Range'Value
                 (DOM.Core.Nodes.Node_Value (N => Bus)));

         exception
            when Constraint_Error => null;
         end;
      end loop;

      return Result;
   end Get_Occupied_PCI_Buses;

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

end Mutools.XML_Utils;
