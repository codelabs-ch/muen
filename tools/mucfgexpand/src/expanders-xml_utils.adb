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
with DOM.Core.Documents;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Paging.Layouts;

with Alloc.Map;

with Mutools.Constants;
with Muxml.Utils;
with Mucfgcheck.Events;

with Expanders.Utils;

package body Expanders.XML_Utils
is

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   -------------------------------------------------------------------------

   function Add_Optional_Events_Source_Group
     (Policy  : in out Muxml.XML_Data_Type;
      Subject :        DOM.Core.Node;
      Group   :        Mutools.Types.Event_Group_Type)
      return DOM.Core.Node
   is
      use type DOM.Core.Node;

      Subj_Source_Node  : DOM.Core.Node;
      Subj_Source_Group : DOM.Core.Node;
      Subj_Events_Node  : constant DOM.Core.Node
        := Muxml.Utils.Get_Element (Doc   => Subject,
                                    XPath => "events");
      Subj_Group_Name   : constant String
        := Ada.Characters.Handling.To_Lower (Item => Group'Img);
   begin
      Subj_Source_Node := Muxml.Utils.Get_Element
        (Doc   => Subj_Events_Node,
         XPath => "source");
      if Subj_Source_Node = null then
         Subj_Source_Node := DOM.Core.Documents.Create_Element
           (Doc      => Policy.Doc,
            Tag_Name => "source");
         Muxml.Utils.Insert_Before
           (Parent    => Subj_Events_Node,
            New_Child => Subj_Source_Node,
            Ref_Child => "target");
      end if;

      Subj_Source_Group := Muxml.Utils.Get_Element
        (Doc   =>  Subj_Source_Node,
         XPath => "group[@name='" & Subj_Group_Name & "']");
      if Subj_Source_Group = null then
         Subj_Source_Group := DOM.Core.Nodes.Append_Child
           (N         => Subj_Source_Node,
            New_Child => DOM.Core.Documents.Create_Element
              (Doc      => Policy.Doc,
               Tag_Name => "group"));
         DOM.Core.Elements.Set_Attribute
           (Elem  => Subj_Source_Group,
            Name  => "name",
            Value => Subj_Group_Name);
      end if;

      return Subj_Source_Group;
   end Add_Optional_Events_Source_Group;

   -------------------------------------------------------------------------

   function Add_Optional_Events_Target
     (Policy  : in out Muxml.XML_Data_Type;
      Subject :        DOM.Core.Node)
      return DOM.Core.Node
   is
      use type DOM.Core.Node;

      Subj_Events_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Subject,
           XPath => "events");
      Subj_Target_Node : DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Subj_Events_Node,
           XPath => "target");
   begin
      if Subj_Target_Node = null then
         Subj_Target_Node := DOM.Core.Nodes.Append_Child
           (N         => Subj_Events_Node,
            New_Child => DOM.Core.Documents.Create_Element
              (Doc      => Policy.Doc,
               Tag_Name => "target"));
      end if;

      return Subj_Target_Node;
   end Add_Optional_Events_Target;

   -------------------------------------------------------------------------

   function Calculate_PT_Size
     (Policy             : Muxml.XML_Data_Type;
      Paging_Levels      : Paging.Paging_Level;
      Large_Pages        : Boolean;
      Dev_Virt_Mem_XPath : String;
      Virt_Mem_XPath     : String)
      return Interfaces.Unsigned_64
   is
      use type DOM.Core.Node;

      Layout        : Paging.Layouts.Memory_Layout_Type
        (Levels => Paging_Levels);
      Device_Nodes  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => Dev_Virt_Mem_XPath);
      Memory_Nodes  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => Virt_Mem_XPath);
      Physical_Mem  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      Physical_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/hardware/devices/device");
   begin
      Paging.Layouts.Set_Large_Page_Support (Mem_Layout => Layout,
                                             State      => Large_Pages);

      for I in 0 .. DOM.Core.Nodes.Length (List => Memory_Nodes) - 1 loop
         declare
            Logical : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Memory_Nodes,
                 Index => I);
            Physical_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Logical,
                 Name => "physical");
            Physical : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Physical_Mem,
                 Ref_Attr  => "name",
                 Ref_Value => Physical_Name);
            Virtual_Address : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Logical,
                    Name => "virtualAddress"));
            Size : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Physical,
                    Name => "size"));
            Alignment : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Physical,
                    Name => "alignment"));
         begin
            Paging.Layouts.Add_Memory_Region
              (Mem_Layout       => Layout,
               Physical_Address => Alignment,
               Virtual_Address  => Virtual_Address,
               Size             => Size,
               Caching          => Paging.WB,
               Writable         => False,
               Executable       => False);
         end;
      end loop;

      for I in 0 .. DOM.Core.Nodes.Length (List => Device_Nodes) - 1 loop
         declare
            Logical_Mem : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Device_Nodes,
                 Index => I);
            Dev_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Parent_Node (N => Logical_Mem),
                 Name => "physical");
            Physical_Mem_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Logical_Mem,
                 Name => "physical");
            Physical_Mem : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Muxml.Utils.Get_Element
                   (Nodes     => Physical_Devs,
                    Ref_Attr  => "name",
                    Ref_Value => Dev_Name),
                 XPath => "memory[@name='" & Physical_Mem_Name & "']");
            Physical_Address : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Physical_Mem,
                    Name => "physicalAddress"));
            Virtual_Address : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Logical_Mem,
                    Name => "virtualAddress"));
            Size : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Physical_Mem,
                    Name => "size"));
         begin
            Paging.Layouts.Add_Memory_Region
              (Mem_Layout       => Layout,
               Physical_Address => Physical_Address,
               Virtual_Address  => Virtual_Address,
               Size             => Size,
               Caching          => Paging.WB,
               Writable         => False,
               Executable       => False);
         end;
      end loop;

      declare
         use type Interfaces.Unsigned_64;

         Table_Counts : constant Paging.Layouts.Table_Count_Array
           := Paging.Layouts.Get_Table_Count (Mem_Layout => Layout);
         Count        : Natural := 0;
      begin
         for C of Table_Counts loop
            Count := Count + C;
         end loop;
         return Interfaces.Unsigned_64 (Count) * Mutools.Constants.Page_Size;
      end;
   end Calculate_PT_Size;

   -------------------------------------------------------------------------

   function Calculate_Region_Address
     (Policy             : Muxml.XML_Data_Type;
      Fixed_Memory       : DOM.Core.Node_List;
      Device_Memory      : DOM.Core.Node_List;
      Address_Space_Size : Interfaces.Unsigned_64;
      Region_Size        : Interfaces.Unsigned_64)
      return Interfaces.Unsigned_64
   is
      Map : Alloc.Map.Map_Type;

      Physical_Mem  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     =>  Policy.Doc,
           XPath => "/system/memory/memory");
      Physical_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     =>  Policy.Doc,
           XPath => "/system/hardware/devices/device");
   begin
      Map.Insert_Empty_Region (Name          => U ("Mem"),
                               Allocatable   => True,
                               First_Address => 0,
                               Last_Address  => Address_Space_Size);

      for I in 0 .. DOM.Core.Nodes.Length (List => Fixed_Memory) - 1 loop
         declare
            use type Interfaces.Unsigned_64;

            Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Fixed_Memory,
                                      Index => I);
            Virt_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "logical");
            Phy_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "physical");
            Virt_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "virtualAddress"));
            Size : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (Muxml.Utils.Get_Attribute
                   (Nodes     => Physical_Mem,
                    Ref_Attr  => "name",
                    Ref_Value => Phy_Name,
                    Attr_Name => "size"));
         begin
            Map.Allocate_Fixed (Name          => U (Virt_Name),
                                First_Address => Virt_Addr,
                                Last_Address  => Virt_Addr + Size - 1);
         end;
      end loop;

      for I in 0 .. DOM.Core.Nodes.Length (List => Device_Memory) - 1 loop
         declare
            use type Interfaces.Unsigned_64;

            Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Device_Memory,
                                      Index => I);
            Virt_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "logical");
            Phy_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "physical");
            Dev_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
               Name => "physical");
            Virt_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "virtualAddress"));
            Size : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (Muxml.Utils.Get_Attribute
                   (Doc   => Muxml.Utils.Get_Element
                      (Nodes     => Physical_Devs,
                       Ref_Attr  => "name",
                       Ref_Value => Dev_Name),
                    XPath => "memory[@name='" & Phy_Name & "']",
                    Name  => "size"));
         begin
            Map.Allocate_Fixed (Name          => U (Virt_Name),
                                First_Address => Virt_Addr,
                                Last_Address  => Virt_Addr + Size - 1);
         end;
      end loop;

      Map.Allocate_Variable (Name => U ("New_Region"),
                             Size => Region_Size);
      return Map.Get_Region (Name => "New_Region").First_Address;
   end Calculate_Region_Address;

   -------------------------------------------------------------------------

   function Create_Logical_Device_Node
     (Policy        : in out Muxml.XML_Data_Type;
      Logical_Name  :        String;
      Physical_Name :        String)
      return DOM.Core.Node
   is
      Device_Node : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Policy.Doc,
           Tag_Name => "device");
   begin
      DOM.Core.Elements.Set_Attribute
        (Elem  => Device_Node,
         Name  => "logical",
         Value => Logical_Name);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Device_Node,
         Name  => "physical",
         Value => Physical_Name);

      return Device_Node;
   end Create_Logical_Device_Node;

   -------------------------------------------------------------------------

   procedure Create_Physical_Event_Node
     (Policy : in out Muxml.XML_Data_Type;
      Name   :        String;
      Mode   :        String)
   is
      Event_Node  : DOM.Core.Node;
      Events_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Policy.Doc,
           XPath => "/system/events");
   begin
      Event_Node := DOM.Core.Documents.Create_Element
        (Doc      => Policy.Doc,
         Tag_Name => "event");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Event_Node,
         Name  => "name",
         Value => Name);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Event_Node,
         Name  => "mode",
         Value => Mode);
      Muxml.Utils.Append_Child
        (Node      => Events_Node,
         New_Child => Event_Node);
   end Create_Physical_Event_Node;

   -------------------------------------------------------------------------

   function Create_Source_Event_Node
     (Policy        : in out Muxml.XML_Data_Type;
      ID            :        String;
      Logical_Name  :        String;
      Physical_Name :        String)
      return DOM.Core.Node
   is
      Event_Node  : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Policy.Doc,
           Tag_Name => "event");
   begin
      DOM.Core.Elements.Set_Attribute
        (Elem  => Event_Node,
         Name  => "id",
         Value => ID);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Event_Node,
         Name  => "logical",
         Value => Logical_Name);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Event_Node,
         Name  => "physical",
         Value => Physical_Name);

      return Event_Node;
   end Create_Source_Event_Node;

   -------------------------------------------------------------------------

   function Create_Target_Event_Node
     (Policy        : in out Muxml.XML_Data_Type;
      Logical_Name  :        String;
      Physical_Name :        String;
      Vector        :        String)
      return DOM.Core.Node
   is
      Event_Node : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Policy.Doc,
           Tag_Name => "event");
   begin
      DOM.Core.Elements.Set_Attribute
        (Elem  => Event_Node,
         Name  => "logical",
         Value => Logical_Name);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Event_Node,
         Name  => "physical",
         Value => Physical_Name);

      if Vector /= "" then
         declare
            Action_Node : constant DOM.Core.Node
              := DOM.Core.Documents.Create_Element
                (Doc      => Policy.Doc,
                 Tag_Name => "inject_interrupt");
         begin
            DOM.Core.Elements.Set_Attribute
              (Elem  => Action_Node,
               Name  => "vector",
               Value => Vector);
            Muxml.Utils.Append_Child
              (Node      => Event_Node,
               New_Child => Action_Node);
         end;
      end if;

      return Event_Node;
   end Create_Target_Event_Node;

   -------------------------------------------------------------------------

   function Is_Free_To_Map
     (Subject         : DOM.Core.Node;
      Virtual_Address : Interfaces.Unsigned_64;
      Region_Size     : Interfaces.Unsigned_64)
      return Boolean
   is
      use type Interfaces.Unsigned_64;

      Ref_End_Addr : constant Interfaces.Unsigned_64
        := Virtual_Address + Region_Size - 1;
      Phys_Mem : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => DOM.Core.Nodes.Owner_Document (N => Subject),
           XPath => "/system/memory/memory");
      Phys_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => DOM.Core.Nodes.Owner_Document (N => Subject),
           XPath => "/system/hardware/devices/device");
      Mem_Regions : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Subject,
           XPath => "memory/memory");
      Subj_Dev_Regions : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Subject,
           XPath => "devices/device/memory");
   begin
      Check_Subject_Memory :
      for I in 0 .. DOM.Core.Nodes.Length (List => Mem_Regions) - 1 loop
         declare
            Virt_Mem : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
              (List  => Mem_Regions,
               Index => I);
            Start_Addr_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Virt_Mem,
                 Name => "virtualAddress");
            Start_Addr : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
         begin
            if Start_Addr_Str'Length > 0 then
               Start_Addr := Interfaces.Unsigned_64'Value (Start_Addr_Str);
            end if;

            if Start_Addr <= Ref_End_Addr then
               declare
                  Phys_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Virt_Mem,
                       Name => "physical");
                  Size      : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (Muxml.Utils.Get_Attribute
                           (Nodes     => Phys_Mem,
                            Ref_Attr  => "name",
                            Ref_Value => Phys_Name,
                            Attr_Name => "size"));
                  End_Addr  : constant Interfaces.Unsigned_64
                    := Start_Addr + Size - 1;
               begin
                  if End_Addr >= Virtual_Address then
                     return False;
                  end if;
               end;
            end if;
         end;
      end loop Check_Subject_Memory;

      Check_Subject_Device_Memory :
      for I in 0 .. DOM.Core.Nodes.Length (List => Subj_Dev_Regions) - 1 loop
         declare
            Virt_Dev_Mem : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Subj_Dev_Regions,
               Index => I);
            Start_Addr   : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                     (Elem => Virt_Dev_Mem,
                      Name => "virtualAddress"));
         begin
            if Start_Addr <= Ref_End_Addr then
               declare
                  Phys_Dev_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => DOM.Core.Nodes.Parent_Node (N => Virt_Dev_Mem),
                       Name => "physical");
                  Phys_Mem_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Virt_Dev_Mem,
                       Name => "physical");
                  Phys_Dev      : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Nodes     => Phys_Devs,
                       Ref_Attr  => "name",
                       Ref_Value => Phys_Dev_Name);
                  Phys_Dev_Mem  : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Doc   => Phys_Dev,
                       XPath => "memory[@name='" & Phys_Mem_Name & "']");
               begin
                  declare
                     Size     : constant Interfaces.Unsigned_64
                       := Interfaces.Unsigned_64'Value
                         (DOM.Core.Elements.Get_Attribute
                              (Elem => Phys_Dev_Mem,
                               Name => "size"));
                     End_Addr : constant Interfaces.Unsigned_64
                       := Start_Addr + Size - 1;
                  begin
                     if End_Addr >= Virtual_Address then
                        return False;
                     end if;
                  end;
               end;
            end if;
         end;
      end loop Check_Subject_Device_Memory;

      return True;
   end Is_Free_To_Map;

   -------------------------------------------------------------------------

   function Next_Free_Source_Event_ID (Group : DOM.Core.Node) return String
   is
      Group_Str : constant String
        := DOM.Core.Elements.Get_Attribute (Elem => Group,
                                            Name => "name");
      Group_Type : constant Mutools.Types.Event_Group_Type
        := Mutools.Types.Event_Group_Type'Value (Group_Str);
      Events : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
        (N     => Group,
         XPath => "*[@id]");
      Alloc : Utils.Number_Allocator_Type
        (Range_Start => 0,
         Range_End   => Mucfgcheck.Events.Get_Max_ID (Group => Group_Type));

      Result : Natural;
   begin
      Utils.Reserve_Numbers (Allocator => Alloc,
                             Nodes     => Events,
                             Attribute => "id");
      Utils.Allocate (Allocator => Alloc,
                      Number    => Result);

      return Ada.Strings.Fixed.Trim (Source => Result'Img,
                                     Side   => Ada.Strings.Left);
   end Next_Free_Source_Event_ID;

end Expanders.XML_Utils;
