--
--  Copyright (C) 2023 secunet Security Networks AG
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
with Ada.Directories;

with DOM.Core;
with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mutools.Utils;
with Mutools.Xmldebuglog;

with Mulog;
with Muxml;
with Muxml.Utils;

with Alloc.Map;
with Alloc.Config;

package body Vres_Alloc
is
   --  Return the size of the given channel via a lookup of the physical
   --  channel name.
   function Get_Size_From_Physical
     (Node : DOM.Core.Node)
     return String;

   ----------------------------------------------------------------------

   --  Given an 'array' node, add mappings
   --  'logical'-> ('virtualAddress','size')
   --  for all elements of the array to Logical_To_Va_Map
   procedure  Include_Array
     (Mapping  : in out Logical_To_Interval_Package.Map;
      Node     :        DOM.Core.Node;
      Run_Type :        Alloc.Map.Run_Type_Type);

   ----------------------------------------------------------------------

   --  Read targets within the given component declaration Comp_Node
   --  and use these entries to populate Components_Map (mapping logical
   --  names to virtual resources).
   procedure Init_Component_Elements
     (Comp_Node : DOM.Core.Node;
      Run_Type  : Alloc.Map.Run_Type_Type);

   ----------------------------------------------------------------------

   --  Insert a new mapping into Components_Map mapping the 'name' of Comp_Node
   --  to its profile and empty mappings for virtual resources.
   procedure Init_Component_Head (Comp_Node : DOM.Core.Node);

   ----------------------------------------------------------------------

   --  Given a non-'array' node, add a mapping
   --  'logical'-> ('virtualAddress','size')
   --  to Logical_To_Va_Map
   procedure Include_Single_Region
     (Mapping  : in out Logical_To_Interval_Package.Map;
      Node     :        DOM.Core.Node;
      Run_Type :        Alloc.Map.Run_Type_Type);

   ----------------------------------------------------------------------

   --  Fill 'Components_Map' with information about from the 'components'
   --  section of Policy
   procedure Initialize_Components_Map
     (Policy : Muxml.XML_Data_Type);

   ----------------------------------------------------------------------

   --  Fill the maps 'Memory_Sizes' and 'Channel_Sizes' with entries of the
   --  form 'channelsname' -> 'channelsize'.
   procedure Initialize_Size_Maps
     (Policy : Muxml.XML_Data_Type);

   ----------------------------------------------------------------------

   --  For each element of Targets, add a mapping from 'logical' to
   --  a virtual resource (debpending on Run_Type) into Components_Map.
   procedure Put_Targets_In_Map
     (Mapping  : in out Logical_To_Interval_Package.Map;
      Targets  :        DOM.Core.Node_List;
      Run_Type :        Alloc.Map.Run_Type_Type);

   ----------------------------------------------------------------------

   --  iterate over target nodes
   --    if nodes has a VA: substract that space from Available_Memory
   --    otherwise:
   --       assign if fixed by Component_Info
   --       otherwise: put node on Todo_List
   procedure Assign_Resources_If_Known
     (Targets          :        DOM.Core.Node_List;
      Todo_List        : in out Alloc.Map.Node_List_Package.List;
      Available_Memory : in out Alloc.Map.VA_Regions_Type;
      Mapping          : in out Logical_To_Interval_Package.Map;
      Read_Only        :        Boolean;
      Run_Type         :        Alloc.Map.Run_Type_Type);

   ----------------------------------------------------------------------

   --  Iterate over Todo_List and assign virtual addresses from the
   --  Available_Memory regions
   procedure Allocate_Todo_Nodes
     (Todo_List :        Alloc.Map.Node_List_Package.List;
      Av_Mem    : in out Alloc.Map.VA_Regions_Type;
      Run_Type  :        Alloc.Map.Run_Type_Type);

   -------------------------------------------------------------------------

   --  Return string representation of I for error messages
   function To_String (I : Address_And_Size_Type) return String;

   -------------------------------------------------------------------------

   procedure Allocate_Todo_Nodes
     (Todo_List :        Alloc.Map.Node_List_Package.List;
      Av_Mem    : in out Alloc.Map.VA_Regions_Type;
      Run_Type  :        Alloc.Map.Run_Type_Type)
   is
      use type Alloc.Map.Node_List_Package.Cursor;
      Curr : Alloc.Map.Node_List_Package.Cursor
        := Todo_List.First;
      Curr_Node : DOM.Core.Node;
      Size : Interfaces.Unsigned_64;
   begin
      while Curr /= Alloc.Map.Node_List_Package.No_Element loop
         Curr_Node :=  Alloc.Map.Node_List_Package.Element (Curr);
         case Run_Type is
            when Alloc.Map.VIRTUAL_ADDRESSES =>
               Size := Interfaces.Unsigned_64'Value
                 (Get_Size_From_Physical (Node => Curr_Node));
            when Alloc.Map.READER_EVENTS | Alloc.Map.WRITER_EVENTS =>
               Size := 1;
         end case;
         Alloc.Map.Allocate_And_Set_Single_Resource
           (Av_Mem   => Av_Mem,
            Node     => Curr_Node,
            Run_Type => Run_Type,
            Size     => Alloc.Map.To_String (Size));
         Alloc.Map.Node_List_Package.Next (Curr);
      end loop;
   end Allocate_Todo_Nodes;

   -------------------------------------------------------------------------

   procedure Assign_Resources_If_Known
     (Targets          :        DOM.Core.Node_List;
      Todo_List        : in out Alloc.Map.Node_List_Package.List;
      Available_Memory : in out Alloc.Map.VA_Regions_Type;
      Mapping          : in out Logical_To_Interval_Package.Map;
      Read_Only        :        Boolean;
      Run_Type         :        Alloc.Map.Run_Type_Type)
   is
      Curr_Node : DOM.Core.Node;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Targets) - 1 loop
         Curr_Node := DOM.Core.Nodes.Item (List  => Targets,
                                           Index => I);
         declare
            Attr_Value : constant String
              := Alloc.Map.Get_Resource_Value (Elem => Curr_Node, Run_Type => Run_Type);
            Logical : constant String
              := DOM.Core.Elements.Get_Attribute
              (Elem => Curr_Node,
               Name => "logical");
            Size : Interfaces.Unsigned_64;
         begin
            if Attr_Value = "" or Attr_Value = "auto" then
               if Read_Only then
                  Mulog.Log (Msg => "Found read-only node which requested automatic "
                               & "allocation of virtual resource. Xpath: '"
                               & Mutools.Xmldebuglog.Get_Xpath (Node => Curr_Node)
                               & "', value: '"
                               & Attr_Value
                               & "'");
                  raise Validation_Error with "Invalid attribute value";
               end if;

               if Logical_To_Interval_Package.Contains
                 (Container => Mapping,
                  Key       => Logical)
               then
                  Alloc.Map.Set_Virtual_Resource
                    (Node     => Curr_Node,
                     Run_Type => Run_Type,
                     Value    => Mapping (Logical).First_Address);
               else
                  Alloc.Map.Node_List_Package.Append
                    (Container => Todo_List,
                     New_Item  => Curr_Node);
               end if;
            else
               case Run_Type is
                  when Alloc.Map.VIRTUAL_ADDRESSES =>
                     Size := Interfaces.Unsigned_64'Value
                       (Get_Size_From_Physical (Node => Curr_Node));
                  when Alloc.Map.READER_EVENTS | Alloc.Map.WRITER_EVENTS =>
                     Size := 1;
               end case;
               Alloc.Map.Subtract_Memory_Interval
                 (List          => Available_Memory,
                  First_Address => Interfaces.Unsigned_64'Value (Attr_Value),
                  Size          => Size);
            end if;
         end;
      end loop;
   end Assign_Resources_If_Known;

   -------------------------------------------------------------------------

   function Get_Size_From_Physical
     (Node : DOM.Core.Node)
     return String
   is
      Physical     : constant String
        := DOM.Core.Elements.Get_Attribute (Elem => Node,
                                            Name => "physical");
      Tag_Name     : constant String
        := DOM.Core.Elements.Get_Tag_Name (Elem => Node);
   begin
      if Tag_Name = "memory" then
         if (not Memory_Sizes.Contains (Physical)) or else
           Memory_Sizes (Physical) = ""
         then
            raise Validation_Error with
              "Cannot find size of node at '"
              & Mutools.Xmldebuglog.Get_Xpath (Node => Node)
              & "' with physical '"
              & Physical
              & "'";
         end if;
         return Memory_Sizes (Physical);
      else
         if (not Channel_Sizes.Contains (Physical)) or else
           Channel_Sizes (Physical) = ""
         then
            raise Validation_Error with
              "Cannot find size of node at '"
              & Mutools.Xmldebuglog.Get_Xpath (Node => Node)
              & "' with physical '"
              & Physical
              & "'";
         end if;
         return Channel_Sizes (Physical);
      end if;
   end Get_Size_From_Physical;

   ----------------------------------------------------------------------

   procedure Include_Array
     (Mapping  : in out Logical_To_Interval_Package.Map;
      Node     :        DOM.Core.Node;
      Run_Type :        Alloc.Map.Run_Type_Type)
   is
      use type Interfaces.Unsigned_64;
      use type DOM.Core.Node_Types;

      Res_Base_Value : constant String
        := Alloc.Map.Get_Resource_Value (Elem => Node, Run_Type => Run_Type);
      Size : constant Interfaces.Unsigned_64
        := Alloc.Map.Get_Resource_Size (Elem => Node, Run_Type => Run_Type);
   begin
      if Res_Base_Value = "" or Res_Base_Value = "auto" then
         raise Validation_Error with
           "Missing attribute value at '"
           & Mutools.Xmldebuglog.Get_Xpath (Node => Node)
           & "'";
      else
         declare
            Child      : DOM.Core.Node;
            Child_List : constant DOM.Core.Node_List
              := DOM.Core.Nodes.Child_Nodes (N => Node);
            Count      : Interfaces.Unsigned_64
              := 0;
            Base_U64   : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value (Res_Base_Value);
            Cursor     : Logical_To_Interval_Package.Cursor;
            Inserted   : Boolean;
         begin
            for I in 0 ..  DOM.Core.Nodes.Length (List => Child_List) - 1 loop
               Child := DOM.Core.Nodes.Item (List  => Child_List,
                                             Index => I);
               if DOM.Core.Nodes.Node_Type (N => Child)
                 = DOM.Core.Element_Node
               then
                  Logical_To_Interval_Package.Insert
                    (Container => Mapping,
                     Key       => DOM.Core.Elements.Get_Attribute
                       (Elem => Child,
                        Name => "logical"),
                     New_Item  => (First_Address => Base_U64 + (Size * Count),
                                   Size          => Size),
                     Position  => Cursor,
                     Inserted  => Inserted);

                  --  If the key existed already, check if the value is equal
                  if not Inserted and then
                    Logical_To_Interval_Package.Element (Position => Cursor)
                    /= Address_And_Size_Type'
                    (First_Address => Base_U64 + (Size * Count),
                     Size          => Size)
                  then
                     raise Validation_Error with
                       "Conflicting resource values for logical '"
                       & DOM.Core.Elements.Get_Attribute
                       (Elem => Child,
                        Name => "logical")
                       & "': found '"
                       & To_String (I => Logical_To_Interval_Package.Element
                                      (Position => Cursor))
                       & "' and '"
                       & To_String
                       (I => (First_Address => Base_U64 + (Size * Count),
                              Size          => Size))
                       & "'";
                  end if;

                  Count := Count + 1;
               end if;
            end loop;
         end;
      end if;
   end Include_Array;

   -------------------------------------------------------------------------

   procedure Include_Single_Region
     (Mapping  : in out Logical_To_Interval_Package.Map;
      Node     :        DOM.Core.Node;
      Run_Type :        Alloc.Map.Run_Type_Type)
   is
      Attr_Value : constant String
        := Alloc.Map.Get_Resource_Value (Elem => Node, Run_Type => Run_Type);
      Size : constant Interfaces.Unsigned_64
        := Alloc.Map.Get_Resource_Size (Elem => Node, Run_Type => Run_Type);
      Logical : constant String
        := DOM.Core.Elements.Get_Attribute (Elem => Node,
                                            Name => "logical");
      Cursor   : Logical_To_Interval_Package.Cursor;
      Inserted : Boolean;
      New_Value : Address_And_Size_Type;
   begin
      if Attr_Value = "" or Attr_Value = "auto" or Logical = "" then
         raise Validation_Error with
           "Missing attribute value at '"
           & Mutools.Xmldebuglog.Get_Xpath (Node => Node)
           & "'";
      else
         New_Value := Address_And_Size_Type'
           (First_Address => Interfaces.Unsigned_64'Value (Attr_Value),
            Size          => Size);
         Logical_To_Interval_Package.Insert
           (Container => Mapping,
            Key       => Logical,
            New_Item  => New_Value,
            Position  => Cursor,
            Inserted  => Inserted);

         --  If the key exists already, check if the value is equal
         if not Inserted and then
           Logical_To_Interval_Package.Element (Position => Cursor)
           /= New_Value
         then
            raise Validation_Error with
              "Conflicting resource values for logical '"
              & Logical
              & "': found '"
              & To_String (I => Logical_To_Interval_Package.Element
                             (Position => Cursor))
              & "' and '"
              & To_String (I => New_Value)
              & "'";
         end if;
      end if;
   end Include_Single_Region;

   -------------------------------------------------------------------------

   procedure Init_Component_Elements
     (Comp_Node : DOM.Core.Node;
      Run_Type  : Alloc.Map.Run_Type_Type)
   is
      Comp_Name : constant String
        := DOM.Core.Elements.Get_Attribute
        (Elem => Comp_Node,
         Name => "name");
      Targets_List_1, Targets_List_2 :  Mutools.String_Vector.Vector;
      Targets : DOM.Core.Node_List;
      Mapping : Logical_To_Interval_Package.Map;
   begin
      case Run_Type is
         when Alloc.Map.VIRTUAL_ADDRESSES =>
            Targets_List_1 := Alloc.Config.C_Va_Alloc_Read_Write_Targets;
            Targets_List_2 := Alloc.Config.C_Va_Alloc_Read_Only_Targets;
            Mapping        := Components_Map (Comp_Name).Va_Map;
         when Alloc.Map.READER_EVENTS =>
            Targets_List_1 := Alloc.Config.C_Readers_Read_Write_Targets;
            Targets_List_2 := Alloc.Config.C_Readers_Read_Only_Targets;
            Mapping        := Components_Map (Comp_Name).Reader_Events_Map;
         when Alloc.Map.WRITER_EVENTS =>
            Targets_List_1 := Alloc.Config.C_Writers_Read_Write_Targets;
            Targets_List_2 := Alloc.Config.C_Writers_Read_Only_Targets;
            Mapping        := Components_Map (Comp_Name).Writer_Events_Map;
      end case;

      Targets := McKae.XML.XPath.XIA.XPath_Query
        (N     => Comp_Node,
         XPath => Alloc.Map.Get_Target_String
           (Target_List => Targets_List_1,
            Prefix      => Alloc.Config.C_Res_Alloc_Target_Prefix));
      Put_Targets_In_Map (Mapping  => Mapping,
                          Targets  => Targets,
                          Run_Type => Run_Type);

      Targets := McKae.XML.XPath.XIA.XPath_Query
        (N     => Comp_Node,
         XPath => Alloc.Map.Get_Target_String
           (Target_List => Targets_List_2,
            Prefix      => Alloc.Config.C_Res_Alloc_Target_Prefix));
      Put_Targets_In_Map (Mapping  => Mapping,
                          Targets  => Targets,
                          Run_Type => Run_Type);
   end Init_Component_Elements;

   -------------------------------------------------------------------------

   procedure Init_Component_Head (Comp_Node : DOM.Core.Node)
   is
      Comp_Name : constant String
        := DOM.Core.Elements.Get_Attribute
        (Elem => Comp_Node,
         Name => "name");
      Comp_Profile : constant String
        := DOM.Core.Elements.Get_Attribute
        (Elem => Comp_Node,
         Name => "profile");
   begin
      if not Component_To_Map_Package.Contains
        (Container => Components_Map,
         Key       => Comp_Name)
      then
         Component_To_Map_Package.Insert
           (Container => Components_Map,
            Key       => Comp_Name,
            New_Item  =>
              (Profile           => Mutools.String_Holder_Type.To_Holder
                 (Comp_Profile),
               Va_Map            => Logical_To_Interval_Package.Empty_Map,
               Reader_Events_Map => Logical_To_Interval_Package.Empty_Map,
               Writer_Events_Map => Logical_To_Interval_Package.Empty_Map));
      end if;
   end Init_Component_Head;

   -------------------------------------------------------------------------

   procedure Initialize_Components_Map
     (Policy : Muxml.XML_Data_Type)
   is
      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/components/component");
      Curr_Component : DOM.Core.Node;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Components) - 1 loop
         Curr_Component := DOM.Core.Nodes.Item (List  => Components,
                                                Index => I);
         Init_Component_Head (Comp_Node => Curr_Component);

         Init_Component_Elements (Comp_Node => Curr_Component,
                                  Run_Type  => Alloc.Map.VIRTUAL_ADDRESSES);
         Init_Component_Elements (Comp_Node => Curr_Component,
                                  Run_Type  => Alloc.Map.WRITER_EVENTS);
         Init_Component_Elements (Comp_Node => Curr_Component,
                                  Run_Type  => Alloc.Map.READER_EVENTS);
      end loop;
   end Initialize_Components_Map;

   -------------------------------------------------------------------------

   procedure Initialize_Size_Maps
     (Policy : Muxml.XML_Data_Type)
   is
      Memory_Entries : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/memory/memory");
      Channel_Entries : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/channels/channel");

      procedure Generic_Init
        (Targets  :        DOM.Core.Node_List;
         Hash_Map : in out Mutools.Expressions.Name_To_String_Hashed_Map.Map);

      procedure Generic_Init
        (Targets  :        DOM.Core.Node_List;
         Hash_Map : in out Mutools.Expressions.Name_To_String_Hashed_Map.Map)
      is
         Curr_Node : DOM.Core.Node;
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Targets) - 1 loop
            Curr_Node := DOM.Core.Nodes.Item (List  => Targets,
                                              Index => I);
            Hash_Map.Insert
              (Key      => DOM.Core.Elements.Get_Attribute (Elem => Curr_Node,
                                                            Name => "name"),
               New_Item => DOM.Core.Elements.Get_Attribute (Elem => Curr_Node,
                                                            Name => "size"));
         end loop;
      end Generic_Init;

   begin
      Generic_Init (Targets => Memory_Entries,
                    Hash_Map => Memory_Sizes);
      Generic_Init (Targets => Channel_Entries,
                    Hash_Map => Channel_Sizes);
   end Initialize_Size_Maps;

   -------------------------------------------------------------------------

   procedure Put_Targets_In_Map
     (Mapping   : in out Logical_To_Interval_Package.Map;
      Targets   :        DOM.Core.Node_List;
      Run_Type  :        Alloc.Map.Run_Type_Type)
   is
      Curr_Node : DOM.Core.Node;
   begin
      for I in 0 ..  DOM.Core.Nodes.Length (List => Targets) - 1 loop
         Curr_Node := DOM.Core.Nodes.Item (List  => Targets,
                                           Index => I);

         if DOM.Core.Elements.Get_Tag_Name (Elem => Curr_Node) = "array"
         then
            Include_Array
              (Mapping  => Mapping,
               Node     => Curr_Node,
               Run_Type => Run_Type);
         else
            Include_Single_Region
              (Mapping  => Mapping,
               Node     => Curr_Node,
               Run_Type => Run_Type);
         end if;
      end loop;

   end Put_Targets_In_Map;

   ----------------------------------------------------------------------

   procedure Run
     (Policy_Joined    : String;
      Output_File_Name : String)
   is
      Policy         : Muxml.XML_Data_Type;

      procedure Process_Subject
        (Subject        : DOM.Core.Node;
         Component_Info : Component_Info_Type;
         Run_Type       : Alloc.Map.Run_Type_Type);

      -------------------------------------------------------------------

      procedure Process_Subject
        (Subject        : DOM.Core.Node;
         Component_Info : Component_Info_Type;
         Run_Type       : Alloc.Map.Run_Type_Type)
      is
         use type Ada.Containers.Count_Type;

         Available_Memory : Alloc.Map.VA_Regions_Type;
         Todo_List        : Alloc.Map.Node_List_Package.List;
         Mapping          : Logical_To_Interval_Package.Map;
      begin
         --  Initialize free space for virtual ressource.
         --  Note: subtracting the memory reserved by the component
         --  specification (stored in Component_Info.*_Map)
         --  is delayed to avoid it when possible.
         case Run_Type is
            when Alloc.Map.VIRTUAL_ADDRESSES =>
               if Component_Info.Profile.Element = "native"
               then
                  Alloc.Map.Add_Memory_Interval
                    (List     => Available_Memory,
                     Interval => Alloc.Config.Default_Va_Space_Native);
               else
                  Alloc.Map.Add_Memory_Interval
                    (List     => Available_Memory,
                     Interval => Alloc.Config.Default_Va_Space_Vm);
               end if;
            when Alloc.Map.WRITER_EVENTS =>
               Alloc.Map.Add_Memory_Interval
                 (List     => Available_Memory,
                  Interval => Alloc.Config.Event_Numbers_Domain);
            when Alloc.Map.READER_EVENTS =>
               Alloc.Map.Add_Memory_Interval
                 (List     => Available_Memory,
                  Interval => Alloc.Config.Vector_Numbers_Domain);
         end case;

         --  Choose targets and mapping depending on Run_Type
         declare
            Targets_List_R, Targets_List_R_W : Mutools.String_Vector.Vector;
            Targets : DOM.Core.Node_List;
         begin
            case Run_Type is
               when Alloc.Map.VIRTUAL_ADDRESSES =>
                  Targets_List_R_W := Alloc.Config.Va_Alloc_Read_Write_Targets;
                  Targets_List_R   := Alloc.Config.Va_Alloc_Read_Only_Targets;
                  Mapping := Component_Info.Va_Map;
               when Alloc.Map.READER_EVENTS =>
                  Targets_List_R_W := Alloc.Config.Readers_Read_Write_Targets;
                  Targets_List_R   := Alloc.Config.Readers_Read_Only_Targets;
                  Mapping := Component_Info.Reader_Events_Map;
               when Alloc.Map.WRITER_EVENTS =>
                  Targets_List_R_W := Alloc.Config.Writers_Read_Write_Targets;
                  Targets_List_R   := Alloc.Config.Writers_Read_Only_Targets;
                  Mapping := Component_Info.Writer_Events_Map;
            end case;

            --  Put Targets within Subject on todo-list if resource
            --  is not given or fixed by component.
            --  Assign it, when fixed by referenced component or by an event.
            --  Read_Only targets must come first as they may contain new
            --  relevant virtual resources.
            Targets := McKae.XML.XPath.XIA.XPath_Query
              (N     => Subject,
               XPath => Alloc.Map.Get_Target_String
                 (Target_List => Targets_List_R,
                  Prefix      => "."));
            Assign_Resources_If_Known
              (Targets          => Targets,
               Todo_List        => Todo_List,
               Available_Memory => Available_Memory,
               Mapping          => Mapping,
               Read_Only        => True,
               Run_Type         => Run_Type);

            Targets := McKae.XML.XPath.XIA.XPath_Query
              (N     => Subject,
               XPath => Alloc.Map.Get_Target_String
                 (Target_List => Targets_List_R_W,
                  Prefix      => "."));
            Assign_Resources_If_Known
              (Targets          => Targets,
               Todo_List        => Todo_List,
               Available_Memory => Available_Memory,
               Mapping          => Mapping,
               Read_Only        => False,
               Run_Type         => Run_Type);
         end;

         --  allocate new resources for remaining nodes
         if Todo_List.Length > 0 then
            --  finish restriction of available memory
            for Addr_And_Size of Mapping loop
               Alloc.Map.Subtract_Memory_Interval
                 (List => Available_Memory,
                  First_Address => Addr_And_Size.First_Address,
                  Size          => Addr_And_Size.Size);
            end loop;

            --  iterate over todo-nodes and allocate
            Mulog.Log (Msg => "Setting " & Run_Type'Image & " for "
                         & Todo_List.Length'Img
                         & " elements of subject '"
                         & DOM.Core.Elements.Get_Attribute
                         (Elem => Subject,
                          Name => "name"));
            Allocate_Todo_Nodes (Todo_List => Todo_List,
                                 Av_Mem    => Available_Memory,
                                 Run_Type  => Run_Type);
         end if;
      end Process_Subject;
   begin
      Mulog.Log (Msg => "Processing joined policy");
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => Policy_Joined);

      --  Read global information about sizes of channels and memory
      --  regions (stored in hashmap for performance reasons).
      Initialize_Size_Maps (Policy => Policy);

      --  Initialize information about the channels in memory regions in the
      --  'components' sections. The targets are defined in alloc-config.ads.
      Initialize_Components_Map (Policy => Policy);

      --  main loop: iterate over subjects
      Mulog.Log (Msg => "Analyzing virtual resources of each subject");
      declare
         use type DOM.Core.Node;

         Subjects : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
           (N     => Policy.Doc,
            XPath => "/system/subjects/subject");
         Curr_Subject : DOM.Core.Node;
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
            Curr_Subject := DOM.Core.Nodes.Item (List  => Subjects,
                                                 Index => I);

            --  get component info
            declare
               Component_Name : constant String
                 := Muxml.Utils.Get_Attribute
                 (Doc   => Curr_Subject,
                  XPath => "component",
                  Name  => "ref");
               Sibling_Node  : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                 (Doc   => Curr_Subject,
                  XPath => "sibling");
            begin
               --  Virtual memory in siblings cannot be autoallocated at the moment
               --  (OK because siblings must not specify memory)
               if Sibling_Node = null then
                  if Component_To_Map_Package.Contains
                    (Container => Components_Map,
                     Key       => Component_Name)
                  then
                     Process_Subject
                       (Subject        => Curr_Subject,
                        Component_Info => Components_Map (Component_Name),
                        Run_Type       => Alloc.Map.VIRTUAL_ADDRESSES);
                     Process_Subject
                       (Subject        => Curr_Subject,
                        Component_Info => Components_Map (Component_Name),
                        Run_Type       => Alloc.Map.READER_EVENTS);
                     Process_Subject
                       (Subject        => Curr_Subject,
                        Component_Info => Components_Map (Component_Name),
                        Run_Type       => Alloc.Map.WRITER_EVENTS);
                  else
                     raise Validation_Error with
                       "Cannot find component with name '"
                       & Component_Name
                       & "'";
                  end if;
               end if;
            end;
         end loop;

         --  Write output data
         if not Ada.Directories.Exists
           (Ada.Directories.Containing_Directory (Output_File_Name))
         then
            Ada.Directories.Create_Path
              (Ada.Directories.Containing_Directory (Output_File_Name));
         end if;
         Muxml.Write
           (File => Output_File_Name,
            Kind => Muxml.Format_Src,
            Data => Policy);
         Mulog.Log (Msg => "Successfully wrote joined policy with virtual"
                      & "resources to '"
                      & Output_File_Name & "'");
      end;
   end Run;

   ----------------------------------------------------------------------

   function To_String (I : Address_And_Size_Type) return String
   is
   begin
      return "(First_Address="
        & Mutools.Utils.To_Hex (Number => I.First_Address)
        & ", Size="
        & Alloc.Map.To_String (I.Size)
        & ")";
   end To_String;

end Vres_Alloc;
