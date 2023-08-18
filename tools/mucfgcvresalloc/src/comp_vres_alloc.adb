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

with Interfaces;

with Ada.Directories;

with GNAT.Directory_Operations;

with DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mucfgcheck.Config;
with Mucfgcheck.Validation_Errors;

with Mutools;
with Mutools.Strings;
with Mutools.Utils;
with Mutools.XML_Utils;
with Mutools.Expressions;
with Mutools.Conditionals;
with Mutools.Substitutions;
with Mutools.Xmldebuglog;

with Mulog;

with Muxml;
with Muxml.Utils;

package body Comp_Vres_Alloc
is
   --  Check and expand expressions and conditionals in given input spec.
   procedure Expand_Expr_Sub_Cond (Data : Muxml.XML_Data_Type);

   --  For a node containing a single virtual resource of the specified kind
   --  (i.e., not an array):
   --  If the virtual resource is not set, append the node to Todo_List.
   --  If the virtual resource is set, subtract the used domain space from
   --  Av_Ival.
   --  Raises an exception if Read_Only is true and the resource is not set.
   procedure Include_Single_Node
     (Av_Ival       : in out Mutools.Intervals.Interval_List_Type;
      Todo_List     : in out Muxml.Utils.Node_List_Package.List;
      Node          :        DOM.Core.Node;
      Read_Only     :        Boolean;
      Resource_Kind :        Mutools.Vres_Alloc.Resource_Kind_Type);

   --  For an array with virtual resources for each element:
   --  If the virtual resource is not set, append the array to Todo_List.
   --  If the virtual resource is set, subtract the used domain space from
   --  Av_Ival.
   --  Raises an exception if Read_Only is true and the resource is not set.
   procedure Include_Array
     (Av_Ival       : in out Mutools.Intervals.Interval_List_Type;
      Todo_List     : in out Muxml.Utils.Node_List_Package.List;
      Node          :        DOM.Core.Node;
      Read_Only     :        Boolean;
      Resource_Kind :        Mutools.Vres_Alloc.Resource_Kind_Type);

   --  Reserve and set the requested virtual resource for an array.
   --  If the array is empty, the requested resource is set to the last
   --  element of the respective domain. No resources are reserved in that
   --  case because resources of empty arrays must never be used and
   --  will not appear in policy-b.xml.
   procedure Allocate_Array
     (Av_Ival       : in out Mutools.Intervals.Interval_List_Type;
      Node          :        DOM.Core.Node;
      Resource_Kind :        Mutools.Vres_Alloc.Resource_Kind_Type);

   -------------------------------------------------------------------------

   procedure Allocate_Array
     (Av_Ival       : in out Mutools.Intervals.Interval_List_Type;
      Node          :        DOM.Core.Node;
      Resource_Kind :        Mutools.Vres_Alloc.Resource_Kind_Type)
   is
      use type Interfaces.Unsigned_64;
      use type Muxml.String_Vector.Vector;

      Name_Filter    : constant Muxml.String_Vector.Vector
        := Muxml.String_Vector."&" ("reader", "writer") & "memory";
      Count          : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64
        (Muxml.Utils.Count_Element_Children
           (Node        => Node,
            Name_Filter => Name_Filter));
      Size           : Interfaces.Unsigned_64;
      New_Address    : Interfaces.Unsigned_64;
   begin
      case Resource_Kind is
         when Mutools.Vres_Alloc.Virtual_Addresses =>
            Size := Interfaces.Unsigned_64'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "elementSize"));
            if not Mutools.Vres_Alloc.Is_Aligned (Size => Size) then
               Mulog.Log (Msg => "Error: elementSize is not "
                            & "a multiple of 16#1000#. XPath: '"
                            & Mutools.Xmldebuglog.Get_Xpath (Node => Node)
                            & "', elementSize: '"
                            & Mutools.Utils.To_Hex (Number => Size)
                            & "'");
               raise Validation_Error with "Virtual resource not aligned";
            end if;

            if Count = 0 then
               New_Address := Interfaces.Unsigned_64'Last;
            else
               New_Address := Mutools.Intervals.Reserve_Interval
                 (List => Av_Ival,
                  Size => Count * Size);
            end if;
            DOM.Core.Elements.Set_Attribute
              (Elem  => Node,
               Name  => "virtualAddressBase",
               Value => Mutools.Utils.To_Hex (Number => New_Address));

         when Mutools.Vres_Alloc.Reader_Vectors =>
            if Count = 0 then
               New_Address := 255;
            else
               New_Address := Mutools.Intervals.Reserve_Interval
                 (List => Av_Ival,
                  Size => Count);
            end if;
            DOM.Core.Elements.Set_Attribute
              (Elem  => Node,
               Name  => "vectorBase",
               Value => Mutools.Utils.To_Decimal (New_Address));
         when Mutools.Vres_Alloc.Writer_Events =>
            if Count = 0 then
               New_Address := 63;
            else
               New_Address := Mutools.Intervals.Reserve_Interval
                 (List => Av_Ival,
                  Size => Count);
            end if;
            DOM.Core.Elements.Set_Attribute
              (Elem  => Node,
               Name  => "eventBase",
               Value => Mutools.Utils.To_Decimal (New_Address));
      end case;
   end Allocate_Array;

   -------------------------------------------------------------------------

   procedure Expand_Expr_Sub_Cond (Data : Muxml.XML_Data_Type)
   is
   begin
      Mucfgcheck.Config.Config_Boolean_Values (XML_Data => Data);
      Mucfgcheck.Config.Config_Integer_Values (XML_Data => Data);
      Mucfgcheck.Config.Expression_Config_Var_Refs (XML_Data => Data);
      Mucfgcheck.Config.Expression_Integer_Values (XML_Data => Data);
      Mucfgcheck.Config.Expression_Boolean_Values (XML_Data => Data);

      Mutools.Expressions.Expand (Policy => Data);
      Muxml.Utils.Remove_Elements (Doc   => Data.Doc,
                                   XPath => "/component/expressions"
                                     & " | /library/expressions");
      Mucfgcheck.Config.Conditional_Config_Var_Refs (XML_Data => Data);
      Mutools.Conditionals.Expand (Policy => Data);
      Mutools.Substitutions.Process_Attributes (Data => Data);

      Mucfgcheck.Validation_Errors.Check;
   end Expand_Expr_Sub_Cond;

   -------------------------------------------------------------------------

   procedure Include_Array
     (Av_Ival       : in out Mutools.Intervals.Interval_List_Type;
      Todo_List     : in out Muxml.Utils.Node_List_Package.List;
      Node          :        DOM.Core.Node;
      Read_Only     :        Boolean;
      Resource_Kind :        Mutools.Vres_Alloc.Resource_Kind_Type)
   is
      use type Interfaces.Unsigned_64;
      use type Mutools.Vres_Alloc.Resource_Kind_Type;
      use type Muxml.String_Vector.Vector;

      Attr_Value  : constant String
        := Mutools.Vres_Alloc.Get_Resource_Value
        (Elem          => Node,
         Resource_Kind => Resource_Kind);
      Size        : constant Interfaces.Unsigned_64
        := Mutools.Vres_Alloc.Get_Resource_Size
        (Elem          => Node,
         Resource_Kind => Resource_Kind);
      Name_Filter : constant Muxml.String_Vector.Vector
        := Muxml.String_Vector."&" ("reader", "writer") & "memory";
   begin
      if Attr_Value = "" or Attr_Value = "auto" then
         --  the resources needs to be written - put it on the todo-list
         if not Read_Only then
            Muxml.Utils.Node_List_Package.Append
              (Container => Todo_List,
               New_Item  => Node);
         else
            Mulog.Log (Msg => "Found read-only node which requested automatic "
                         & "allocation of virtual resource. Xpath: '"
                         & Mutools.Xmldebuglog.Get_Xpath (Node => Node)
                         & "', value: '"
                         & Attr_Value
                         & "'");
            raise Validation_Error with "Invalid attribute value";
         end if;
      else
         --  Because the resource is set, we exclude it from the domain.
         if Resource_Kind = Mutools.Vres_Alloc.Virtual_Addresses and then
           not Mutools.Vres_Alloc.Is_Aligned
           (Address => Interfaces.Unsigned_64'Value (Attr_Value),
            Size    => Size)
         then
            Mulog.Log (Msg => "Error: Size or virtual address of node is not "
                         & "a multiple of 16#1000#. XPath: '"
                         & Mutools.Xmldebuglog.Get_Xpath (Node => Node)
                         & "', Size: '"
                         & Mutools.Utils.To_Hex (Number => Size)
                         & "', Virtual address: '"
                         & Attr_Value
                         & "'");
            raise Validation_Error with "Virtual resource not aligned";
         end if;
         Mutools.Intervals.Subtract_Interval
           (List          => Av_Ival,
            First_Element => Interfaces.Unsigned_64'Value (Attr_Value),
            Size          => Size * Interfaces.Unsigned_64
              (Muxml.Utils.Count_Element_Children
                 (Node => Node,
                  Name_Filter => Name_Filter)));
      end if;
   end Include_Array;

   -------------------------------------------------------------------------

   procedure Include_Single_Node
     (Av_Ival       : in out Mutools.Intervals.Interval_List_Type;
      Todo_List     : in out Muxml.Utils.Node_List_Package.List;
      Node          :        DOM.Core.Node;
      Read_Only     :        Boolean;
      Resource_Kind :        Mutools.Vres_Alloc.Resource_Kind_Type)
   is
      use type Mutools.Vres_Alloc.Resource_Kind_Type;

      Attr_Value : constant String
        := Mutools.Vres_Alloc.Get_Resource_Value
        (Elem          => Node,
         Resource_Kind => Resource_Kind);
      Size       : constant Interfaces.Unsigned_64
        := Mutools.Vres_Alloc.Get_Resource_Size
        (Elem          => Node,
         Resource_Kind => Resource_Kind);
   begin
      if Attr_Value = "auto" or Attr_Value = "" then
         --  If the node is missing a resource, add it to the todo-list.
         if not Read_Only then
            Muxml.Utils.Node_List_Package.Append
              (Container => Todo_List,
               New_Item  => Node);
         else
            Mulog.Log (Msg => "Found read-only node which requested automatic "
                         & "allocation of virtual resource. Xpath: '"
                         & Mutools.Xmldebuglog.Get_Xpath (Node => Node)
                         & "', value: '"
                         & Attr_Value
                         & "'");
            raise Validation_Error with "Invalid attribute value";
         end if;
      else
         --  If the resource is set already, exclude it from Av_Ival.
         if Resource_Kind = Mutools.Vres_Alloc.Virtual_Addresses and then
           not Mutools.Vres_Alloc.Is_Aligned
           (Address => Interfaces.Unsigned_64'Value (Attr_Value),
            Size    => Size)
         then
            Mulog.Log (Msg => "Error: Size or virtual address of node is not "
                         & "a multiple of 16#1000#. XPath: '"
                         & Mutools.Xmldebuglog.Get_Xpath (Node => Node)
                         & "', Size: '"
                         & Mutools.Utils.To_Hex (Number => Size)
                         & "', Virtual address: '"
                         & Attr_Value
                         & "'");
            raise Validation_Error with "Virtual resource not aligned";
         end if;
         Mutools.Intervals.Subtract_Interval
           (List          => Av_Ival,
            First_Element => Interfaces.Unsigned_64'Value (Attr_Value),
            Size          => Size);
      end if;
   end Include_Single_Node;

   -------------------------------------------------------------------------

   procedure Run
     (Input_Spec       : String;
      Include_Path     : String;
      Output_File_Name : String)
   is
      Policy           : Muxml.XML_Data_Type;
      Node             : DOM.Core.Node;

      --  Depending on Resource_Kind, create and/or assign
      --  'virtualAddress', 'event' and 'vector' attributes in
      --  channel-reader, channel-writer and memory nodes and in the
      --  corresponding arrays.
      procedure Assign_Missing_Virtual_Resources
        (Resource_Kind : Mutools.Vres_Alloc.Resource_Kind_Type);

      ----------------------------------------------------------------------

      --  Iterate over Node_List and trigger "inclusion" for each node,
      --  i.e., either block the resources taken by it or put it on Todo_List
      --  for allocation lateron.
      procedure Include_Node_List
        (Av_Ival       : in out Mutools.Intervals.Interval_List_Type;
         Todo_List     : in out Muxml.Utils.Node_List_Package.List;
         Node_List     :        DOM.Core.Node_List;
         Read_Only     :        Boolean;
         Resource_Kind :        Mutools.Vres_Alloc.Resource_Kind_Type);

      ----------------------------------------------------------------------

      procedure Assign_Missing_Virtual_Resources
        (Resource_Kind : Mutools.Vres_Alloc.Resource_Kind_Type)
      is
         Todo_List           : Muxml.Utils.Node_List_Package.List;
         Available_Intervals : Mutools.Intervals.Interval_List_Type;
      begin
         --  Initialize the domain of the virtual resource.
         case Resource_Kind is
            when Mutools.Vres_Alloc.Virtual_Addresses =>
               if not (Mutools.Vres_Alloc.Is_Aligned
                         (Address => Va_Space_Native.First_Element)
                  and Mutools.Vres_Alloc.Is_Aligned
                         (Address => Va_Space_Vm.First_Element))
               then
                  raise Validation_Error with
                    "Default values for virtual address domains are not "
                    & "aligned to page size.";
               end if;
               Node := DOM.Core.Documents.Get_Element (Doc => Policy.Doc);
               if "native" = DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "profile")
               then
                  Mutools.Intervals.Add_Interval
                    (List     => Available_Intervals,
                     Interval => Va_Space_Native);
               else
                  Mutools.Intervals.Add_Interval
                    (List     => Available_Intervals,
                     Interval => Va_Space_Vm);
               end if;
            when Mutools.Vres_Alloc.Writer_Events =>
               Mutools.Intervals.Add_Interval
                 (List     => Available_Intervals,
                  Interval => Event_Numbers_Domain);
            when Mutools.Vres_Alloc.Reader_Vectors =>
               Mutools.Intervals.Add_Interval
                 (List     => Available_Intervals,
                  Interval => Vector_Numbers_Domain);
         end case;

         --  Block domain-space taken by existsing virtual resources
         --  and fill todo-list.
         Mulog.Log (Msg => "Analyzing resources for "
                   & Resource_Kind'Image);
         declare
            Target_List_R_W : constant Mutools.String_Vector.Vector
              := (case Resource_Kind is
                 when Mutools.Vres_Alloc.Virtual_Addresses =>
                    Mutools.Vres_Alloc.Config.C_Va_Alloc_Read_Write_Targets,
                 when Mutools.Vres_Alloc.Writer_Events =>
                    Mutools.Vres_Alloc.Config.C_Writers_Read_Write_Targets,
                 when Mutools.Vres_Alloc.Reader_Vectors =>
                    Mutools.Vres_Alloc.Config.C_Readers_Read_Write_Targets);
            Target_List_R : constant Mutools.String_Vector.Vector
              := (case Resource_Kind is
                 when Mutools.Vres_Alloc.Virtual_Addresses =>
                    Mutools.Vres_Alloc.Config.C_Va_Alloc_Read_Only_Targets,
                 when Mutools.Vres_Alloc.Writer_Events =>
                    Mutools.Vres_Alloc.Config.C_Writers_Read_Only_Targets,
                 when Mutools.Vres_Alloc.Reader_Vectors =>
                    Mutools.Vres_Alloc.Config.C_Readers_Read_Only_Targets);

            R_W_Targets : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
              (N     => Policy.Doc,
               XPath => Mutools.Vres_Alloc.Get_Target_String
                 (Target_List => Target_List_R_W,
                  Prefix      => "/component"));
            R_Targets : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
              (N     => Policy.Doc,
               XPath => Mutools.Vres_Alloc.Get_Target_String
                 (Target_List => Target_List_R,
                  Prefix      => "/component"));
         begin
            Include_Node_List (Av_Ival       => Available_Intervals,
                               Todo_List     => Todo_List,
                               Node_List     => R_Targets,
                               Read_Only     => True,
                               Resource_Kind => Resource_Kind);
            Include_Node_List (Av_Ival       => Available_Intervals,
                               Todo_List     => Todo_List,
                               Node_List     => R_W_Targets,
                               Read_Only     => False,
                               Resource_Kind => Resource_Kind);
         end;

         --  Iterate over nodes on todo-list and allocate from
         --  Available_Intervals.
         Mulog.Log (Msg => "Setting " & Resource_Kind'Image & " for"
                      & Todo_List.Length'Img
                      & " nodes");
         declare
            use type Muxml.Utils.Node_List_Package.Cursor;

            Curr : Muxml.Utils.Node_List_Package.Cursor
              := Todo_List.First;
            Curr_Node : DOM.Core.Node;
         begin
            while Curr /= Muxml.Utils.Node_List_Package.No_Element loop
               Curr_Node :=  Muxml.Utils.Node_List_Package.Element (Curr);
               if DOM.Core.Elements.Get_Tag_Name (Elem => Curr_Node) /= "array"
               then
                  Mutools.Vres_Alloc.Allocate_And_Set_Single_Resource
                    (Av_Ival       => Available_Intervals,
                     Node          => Curr_Node,
                     Resource_Kind => Resource_Kind);
               else
                  Allocate_Array
                    (Av_Ival       => Available_Intervals,
                     Node          => Curr_Node,
                     Resource_Kind => Resource_Kind);
               end if;
               Muxml.Utils.Node_List_Package.Next (Curr);
            end loop;
         end;
      end Assign_Missing_Virtual_Resources;

      ----------------------------------------------------------------------

      procedure Include_Node_List
        (Av_Ival       : in out Mutools.Intervals.Interval_List_Type;
         Todo_List     : in out Muxml.Utils.Node_List_Package.List;
         Node_List     :        DOM.Core.Node_List;
         Read_Only     :        Boolean;
         Resource_Kind :        Mutools.Vres_Alloc.Resource_Kind_Type)
      is
         Curr_Node : DOM.Core.Node;
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Node_List) - 1 loop
            Curr_Node := DOM.Core.Nodes.Item (List  => Node_List,
                                              Index => I);

            if DOM.Core.Elements.Get_Tag_Name (Elem => Curr_Node) /= "array"
            then
               Include_Single_Node
                 (Av_Ival       => Av_Ival,
                  Todo_List     => Todo_List,
                  Node          => Curr_Node,
                  Read_Only     => Read_Only,
                  Resource_Kind => Resource_Kind);
            else
               Include_Array
                 (Av_Ival       => Av_Ival,
                  Todo_List     => Todo_List,
                  Node          => Curr_Node,
                  Read_Only     => Read_Only,
                  Resource_Kind => Resource_Kind);
            end if;
         end loop;
      end Include_Node_List;
   begin
      Mulog.Log (Msg => "Processing component specification");
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.None,
                   File => Input_Spec);

      Mulog.Log (Msg => "Expanding XIncludes, expressions, substitutions and "
                   & "condititionals");
      declare
         Inc_Path_Str : constant String
           := Include_Path & (if Include_Path'Length > 0 then ":" else "")
             & GNAT.Directory_Operations.Dir_Name (Path => Input_Spec);
      begin
         Mulog.Log (Msg => "Using include path '" & Inc_Path_Str & "'");
         Mutools.XML_Utils.Merge_XIncludes
           (Policy       => Policy,
            Include_Dirs => Mutools.Strings.Tokenize (Str => Inc_Path_Str));
      end;
      Expand_Expr_Sub_Cond (Data => Policy);

      Assign_Missing_Virtual_Resources
        (Resource_Kind => Mutools.Vres_Alloc.Virtual_Addresses);
      Assign_Missing_Virtual_Resources
        (Resource_Kind => Mutools.Vres_Alloc.Reader_Vectors);
      Assign_Missing_Virtual_Resources
        (Resource_Kind => Mutools.Vres_Alloc.Writer_Events);

      --  Write output with validation.
      if not Ada.Directories.Exists
        (Ada.Directories.Containing_Directory (Output_File_Name))
      then
         Ada.Directories.Create_Path
           (Ada.Directories.Containing_Directory (Output_File_Name));
      end if;
      Muxml.Write
        (File => Output_File_Name,
         Kind => Muxml.Component_Ext,
         Data => Policy);
      Mulog.Log (Msg => "Successfully created policy '"
                   & Output_File_Name & "'");
   end Run;
end Comp_Vres_Alloc;
