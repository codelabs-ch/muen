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

   procedure Include_Single_Node
     (Av_Mem    : in out Alloc.Map.VA_Regions_Type;
      Todo_List : in out Alloc.Map.Node_List_Package.List;
      Node      : DOM.Core.Node;
      Read_Only : Boolean;
      Run_Type  : Alloc.Map.Run_Type_Type);

   procedure Include_Array
     (Av_Mem    : in out Alloc.Map.VA_Regions_Type;
      Todo_List : in out Alloc.Map.Node_List_Package.List;
      Node      : DOM.Core.Node;
      Read_Only : Boolean;
      Run_Type  : Alloc.Map.Run_Type_Type);

   procedure Allocate_Array
     (Av_Mem   : in out Alloc.Map.VA_Regions_Type;
      Node     :        DOM.Core.Node;
      Run_Type : Alloc.Map.Run_Type_Type);

   -------------------------------------------------------------------------

   procedure Allocate_Array
     (Av_Mem   : in out Alloc.Map.VA_Regions_Type;
      Node     :        DOM.Core.Node;
      Run_Type : Alloc.Map.Run_Type_Type)
   is
      use type Interfaces.Unsigned_64;
      Count : constant Interfaces.Unsigned_64
        := Muxml.Utils.Count_Element_Children (Node => Node);
      --  We prevent Count from reaching 0 when arrays are empty.
      --  Setting the resource-base to a default (like 0) may case clashes
      --  in the component or toolchain code.
      --  If elementSize is 0, there is a mistake and Reserve_Memory
      --  will raise an exception.
      Count_Not_Zero : constant Interfaces.Unsigned_64
        := (if Count > 0 then Count else 1);
      Size : Interfaces.Unsigned_64;
      New_Address : Interfaces.Unsigned_64;
   begin
      case Run_Type is
         when Alloc.Map.VIRTUAL_ADDRESSES =>
            Size := Count_Not_Zero * Interfaces.Unsigned_64'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "elementSize"));
            New_Address := Alloc.Map.Reserve_Memory
              (List => Av_Mem,
               Size => Size);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Node,
               Name  => "virtualAddressBase",
               Value => Mutools.Utils.To_Hex (Number => New_Address));
         when Alloc.Map.READER_EVENTS =>
            New_Address := Alloc.Map.Reserve_Memory
              (List => Av_Mem,
               Size => Count_Not_Zero);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Node,
               Name  => "vectorBase",
               Value => Alloc.Map.To_String (New_Address));
         when Alloc.Map.WRITER_EVENTS =>
            New_Address := Alloc.Map.Reserve_Memory
              (List => Av_Mem,
               Size => Count_Not_Zero);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Node,
               Name  => "eventBase",
               Value => Alloc.Map.To_String (New_Address));
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
                                   XPath => "/*/expressions");
      Mucfgcheck.Config.Conditional_Config_Var_Refs (XML_Data => Data);
      Mutools.Conditionals.Expand (Policy => Data);
      Mutools.Substitutions.Process_Attributes (Data => Data);

      Mucfgcheck.Validation_Errors.Check;
   end Expand_Expr_Sub_Cond;

   -------------------------------------------------------------------------

   procedure Include_Array
     (Av_Mem    : in out Alloc.Map.VA_Regions_Type;
      Todo_List : in out Alloc.Map.Node_List_Package.List;
      Node      : DOM.Core.Node;
      Read_Only : Boolean;
      Run_Type  : Alloc.Map.Run_Type_Type)
   is
      use type Interfaces.Unsigned_64;

      Attr_Value : constant String
        := Alloc.Map.Get_Resource_Value (Elem => Node, Run_Type => Run_Type);
      Size : constant Interfaces.Unsigned_64
        := Alloc.Map.Get_Resource_Size (Elem => Node, Run_Type => Run_Type);
   begin
      if Attr_Value = "" or Attr_Value = "auto" then
         --  the resources needs to be written - put it on the todo-list
         if not Read_Only then
            Alloc.Map.Node_List_Package.Append
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
         --  the resource is set - exclude from domain
         Alloc.Map.Subtract_Memory_Interval
           (List          => Av_Mem,
            First_Address => Interfaces.Unsigned_64'Value (Attr_Value),
            Size          => Size
              * Muxml.Utils.Count_Element_Children (Node => Node));
      end if;
   end Include_Array;

   -------------------------------------------------------------------------

   procedure Include_Single_Node
     (Av_Mem    : in out Alloc.Map.VA_Regions_Type;
      Todo_List : in out Alloc.Map.Node_List_Package.List;
      Node      : DOM.Core.Node;
      Read_Only : Boolean;
      Run_Type  : Alloc.Map.Run_Type_Type)
   is
      Attr_Value : constant String
        := Alloc.Map.Get_Resource_Value (Elem => Node, Run_Type => Run_Type);
      Size : constant Interfaces.Unsigned_64
        := Alloc.Map.Get_Resource_Size (Elem => Node, Run_Type => Run_Type);
   begin
      if Attr_Value = "auto" or Attr_Value = "" then
         --  if the node is missing a resource, add it to the todo-list
         if not Read_Only then
            Alloc.Map.Node_List_Package.Append
              (Container => Todo_List,
               New_Item  => Node);
         else
            Mulog.Log (Msg => "Error: Found node with false "
                         & "'virtualAddress'/'vector'/'event'/'id' "
                         & "attribute value. Xpath: '"
                         & Mutools.Xmldebuglog.Get_Xpath (Node => Node)
                         & "', value: '"
                         & Attr_Value
                         & "'");
            raise Validation_Error with "Invalid attribute value";
         end if;
      else
         --  if the resource is set already, exclude it from Av_Mem
         Alloc.Map.Subtract_Memory_Interval
           (List          => Av_Mem,
            First_Address => Interfaces.Unsigned_64'Value (Attr_Value),
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
      Available_Memory : Alloc.Map.VA_Regions_Type;
      Node             : DOM.Core.Node;
      Todo_List        : Alloc.Map.Node_List_Package.List;

      --  Depending on Run_Type, create and/or assign
      --  'virtualAddress', 'event' and 'vector' attributes in
      --  channel-reader or channel-writer nodes and the corresponding arrays.
      procedure Assign_Missing_Virtual_Resources
        (Run_Type : Alloc.Map.Run_Type_Type);

      ----------------------------------------------------------------------

      --  Iterate over given list and trigger "inclusion" for each node
      procedure Include_Node_List
        (Node_List : DOM.Core.Node_List;
         Read_Only : Boolean;
         Run_Type  : Alloc.Map.Run_Type_Type);

      ----------------------------------------------------------------------

      procedure Assign_Missing_Virtual_Resources
        (Run_Type : Alloc.Map.Run_Type_Type)
      is
      begin
         Alloc.Map.Node_List_Package.Clear (Container => Todo_List);
         Alloc.Map.Clear (List => Available_Memory);

         --  Initialize the domain of the virtual resource
         case Run_Type is
            when Alloc.Map.VIRTUAL_ADDRESSES =>
               Node := DOM.Core.Documents.Get_Element (Doc => Policy.Doc);
               if DOM.Core.Elements.Get_Attribute (Elem => Node, Name => "profile")
                 = "native"
               then
                  Alloc.Map.Add_Memory_Interval
                    (List     => Available_Memory,
                     Interval => Va_Space_Native);
               else
                  Alloc.Map.Add_Memory_Interval
                    (List     => Available_Memory,
                     Interval => Va_Space_Vm);
               end if;
            when Alloc.Map.WRITER_EVENTS =>
               Alloc.Map.Add_Memory_Interval
                 (List     => Available_Memory,
                  Interval => Event_Numbers_Domain);
            when Alloc.Map.READER_EVENTS =>
               Alloc.Map.Add_Memory_Interval
                 (List     => Available_Memory,
                  Interval => Vector_Numbers_Domain);
         end case;

         --  Block domain-space taken by existsing virtual resources
         --  and fill todo-list
         Mulog.Log (Msg => "Analyzing resources for "
                   & Run_Type'Image);
         declare
            Target_List_R_W : constant Mutools.String_Vector.Vector
              := (case Run_Type is
                 when Alloc.Map.VIRTUAL_ADDRESSES =>
                    Alloc.Config.C_Va_Alloc_Read_Write_Targets,
                 when Alloc.Map.WRITER_EVENTS =>
                    Alloc.Config.C_Writers_Read_Write_Targets,
                 when Alloc.Map.READER_EVENTS =>
                    Alloc.Config.C_Readers_Read_Write_Targets);
            Target_List_R : constant Mutools.String_Vector.Vector
              := (case Run_Type is
                 when Alloc.Map.VIRTUAL_ADDRESSES =>
                    Alloc.Config.C_Va_Alloc_Read_Only_Targets,
                 when Alloc.Map.WRITER_EVENTS =>
                    Alloc.Config.C_Writers_Read_Only_Targets,
                 when Alloc.Map.READER_EVENTS =>
                    Alloc.Config.C_Readers_Read_Only_Targets);

            R_W_Targets : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
              (N     => Policy.Doc,
               XPath => Alloc.Map.Get_Target_String
                 (Target_List => Target_List_R_W,
                  Prefix      => "/component"));
            R_Targets : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
              (N     => Policy.Doc,
               XPath => Alloc.Map.Get_Target_String
                 (Target_List => Target_List_R,
                  Prefix      => "/component"));
         begin
            Include_Node_List (Node_List => R_Targets,
                               Read_Only => True,
                               Run_Type  => Run_Type);
            Include_Node_List (Node_List => R_W_Targets,
                               Read_Only => False,
                               Run_Type  => Run_Type);
         end;

         --  iterate over nodes on todo-list and allocate from Available_Memory
         Mulog.Log (Msg => "Setting " & Run_Type'Image & " for "
                      & Todo_List.Length'Img
                      & " nodes");
         declare
            use type Alloc.Map.Node_List_Package.Cursor;

            Curr : Alloc.Map.Node_List_Package.Cursor
              := Todo_List.First;
            Curr_Node : DOM.Core.Node;
         begin
            while Curr /= Alloc.Map.Node_List_Package.No_Element loop
               Curr_Node :=  Alloc.Map.Node_List_Package.Element (Curr);
               if DOM.Core.Elements.Get_Tag_Name (Elem => Curr_Node) /= "array"
               then
                  Alloc.Map.Allocate_And_Set_Single_Resource
                    (Av_Mem   => Available_Memory,
                     Node     => Curr_Node,
                     Run_Type => Run_Type);
               else
                  Allocate_Array
                    (Av_Mem   => Available_Memory,
                     Node     => Curr_Node,
                     Run_Type => Run_Type);
               end if;
               Alloc.Map.Node_List_Package.Next (Curr);
            end loop;
         end;
      end Assign_Missing_Virtual_Resources;

      ----------------------------------------------------------------------

      procedure Include_Node_List
        (Node_List : DOM.Core.Node_List;
         Read_Only : Boolean;
         Run_Type  : Alloc.Map.Run_Type_Type)
      is
         Curr_Node : DOM.Core.Node;
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Node_List) - 1 loop
            Curr_Node := DOM.Core.Nodes.Item (List  => Node_List,
                                              Index => I);

            if DOM.Core.Elements.Get_Tag_Name (Elem => Curr_Node) /= "array"
            then
               Include_Single_Node
                 (Av_Mem    => Available_Memory,
                  Todo_List => Todo_List,
                  Node      => Curr_Node,
                  Read_Only => Read_Only,
                  Run_Type  => Run_Type);
            else
               Include_Array
                 (Av_Mem    => Available_Memory,
                  Todo_List => Todo_List,
                  Node      => Curr_Node,
                  Read_Only => Read_Only,
                  Run_Type  => Run_Type);
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
        (Run_Type => Alloc.Map.VIRTUAL_ADDRESSES);
      Assign_Missing_Virtual_Resources
        (Run_Type => Alloc.Map.READER_EVENTS);
      Assign_Missing_Virtual_Resources
        (Run_Type => Alloc.Map.WRITER_EVENTS);

      --  Write output with validation
      if not Ada.Directories.Exists
        (Ada.Directories.Containing_Directory (Output_File_Name))
      then
         Ada.Directories.Create_Path
           (Ada.Directories.Containing_Directory (Output_File_Name));
      end if;
      Muxml.Write
        (File => Output_File_Name,
         Kind => Muxml.Component,
         Data => Policy);
      Mulog.Log (Msg => "Successfully created policy '"
                   & Output_File_Name & "'");
   end Run;
end Comp_Vres_Alloc;
