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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.XML_Utils;

package body Expanders.Components
is

   -------------------------------------------------------------------------

   procedure Add_Binaries (Data : in out Muxml.XML_Data_Type)
   is
      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/components/component");
      Subjects   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[@name!='tau0']");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Comp_Ref_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "component");
            Comp_Ref : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Comp_Ref_Node,
                 Name => "ref");
            Comp_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Components,
                 Ref_Attr  => "name",
                 Ref_Value => Comp_Ref);

            Bin_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Comp_Node,
                 XPath => "binary");
            Filename : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Bin_Node,
                 Name => "filename");
            Filesize : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Bin_Node,
                 Name => "size");
            Virtual_Address : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Bin_Node,
                 Name => "virtualAddress");
            Subj_Mem_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "memory");
         begin
            Mulog.Log (Msg => "Mapping binary '" & Filename & "' with size "
                       & Filesize & " at virtual address " & Virtual_Address
                       & " of subject '" & Subj_Name & "'");
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => Subj_Name & "|bin",
               Address     => "",
               Size        => Filesize,
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "subject_binary",
               File_Name   => Filename,
               File_Offset => "none");
            Muxml.Utils.Append_Child
              (Node      => Subj_Mem_Node,
               New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "binary",
                  Physical_Name => Subj_Name & "|bin",
                  Address       => Virtual_Address,
                  Writable      => True,
                  Executable    => True));
         end;
      end loop;
   end Add_Binaries;

   -------------------------------------------------------------------------

   procedure Add_Channel_Arrays (Data : in out Muxml.XML_Data_Type)
   is
      Channel_Arrays : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/components/*/requires/channels/array");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Channel_Arrays) - 1 loop
         declare
            Arr_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Channel_Arrays,
                 Index => I);
            Arr_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Arr_Node,
                 Name => "logical");
            Parent_Chan : constant DOM.Core.Node
              := DOM.Core.Nodes.Parent_Node (N => Arr_Node);
            Comp_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Parent_Node (N => Parent_Chan),
                 Name => "name");
            Address : Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Arr_Node,
                    Name => "virtualAddressBase"));
            Element_Size : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Arr_Node,
                    Name => "elementSize"));
            Event_Base_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Arr_Node,
                 Name => "eventBase");
            Vector_Base_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Arr_Node,
                 Name => "vectorBase");
            Has_Event_Base : constant Boolean
              := Event_Base_Str'Length > 0;
            Has_Vector_Base : constant Boolean
              := Vector_Base_Str'Length > 0;
            Event_Base : Natural
              := (if Has_Event_Base
                  then Natural'Value (Event_Base_Str)
                  else 0);
            Vector_Base : Natural
              := (if Has_Vector_Base
                  then Natural'Value (Vector_Base_Str)
                  else 0);
            Children : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Arr_Node,
                 XPath => "*[self::reader or self::writer]");
            Child_Count : constant Positive
              := DOM.Core.Nodes.Length (List => Children);
         begin
            Mulog.Log (Msg => "Adding" & Child_Count'Img & " channels(s) "
                       & "of array '" & Arr_Name & "' to component '"
                       & Comp_Name & "'");
            for J in 0 .. Child_Count - 1 loop
               declare
                  use type Interfaces.Unsigned_64;

                  Chan_Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Children,
                       Index => J);
                  New_Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Clone_Node
                      (N    => Chan_Node,
                       Deep => False);
               begin
                  DOM.Core.Elements.Set_Attribute
                    (Elem  => New_Node,
                     Name  => "virtualAddress",
                     Value => Mutools.Utils.To_Hex (Number => Address));
                  DOM.Core.Elements.Set_Attribute
                    (Elem  => New_Node,
                     Name  => "size",
                     Value => Mutools.Utils.To_Hex (Number => Element_Size));
                  if Has_Vector_Base then
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => New_Node,
                        Name  => "vector",
                        Value => Ada.Strings.Fixed.Trim
                          (Source => Vector_Base'Img,
                           Side   => Ada.Strings.Left));
                     Vector_Base := Vector_Base + 1;
                  end if;
                  if Has_Event_Base then
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => New_Node,
                        Name  => "event",
                        Value => Ada.Strings.Fixed.Trim
                          (Source => Event_Base'Img,
                           Side   => Ada.Strings.Left));
                     Event_Base := Event_Base + 1;
                  end if;
                  Muxml.Utils.Append_Child
                    (Node      => Parent_Chan,
                     New_Child => New_Node);
                  Address := Address + Element_Size;
               end;
            end loop;
         end;
      end loop;
   end Add_Channel_Arrays;

   -------------------------------------------------------------------------

   procedure Add_Channels (Data : in out Muxml.XML_Data_Type)
   is
      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/components/component");
      Subjects   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[@name!='tau0']");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Subj_Channel_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "channels");

            Comp_Ref_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "component");
            Comp_Ref : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Comp_Ref_Node,
                 Name => "ref");
            Mappings : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Ref_Node,
                 XPath => "map");
            Comp_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Components,
                 Ref_Attr  => "name",
                 Ref_Value => Comp_Ref);
            Comp_Channels : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Node,
                 XPath => "requires/channels/*[self::reader or self::writer]");
            Log_Channel_Count : constant Natural
              := DOM.Core.Nodes.Length (List => Comp_Channels);
         begin
            if Log_Channel_Count > 0 then
               Mulog.Log (Msg => "Expanding" & Log_Channel_Count'Img
                          & " logical channel(s) of component '" & Comp_Ref
                          & "' to subject '" & Subj_Name & "'");

               for J in 0 .. Log_Channel_Count - 1 loop
                  declare
                     Logical_Channel : constant DOM.Core.Node
                       := DOM.Core.Nodes.Clone_Node
                         (N    => DOM.Core.Nodes.Item
                            (List  => Comp_Channels,
                             Index => J),
                          Deep => False);
                     Logical_Channel_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Logical_Channel,
                          Name => "logical");
                     Physical_Channel_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Muxml.Utils.Get_Element
                            (Nodes     => Mappings,
                             Ref_Attr  => "logical",
                             Ref_Value => Logical_Channel_Name),
                          Name => "physical");
                  begin
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => Logical_Channel,
                        Name  => "physical",
                        Value => Physical_Channel_Name);
                     Muxml.Utils.Append_Child
                       (Node      => Subj_Channel_Node,
                        New_Child => Logical_Channel);
                  end;
               end loop;
            end if;
         end;
      end loop;
   end Add_Channels;

   -------------------------------------------------------------------------

   procedure Add_Devices (Data : in out Muxml.XML_Data_Type)
   is
      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/components/component");
      Subjects   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[@name!='tau0']");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node     : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name     : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Subj_Dev_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "devices");
            Comp_Ref_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "component");
            Comp_Ref      : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Comp_Ref_Node,
                 Name => "ref");
            Mappings      : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Ref_Node,
                 XPath => "map");
            Comp_Node     : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Components,
                 Ref_Attr  => "name",
                 Ref_Value => Comp_Ref);
            Comp_Devices  : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Node,
                 XPath => "requires/devices/device");
            Log_Dev_Count : constant Natural
              := DOM.Core.Nodes.Length (List => Comp_Devices);
         begin
            if Log_Dev_Count > 0 then
               Mulog.Log (Msg => "Expanding" & Log_Dev_Count'Img & " logical "
                          & "device(s) of component '" & Comp_Ref
                          & "' to subject '" & Subj_Name & "'");

               for J in 0 .. Log_Dev_Count - 1 loop
                  declare
                     Log_Dev       : constant DOM.Core.Node
                       := DOM.Core.Nodes.Clone_Node
                         (N    => DOM.Core.Nodes.Item
                            (List  => Comp_Devices,
                             Index => J),
                          Deep => True);
                     Log_Dev_Res   : constant DOM.Core.Node_List
                       := McKae.XML.XPath.XIA.XPath_Query
                         (N     => Log_Dev,
                          XPath => "*");
                     Log_Dev_Name  : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Log_Dev,
                          Name => "logical");
                     Dev_Mapping   : constant DOM.Core.Node
                       := Muxml.Utils.Get_Element
                         (Nodes     => Mappings,
                          Ref_Attr  => "logical",
                          Ref_Value => Log_Dev_Name);
                     Res_Mappings  : constant DOM.Core.Node_List
                       := McKae.XML.XPath.XIA.XPath_Query
                         (N     => Dev_Mapping,
                          XPath => "map");
                     Phys_Mem_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Dev_Mapping,
                          Name => "physical");
                  begin
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => Log_Dev,
                        Name  => "physical",
                        Value => Phys_Mem_Name);

                     for K in 0 .. DOM.Core.Nodes.Length
                       (List => Log_Dev_Res) - 1
                     loop
                        declare
                           Log_Res       : constant DOM.Core.Node
                             := DOM.Core.Nodes.Item (List  => Log_Dev_Res,
                                                     Index => K);
                           Log_Res_Name  : constant String
                             := DOM.Core.Elements.Get_Attribute
                               (Elem => Log_Res,
                                Name => "logical");
                           Phys_Res_Name : constant String
                             := Muxml.Utils.Get_Attribute
                               (Nodes     => Res_Mappings,
                                Ref_Attr  => "logical",
                                Ref_Value => Log_Res_Name,
                                Attr_Name => "physical");
                           Node_Name     : constant String
                             := DOM.Core.Nodes.Node_Name (N => Log_Res);
                        begin
                           DOM.Core.Elements.Set_Attribute
                             (Elem  => Log_Res,
                              Name  => "physical",
                              Value => Phys_Res_Name);

                           if Node_Name = "memory" then
                              DOM.Core.Elements.Remove_Attribute
                                (Elem => Log_Res,
                                 Name => "size");
                           elsif Node_Name = "ioPort" then
                              DOM.Core.Elements.Remove_Attribute
                                (Elem => Log_Res,
                                 Name => "start");
                              DOM.Core.Elements.Remove_Attribute
                                (Elem => Log_Res,
                                 Name => "end");
                           end if;
                        end;
                     end loop;

                     Muxml.Utils.Append_Child
                       (Node      => Subj_Dev_Node,
                        New_Child => Log_Dev);
                  end;
               end loop;
            end if;
         end;
      end loop;
   end Add_Devices;

   -------------------------------------------------------------------------

   procedure Add_Library_Resources (Data : in out Muxml.XML_Data_Type)
   is
      Libraries : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/components/library");

      --  Merge all child nodes of right into child nodes of left. If no child
      --  with given tag name exists in left, it is created. Child nodes are
      --  assumed to be sequences.
      procedure Merge_Childs (Left, Right : DOM.Core.Node);

      --  Recursively resolve dependencies of given component/library node.
      procedure Resolve_Depends (Node : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Merge_Childs (Left, Right : DOM.Core.Node)
      is
         use Ada.Strings.Unbounded;

         type Node_Type is
           (Depends,
            Requires);

         subtype Child_Range is Positive range 1 .. 2;

         Ref_Children : constant Muxml.Utils.Tags_Type (Child_Range)
           := (1 => To_Unbounded_String ("requires"),
               2 => To_Unbounded_String ("binary"));

         First_Child_Index : constant array (Node_Type) of Child_Range
           := (Depends  => 1,
               Requires => 2);

         R_Childs : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Right,
              XPath => "*");
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => R_Childs) - 1 loop
            declare
               use type DOM.Core.Node;

               R_Child : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => R_Childs,
                    Index => I);
               Child_Tag : constant String
                 := DOM.Core.Elements.Get_Tag_Name (Elem => R_Child);
               L_Child : DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Doc   => Left,
                    XPath => Child_Tag);
            begin
               if L_Child = null then
                  Muxml.Utils.Add_Child
                    (Parent     => Left,
                     Child_Name => Child_Tag,
                     Ref_Names  => Ref_Children (First_Child_Index
                       (Node_Type'Value (Child_Tag)) .. Ref_Children'Last));
                  L_Child := Muxml.Utils.Get_Element
                    (Doc   => Left,
                     XPath => Child_Tag);

                  if L_Child = null then

                     --  Append child if no reference child was present.

                     Muxml.Utils.Add_Child
                       (Parent     => Left,
                        Child_Name => Child_Tag);
                     L_Child := Muxml.Utils.Get_Element
                       (Doc   => Left,
                        XPath => Child_Tag);
                  end if;
               end if;
               Muxml.Utils.Merge
                 (Left      => L_Child,
                  Right     => R_Child,
                  List_Tags => (1 => To_Unbounded_String ("memory"),
                                2 => To_Unbounded_String ("reader"),
                                3 => To_Unbounded_String ("writer"),
                                4 => To_Unbounded_String ("device"),
                                5 => To_Unbounded_String ("array"),
                                6 => To_Unbounded_String ("libray")));
            end;
         end loop;
      end Merge_Childs;

      ----------------------------------------------------------------------

      procedure Resolve_Depends (Node : DOM.Core.Node)
      is
         use type DOM.Core.Node;

         Name : constant String
           := DOM.Core.Elements.Get_Attribute (Elem => Node,
                                               Name => "name");
         Deps_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element (Doc   => Node,
                                       XPath => "depends");
         Deps : DOM.Core.Node_List;
      begin
         if Deps_Node = null then
            return;
         end if;

         Deps := McKae.XML.XPath.XIA.XPath_Query
             (N     => Deps_Node,
              XPath => "library");

         for I in 0 .. DOM.Core.Nodes.Length (List => Deps) - 1 loop
            declare
               Cur_Dep  : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item (List  => Deps,
                                         Index => I);
               Dep_Name : constant String
                 := DOM.Core.Elements.Get_Attribute (Elem => Cur_Dep,
                                                     Name => "ref");
               Lib_Node : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element (Nodes     => Libraries,
                                             Ref_Attr  => "name",
                                             Ref_Value => Dep_Name);
            begin
               Resolve_Depends (Node => Lib_Node);

               Mulog.Log (Msg => "Adding library '" & Dep_Name
                          & "' resources to component '" & Name & "'");
               Merge_Childs (Left  => Node,
                             Right => Lib_Node);

               declare
                  Dummy : DOM.Core.Node;
               begin
                  Dummy := DOM.Core.Nodes.Remove_Child
                    (N         => Deps_Node,
                     Old_Child => Cur_Dep);
               end;
            end;
         end loop;
      end Resolve_Depends;

      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/components/*[depends/library]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Components) - 1 loop
         declare
            Comp_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Components,
                 Index => I);
         begin
            Resolve_Depends (Node => Comp_Node);
         end;
      end loop;
   end Add_Library_Resources;

   -------------------------------------------------------------------------

   procedure Add_Memory (Data : in out Muxml.XML_Data_Type)
   is
      Components : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/components/component");
      Subjects   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[@name!='tau0']");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Subj_Mem_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "memory");

            Comp_Ref_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "component");
            Comp_Ref : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Comp_Ref_Node,
                 Name => "ref");
            Mappings : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Ref_Node,
                 XPath => "map");
            Comp_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Components,
                 Ref_Attr  => "name",
                 Ref_Value => Comp_Ref);
            Comp_Memory : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Comp_Node,
                 XPath => "requires/memory/memory");
            Log_Mem_Count : constant Natural
              := DOM.Core.Nodes.Length (List => Comp_Memory);
         begin
            if Log_Mem_Count > 0 then
               Mulog.Log (Msg => "Expanding" & Log_Mem_Count'Img & " logical "
                          & "memory region(s) of component '" & Comp_Ref
                          & "' to subject '" & Subj_Name & "'");

               for J in 0 .. Log_Mem_Count - 1 loop
                  declare
                     Log_Mem : constant DOM.Core.Node
                       := DOM.Core.Nodes.Clone_Node
                         (N    => DOM.Core.Nodes.Item
                            (List  => Comp_Memory,
                             Index => J),
                          Deep => False);
                     Log_Mem_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Log_Mem,
                          Name => "logical");
                     Phys_Mem_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => Muxml.Utils.Get_Element
                            (Nodes     => Mappings,
                             Ref_Attr  => "logical",
                             Ref_Value => Log_Mem_Name),
                          Name => "physical");
                  begin
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => Log_Mem,
                        Name  => "physical",
                        Value => Phys_Mem_Name);
                     DOM.Core.Elements.Remove_Attribute
                       (Elem => Log_Mem,
                        Name => "size");
                     Muxml.Utils.Append_Child
                       (Node      => Subj_Mem_Node,
                        New_Child => Log_Mem);
                  end;
               end loop;
            end if;
         end;
      end loop;
   end Add_Memory;

   -------------------------------------------------------------------------

   procedure Add_Memory_Arrays (Data : in out Muxml.XML_Data_Type)
   is
      Memarrays : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/components/*/requires/memory/array");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Memarrays) - 1 loop
         declare
            Arr_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Memarrays,
                 Index => I);
            Arr_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Arr_Node,
                 Name => "logical");
            Parent_Mem : constant DOM.Core.Node
              := DOM.Core.Nodes.Parent_Node (N => Arr_Node);
            Comp_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Muxml.Utils.Ancestor_Node
                   (Node  => Arr_Node,
                    Level => 2),
                 Name => "name");
            Address : Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Arr_Node,
                    Name => "virtualAddressBase"));
            Element_Size : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Arr_Node,
                    Name => "elementSize"));
            Executable : constant Boolean
              := Boolean'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Arr_Node,
                    Name => "executable"));
            Writable : constant Boolean
              := Boolean'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Arr_Node,
                    Name => "writable"));
            Children : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Arr_Node,
                 XPath => "*");
            Child_Count : constant Positive
              := DOM.Core.Nodes.Length (List => Children);
         begin
            Mulog.Log (Msg => "Adding" & Child_Count'Img & " memory region(s) "
                       & "of array '" & Arr_Name & "' to component '"
                       & Comp_Name & "'");
            for J in 0 .. Child_Count - 1 loop
               declare
                  use type Interfaces.Unsigned_64;

                  package XML renames Mutools.XML_Utils;

                  Mem_Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Children,
                       Index => J);
                  Mem_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Mem_Node,
                       Name => "logical");
               begin
                  Muxml.Utils.Append_Child
                    (Node      => Parent_Mem,
                     New_Child => XML.Create_Component_Memory_Node
                       (Policy       => Data,
                        Logical_Name => Mem_Name,
                        Address      => Mutools.Utils.To_Hex
                          (Number => Address),
                        Size         => Mutools.Utils.To_Hex
                          (Number => Element_Size),
                        Executable   => Executable,
                        Writable     => Writable));
                  Address := Address + Element_Size;
               end;
            end loop;
         end;
      end loop;
   end Add_Memory_Arrays;

   -------------------------------------------------------------------------

   procedure Remove_Component_Reference (Data : in out Muxml.XML_Data_Type)
   is
      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[component]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Subjects,
                                      Index => I);
         begin
            Muxml.Utils.Remove_Child
              (Node       => Subj_Node,
               Child_Name => "component");
         end;
      end loop;
   end Remove_Component_Reference;

   -------------------------------------------------------------------------

   procedure Remove_Components (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Muxml.Utils.Remove_Child
        (Node       => DOM.Core.Documents.Get_Element (Doc => Data.Doc),
         Child_Name => "components");
   end Remove_Components;

end Expanders.Components;
