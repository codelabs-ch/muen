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

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
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
                 XPath => "channels/*");
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
                 XPath => "devices/device");
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
                        begin
                           DOM.Core.Elements.Set_Attribute
                             (Elem  => Log_Res,
                              Name  => "physical",
                              Value => Phys_Res_Name);

                           if DOM.Core.Nodes.Node_Name
                             (N => Log_Res) = "memory"
                           then
                              DOM.Core.Elements.Remove_Attribute
                                (Elem => Log_Res,
                                 Name => "size");
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
                 XPath => "memory/memory");
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
