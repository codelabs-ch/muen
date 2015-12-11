--
--  Copyright (C) 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Muxml.Utils;

package body Expanders.Platform
is

   -------------------------------------------------------------------------

   procedure Add_Section_Skeleton (Data : in out Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      Platform_Node : DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/platform");
      Mappings_Node, Aliases_Node : DOM.Core.Node;
   begin
      if Platform_Node = null then
         Platform_Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "platform");
         Muxml.Utils.Insert_Before
           (Parent    => DOM.Core.Documents.Get_Element (Doc => Data.Doc),
            New_Child => Platform_Node,
            Ref_Child => "memory");
      end if;

      Mappings_Node := Muxml.Utils.Get_Element
        (Doc   => Platform_Node,
         XPath => "mappings");
      if Mappings_Node = null then
         Mappings_Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "mappings");
         Muxml.Utils.Append_Child
           (Node      => Platform_Node,
            New_Child => Mappings_Node);
      end if;

      Aliases_Node := Muxml.Utils.Get_Element
        (Doc   => Mappings_Node,
         XPath => "aliases");
      if Aliases_Node = null then
         Aliases_Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "aliases");
         Muxml.Utils.Append_Child
           (Node      => Mappings_Node,
            New_Child => Aliases_Node);
      end if;
   end Add_Section_Skeleton;

   -------------------------------------------------------------------------

   procedure Resolve_Device_Aliases (Data : in out Muxml.XML_Data_Type)
   is
      Subj_Devs   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/devices/device");
      Dev_Aliases : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/platform/mappings/aliases/alias");

      --  Resolve names of device resources of the specified subject device
      --  using the given alias node.
      procedure Resolve_Device_Resource_Names
        (Alias          : DOM.Core.Node;
         Subject_Device : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Resolve_Device_Resource_Names
        (Alias          : DOM.Core.Node;
         Subject_Device : DOM.Core.Node)
      is
         Alias_Resources : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Alias,
              XPath => "resource");
         Dev_Resources : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject_Device,
              XPath => "memory|irq|ioPort");
      begin
         for I in 1 .. DOM.Core.Nodes.Length (List => Dev_Resources) loop
            declare
               Dev_Res : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item (List  => Dev_Resources,
                                         Index => I - 1);
               Alias_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Dev_Res,
                    Name => "physical");
               Phys_Name : constant String := Muxml.Utils.Get_Attribute
                 (Nodes     => Alias_Resources,
                  Ref_Attr  => "name",
                  Ref_Value => Alias_Name,
                  Attr_Name => "physical");
            begin
               DOM.Core.Elements.Set_Attribute
                 (Elem  => Dev_Res,
                  Name  => "physical",
                  Value => Phys_Name);
            end;
         end loop;
      end Resolve_Device_Resource_Names;
   begin
      for I in 1 .. DOM.Core.Nodes.Length (List => Dev_Aliases) loop
         declare
            use type DOM.Core.Node;

            Alias : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Dev_Aliases,
               Index => I - 1);
            Alias_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Alias,
               Name => "name");
            Phys_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Alias,
               Name => "physical");
            Subj_Dev : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Nodes     => Subj_Devs,
               Ref_Attr  => "physical",
               Ref_Value => Alias_Name);
         begin
            if Subj_Dev /= null then
               DOM.Core.Elements.Set_Attribute
                 (Elem  => Subj_Dev,
                  Name  => "physical",
                  Value => Phys_Name);
               Resolve_Device_Resource_Names
                 (Alias          => Alias,
                  Subject_Device => Subj_Dev);
            end if;
         end;
      end loop;
   end Resolve_Device_Aliases;

end Expanders.Platform;
