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
with DOM.Core.Documents;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

package body Expand.XML_Utils
is

   --  Create memory node element with given parameters.
   function Create_Memory_Node
     (Policy  : in out Muxml.XML_Data_Type;
      Name    :        String;
      Address :        String;
      Size    :        String;
      Caching :        String)
      return DOM.Core.Node;

   -------------------------------------------------------------------------

   procedure Add_Memory_Region
     (Policy  : in out Muxml.XML_Data_Type;
      Name    :        String;
      Address :        String;
      Size    :        String;
      Caching :        String)
   is
      Section    : constant DOM.Core.Node
        := DOM.Core.Nodes.Item
          (List  => McKae.XML.XPath.XIA.XPath_Query
               (N     => Policy.Doc,
                XPath => "/system/memory"),
           Index => 0);
      Dummy_Node : DOM.Core.Node;
      pragma Unreferenced (Dummy_Node);
   begin
      Dummy_Node := DOM.Core.Nodes.Append_Child
        (N         => Section,
         New_Child => Create_Memory_Node
           (Policy  => Policy,
            Name    => Name,
            Address => Address,
            Size    => Size,
            Caching => Caching));
   end Add_Memory_Region;

   -------------------------------------------------------------------------

   procedure Add_Memory_Region
     (Policy      : in out Muxml.XML_Data_Type;
      Name        :        String;
      Address     :        String;
      Size        :        String;
      Caching     :        String;
      File_Name   :        String;
      File_Format :        String;
      File_Offset :        String)
   is
      Section   : constant DOM.Core.Node
        := DOM.Core.Nodes.Item
          (List  => McKae.XML.XPath.XIA.XPath_Query
               (N     => Policy.Doc,
                XPath => "/system/memory"),
           Index => 0);
      Mem_Node  : DOM.Core.Node
        := Create_Memory_Node
          (Policy  => Policy,
           Name    => Name,
           Address => Address,
           Size    => Size,
           Caching => Caching);
      File_Node : DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Policy.Doc,
           Tag_Name => "file");
   begin
      Mem_Node := DOM.Core.Nodes.Append_Child
        (N         => Section,
         New_Child => Mem_Node);
      File_Node := DOM.Core.Nodes.Append_Child
        (N         => Mem_Node,
         New_Child => File_Node);

      DOM.Core.Elements.Set_Attribute
        (Elem  => File_Node,
         Name  => "format",
         Value => File_Format);
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

   function Create_Memory_Node
     (Policy  : in out Muxml.XML_Data_Type;
      Name    :        String;
      Address :        String;
      Size    :        String;
      Caching :        String)
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

      if Address'Length > 0 then
         DOM.Core.Elements.Set_Attribute
           (Elem  => Mem_Node,
            Name  => "physicalAddress",
            Value => Address);
      end if;

      return Mem_Node;
   end Create_Memory_Node;

end Expand.XML_Utils;
