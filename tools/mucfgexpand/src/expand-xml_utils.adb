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

with Mutools.Utils;

package body Expand.XML_Utils
is

   -------------------------------------------------------------------------

   procedure Add_Memory_Region
     (Policy      : in out Muxml.XML_Data_Type;
      Name        :        String;
      Address     :        Interfaces.Unsigned_64;
      Size        :        Interfaces.Unsigned_64;
      Caching     :        String;
      File_Name   :        String;
      File_Format :        String;
      File_Offset :        Interfaces.Unsigned_64)
   is
      Section   : constant DOM.Core.Node := DOM.Core.Nodes.Item
        (List  => McKae.XML.XPath.XIA.XPath_Query
           (N     => Policy.Doc,
            XPath => "/system/memory"),
         Index => 0);
      Mem_Node  : DOM.Core.Node := DOM.Core.Documents.Create_Element
        (Doc      => Policy.Doc,
         Tag_Name => "memory");
      File_Node : DOM.Core.Node := DOM.Core.Documents.Create_Element
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
        (Elem  => Mem_Node,
         Name  => "name",
         Value => Name);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "physicalAddress",
         Value => Mutools.Utils.To_Hex (Number => Address));
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "size",
         Value => Mutools.Utils.To_Hex (Number => Size));
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "caching",
         Value => Caching);

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
         Value => Mutools.Utils.To_Hex (Number => File_Offset));
   end Add_Memory_Region;

end Expand.XML_Utils;
