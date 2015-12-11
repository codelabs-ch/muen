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

end Expanders.Platform;
