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

with Muxml.Utils;

package body Expanders.Device_Domains
is

   -------------------------------------------------------------------------

   procedure Add_Section_Skeleton (Data : in out Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      DD_Node     : DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/deviceDomains");
      Events_Node : DOM.Core.Node;
   begin
      if DD_Node = null then
         Events_Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/events");

         DD_Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "deviceDomains");
         DD_Node := DOM.Core.Nodes.Insert_Before
           (N         => DOM.Core.Documents.Get_Element (Doc => Data.Doc),
            New_Child => DD_Node,
            Ref_Child => Events_Node);
         pragma Unreferenced (DD_Node);
      end if;
   end Add_Section_Skeleton;

end Expanders.Device_Domains;
