--
--  Copyright (C) 2022 secunet Security Networks AG
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

with DOM.Core;
with DOM.Core.Attrs;
with DOM.Core.Documents;
with DOM.Core.Elements;
with Sax.Locators;

package body Schema.Dom_Readers_With_Location is

   overriding procedure Start_Element
      (Handler    : in out Tree_Reader_With_Location;
       NS         : Sax.Utils.XML_NS;
       Local_Name : Sax.Symbols.Symbol;
       Atts       : Sax.Readers.Sax_Attribute_List)
   is
      Att_Node, Unused : DOM.Core.Attr;
   begin

      --  Execute the original Start_Element.
      Schema.Dom_Readers.Start_Element (Schema.Dom_Readers.Tree_Reader (Handler),
                                        NS, Local_Name, Atts);

      --  Then add a new attribute to the created node.
      Att_Node := DOM.Core.Documents.Create_Attribute
         (Doc => Schema.Dom_Readers_With_Location.Get_Tree (Handler),
          Name => "originOfNode");
      DOM.Core.Attrs.Set_Value (Att_Node, Sax.Locators.To_String
                                   (Sax.Readers.Current_Location
                                       (Sax.Readers.Sax_Reader
                                           (Handler)), Use_Basename => True));
      Unused := DOM.Core.Elements.Set_Attribute_Node (Handler.Current_Node, Att_Node);
   end Start_Element;

end Schema.Dom_Readers_With_Location;
