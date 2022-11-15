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

with Schema.Dom_Readers;
with Sax.Utils;
with Sax.Readers;
with Sax.Symbols;

package Schema.Dom_Readers_With_Location
is
   --  This tree reader adds information about the location (within the parsed
   --  file) of each element-node. The returned nodes have an additional
   --  attribute "originOfNode" containing the location information.
   type Tree_Reader_With_Location is new Schema.Dom_Readers.Tree_Reader with null record;
   overriding procedure Start_Element
      (Handler    : in out Tree_Reader_With_Location;
       NS         : Sax.Utils.XML_NS;
       Local_Name : Sax.Symbols.Symbol;
       Atts       : Sax.Readers.Sax_Attribute_List);

end Schema.Dom_Readers_With_Location;
