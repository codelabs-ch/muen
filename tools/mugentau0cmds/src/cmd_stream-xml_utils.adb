--
--  Copyright (C) 2019  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2019  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Cmd_Stream.XML_Utils
is

   -------------------------------------------------------------------------

   procedure Create_Stream_Boilerplate (Stream_Doc : out Muxml.XML_Data_Type)
   is
      Dom_Impl : DOM.Core.DOM_Implementation;
      Dummy    : DOM.Core.Node;
   begin
      Stream_Doc.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);

      Dummy := DOM.Core.Nodes.Append_Child
        (N         => Stream_Doc.Doc,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Stream_Doc.Doc,
            Tag_Name => "tau0"));
      Dummy := DOM.Core.Nodes.Append_Child
        (N         => Dummy,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Stream_Doc.Doc,
            Tag_Name => "commands"));
   end Create_Stream_Boilerplate;

end Cmd_Stream.XML_Utils;
