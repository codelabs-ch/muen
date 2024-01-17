--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with DOM.Core.Documents.Local;

with Muxml.Utils;

package body Compjoin.Utils
is

   -------------------------------------------------------------------------

   procedure Add_Component
     (Policy         : Muxml.XML_Data_Type;
      Component_File : String)
   is
      Section_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element (Doc   => Policy.Doc,
                                    XPath => "/system/components");

      Component      : Muxml.XML_Data_Type;
      Component_Node : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Component,
                   Kind => Muxml.Component_Ext,
                   File => Component_File);
      Component_Node := DOM.Core.Documents.Local.Adopt_Node
        (Doc    => Policy.Doc,
         Source => DOM.Core.Documents.Local.Clone_Node
           (N    => DOM.Core.Documents.Get_Element (Doc => Component.Doc),
            Deep => True));

      Muxml.Utils.Insert_Child
        (Parent    => Section_Node,
         New_Child => Component_Node);
   end Add_Component;

end Compjoin.Utils;
