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

with DOM.Core.Attrs;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mutools.System_Config;

package body Mutools.Substitutions
is

   -------------------------------------------------------------------------

   procedure Process_Attributes (Data : Muxml.XML_Data_Type)
   is
      Attrs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "//@*[starts-with(.,'$')]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Attrs) - 1 loop
         declare
            Attr_Node  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Attrs,
                 Index => I);
            Attr_Value : constant String
              := DOM.Core.Attrs.Value (Att => Attr_Node);
            Var_Name   : constant String
              := Attr_Value (Attr_Value'First + 1 .. Attr_Value'Last);
         begin
            if System_Config.Has_Value (Data => Data,
                                        Name => Var_Name)
            then
               DOM.Core.Attrs.Set_Value
                 (Att   => Attr_Node,
                  Value => System_Config.Get_Raw_Value
                    (Data => Data,
                     Name => Var_Name));
            end if;
         end;
      end loop;
   end Process_Attributes;

end Mutools.Substitutions;
