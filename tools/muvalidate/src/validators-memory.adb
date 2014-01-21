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
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;

package body Validators.Memory
is

   use McKae.XML.XPath.XIA;

   -------------------------------------------------------------------------

   procedure Physical_Memory_References (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "//physical");
   begin
      Mulog.Log (Msg => "Checking physical memory references");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Node         : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Logical_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
                 Name => "logical");
            Phys_Name    : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Node,
                 Name => "name");
            Physical     : constant DOM.Core.Node_List
              := XPath_Query
                (N     => XML_Data.Doc,
                 XPath => "/system/memory/memory[@name='" & Phys_Name & "']");
         begin
            if DOM.Core.Nodes.Length (List => Physical) = 0 then
               raise Validation_Error with "Physical memory '" & Phys_Name
                 & "' referenced by logical memory '" & Logical_Name
                 & "' not found";
            end if;
         end;
      end loop;
   end Physical_Memory_References;

end Validators.Memory;
