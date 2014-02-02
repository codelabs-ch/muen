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

with Muxml;

package body Pack.Parser
is

   -------------------------------------------------------------------------

   function Parse (Policy : String) return File_Array
   is
      Data       : Muxml.XML_Data_Type;
      File_Nodes : DOM.Core.Node_List;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => Policy);

      File_Nodes := McKae.XML.XPath.XIA.XPath_Query
        (N     => Data.Doc,
         XPath => "/system/memory/memory/file");

      declare
         Files : File_Array (1 .. DOM.Core.Nodes.Length (List => File_Nodes));
      begin
         for I in Files'Range loop
            declare
               File   : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => File_Nodes,
                    Index => I - 1);
               Memory : constant DOM.Core.Node
                 := DOM.Core.Nodes.Parent_Node (N => File);
            begin
               Files (I).Path   := U
                 (DOM.Core.Elements.Get_Attribute
                    (Elem => File,
                     Name => "filename"));
               Files (I).Format := File_Format_Type'Value
                 (DOM.Core.Elements.Get_Attribute
                    (Elem => File,
                     Name => "format"));
               Files (I).Offset := Interfaces.Unsigned_64'Value
                 (DOM.Core.Elements.Get_Attribute
                    (Elem => File,
                     Name => "offset"));

               Files (I).Name    := U
                 (DOM.Core.Elements.Get_Attribute
                    (Elem => Memory,
                     Name => "name"));
               Files (I).Address := Interfaces.Unsigned_64'Value
                 (DOM.Core.Elements.Get_Attribute
                    (Elem => Memory,
                     Name => "physicalAddress"));
               Files (I).Size    := Interfaces.Unsigned_64'Value
                 (DOM.Core.Elements.Get_Attribute
                    (Elem => Memory,
                     Name => "size"));
            end;
         end loop;

         return Files;
      end;
   end Parse;

end Pack.Parser;
