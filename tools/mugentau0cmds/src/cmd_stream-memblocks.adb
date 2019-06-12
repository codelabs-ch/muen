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

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mutools.Constants;

package body Cmd_Stream.Memblocks
is

   -------------------------------------------------------------------------

   procedure Create_Memory_Blocks
     (Policy     : in out Muxml.XML_Data_Type;
      Stream_Doc : in out Utils.Stream_Document_Type)
   is
      Blocks : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/hardware/memory/memoryBlock");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Blocks) - 1 loop
         declare
            use type Interfaces.Unsigned_64;

            Block : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Blocks,
                 Index => I);
            Pages : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Block,
                    Name => "size")) / Mutools.Constants.Page_Size;
         begin
            Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "addMemoryBlock",
               Attrs      => ((Attr  => U ("address"),
                               Value => U (DOM.Core.Elements.Get_Attribute
                                 (Elem => Block,
                                  Name => "physicalAddress"))),
                              (Attr  => U ("size"),
                               Value => U (Trim (Pages'Img)))));
         end;
      end loop;
   end Create_Memory_Blocks;

end Cmd_Stream.Memblocks;
