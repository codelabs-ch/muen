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
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Cmd_Stream.XML_Utils;

package body Cmd_Stream.Roots.Kernels
is

   -------------------------------------------------------------------------

   procedure Create_Per_CPU_Kernel
     (Policy     : in out Muxml.XML_Data_Type;
      Stream_Doc : in out Muxml.XML_Data_Type)
   is
      Kernels : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/kernel/memory/cpu");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Kernels) - 1 loop
         declare
            Kernel : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Kernels,
                 Index => I);
            CPU : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Kernel,
                 Name => "id");
            Krnl_Attr : constant XML_Utils.Attribute_Type
              := (Attr  => U ("kernel"),
                  Value => U (Trim (I'Img)));
         begin
            XML_Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "createKernel",
               Attrs      => (Krnl_Attr,
                              (Attr  => U ("cpu"),
                               Value => U (CPU))));
         end;
      end loop;
   end Create_Per_CPU_Kernel;

end Cmd_Stream.Roots.Kernels;
