--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Streams;

with GNAT.SHA256;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;

with Memhashes.Utils;

package body Memhashes
is

   --  Generate hashes for memory content in given policy.
   procedure Generate_Hashes
     (Policy    : in out Muxml.XML_Data_Type;
      Input_Dir :        String);

   -------------------------------------------------------------------------

   procedure Generate_Hashes
     (Policy    : in out Muxml.XML_Data_Type;
      Input_Dir :        String)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "//memory/*[self::fill or self::file]/..");
      Count : constant Natural := DOM.Core.Nodes.Length (List => Nodes);
   begin
      Mulog.Log (Msg => "Looking for input files in '" & Input_Dir & "'");
      Mulog.Log (Msg => "Generating hashes for" & Count'Img
                 & " memory regions");

      for I in 0 .. Count - 1 loop
         declare
            Mem_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Buffer : constant Ada.Streams.Stream_Element_Array
              := Utils.To_Stream
                (Node      => Mem_Node,
                 Input_Dir => Input_Dir);
            Hash_Node : constant DOM.Core.Node
              := DOM.Core.Documents.Create_Element
                (Doc      => Policy.Doc,
                 Tag_Name => "hash");
         begin
            DOM.Core.Elements.Set_Attribute
              (Elem  => Hash_Node,
               Name  => "value",
               Value => "16#" & GNAT.SHA256.Digest (A => Buffer) & "#");
            Muxml.Utils.Append_Child (Node      => Mem_Node,
                                      New_Child => Hash_Node);
         end;
      end loop;
   end Generate_Hashes;

   -------------------------------------------------------------------------

   procedure Run (Policy_In, Policy_Out, Input_Dir : String)
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Mulog.Log (Msg => "Processing policy '" & Policy_In & "'");
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => Policy_In);

      Generate_Hashes (Policy    => Policy,
                       Input_Dir => Input_Dir);

      Mulog.Log (Msg => "Writing policy to '" & Policy_Out & "'");
      Muxml.Write (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => Policy_Out);
   end Run;

end Memhashes;
