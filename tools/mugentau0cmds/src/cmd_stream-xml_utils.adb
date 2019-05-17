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

with DOM.Core.Append_Node;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;

with Muxml.Utils;

package body Cmd_Stream.XML_Utils
is

   --  Create command node with specified name and attributes.
   function Create_Command
     (Stream_Doc : Muxml.XML_Data_Type;
      Name       : String;
      Attrs      : Attribute_Array := Null_Attrs)
      return DOM.Core.Node;

   -------------------------------------------------------------------------

   procedure Append_Command
     (Stream_Doc : Muxml.XML_Data_Type;
      Name       : String;
      Attrs      : Attribute_Array := Null_Attrs)
   is
   begin
      Muxml.Utils.Append_Child
        (Node      => Muxml.Utils.Get_Element
           (Doc   => Stream_Doc.Doc,
            XPath => "/tau0/commands"),
         New_Child => Create_Command
           (Stream_Doc => Stream_Doc,
            Name       => Name,
            Attrs      => Attrs));
   end Append_Command;

   -------------------------------------------------------------------------

   procedure Append_Command
     (Buffer     : in out Command_Buffer_Type;
      Stream_Doc :        Muxml.XML_Data_Type;
      Name       :        String;
      Attrs      :        Attribute_Array := Null_Attrs)
   is
   begin
      DOM.Core.Append_Node
        (List => DOM.Core.Node_List (Buffer),
         N    => Create_Command
           (Stream_Doc => Stream_Doc,
            Name       => Name,
            Attrs      => Attrs));
   end Append_Command;

   -------------------------------------------------------------------------

   procedure Append_Commands
     (Stream_Doc : Muxml.XML_Data_Type;
      Buffer     : Command_Buffer_Type)
   is
      Cmds_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element (Doc   => Stream_Doc.Doc,
                                    XPath => "/tau0/commands");
   begin
      for I in 0 .. DOM.Core.Nodes.Length
        (List => DOM.Core.Node_List (Buffer)) - 1
      loop
         Muxml.Utils.Append_Child
           (Node      => Cmds_Node,
            New_Child => DOM.Core.Nodes.Item
              (List  => DOM.Core.Node_List (Buffer),
               Index => I));
      end loop;
   end Append_Commands;

   -------------------------------------------------------------------------

   function Create_Command
     (Stream_Doc : Muxml.XML_Data_Type;
      Name       : String;
      Attrs      : Attribute_Array := Null_Attrs)
      return DOM.Core.Node
   is
   begin
      return Node : DOM.Core.Node do
         Node := DOM.Core.Documents.Create_Element
           (Doc      => Stream_Doc.Doc,
            Tag_Name => Name);

         for A of Attrs loop
            DOM.Core.Elements.Set_Attribute
              (Elem  => Node,
               Name  => S (A.Attr),
               Value => S (A.Value));
         end loop;
      end return;
   end Create_Command;

   -------------------------------------------------------------------------

   procedure Create_Stream_Boilerplate (Stream_Doc : out Muxml.XML_Data_Type)
   is
      Dom_Impl : DOM.Core.DOM_Implementation;
      Node     : DOM.Core.Node;
   begin
      Stream_Doc.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);

      Node := DOM.Core.Nodes.Append_Child
        (N         => Stream_Doc.Doc,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Stream_Doc.Doc,
            Tag_Name => "tau0"));
      Muxml.Utils.Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Stream_Doc.Doc,
            Tag_Name => "commands"));
   end Create_Stream_Boilerplate;

end Cmd_Stream.XML_Utils;
