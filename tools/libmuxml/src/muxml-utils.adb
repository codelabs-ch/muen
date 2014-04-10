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

with DOM.Core.Append_Node;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Attrs;

with McKae.XML.XPath.XIA;

package body Muxml.Utils
is

   -------------------------------------------------------------------------

   procedure Append
     (Left  : in out DOM.Core.Node_List;
      Right :        DOM.Core.Node_List)
   is

   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Right) - 1 loop
         DOM.Core.Append_Node
           (List => Left,
            N    => DOM.Core.Nodes.Item
              (List  => Right,
               Index => I));
      end loop;
   end Append;

   -------------------------------------------------------------------------

   procedure Append_Child
     (Node      : DOM.Core.Node;
      New_Child : DOM.Core.Node)
   is
      Dummy : DOM.Core.Node;
      pragma Unreferenced (Dummy);
   begin
      Dummy := DOM.Core.Nodes.Append_Child
        (N         => Node,
         New_Child => New_Child);
   end Append_Child;

   -------------------------------------------------------------------------

   function Get_Attribute
     (Doc   : DOM.Core.Node;
      XPath : String;
      Name  : String)
      return String
   is
      Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
        (List  => McKae.XML.XPath.XIA.XPath_Query
           (N     => Doc,
            XPath => XPath),
         Index => 0);
   begin
      return DOM.Core.Elements.Get_Attribute
        (Elem => Node,
         Name => Name);
   end Get_Attribute;

   -------------------------------------------------------------------------

   function Get_Element_Value
     (Doc   : DOM.Core.Node;
      XPath : String)
      return String
   is
      Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
        (List  => McKae.XML.XPath.XIA.XPath_Query
           (N     => Doc,
            XPath => XPath & "/text()"),
         Index => 0);
   begin
      return DOM.Core.Nodes.Node_Value (N => Node);
   end Get_Element_Value;

   -------------------------------------------------------------------------

   procedure Merge
     (Left     : DOM.Core.Node;
      Right    : DOM.Core.Node;
      List_Tag : String := "")
   is
   begin
      if DOM.Core.Nodes.Node_Name (N => Left)
        /= DOM.Core.Nodes.Node_Name (N => Right)
      then
         return;
      end if;
      declare
         use type DOM.Core.Node;

         R_Child : DOM.Core.Node := DOM.Core.Nodes.First_Child (N => Right);
      begin
         while R_Child /= null loop
            declare
               L_Child : DOM.Core.Node := DOM.Core.Nodes.First_Child
                 (N => Left);
            begin

               --  Find matching children.

               while L_Child /= null and then
                 DOM.Core.Nodes.Node_Name (N => L_Child)
                 /= DOM.Core.Nodes.Node_Name (N => R_Child)
               loop
                  L_Child := DOM.Core.Nodes.Next_Sibling (N => L_Child);
               end loop;

               if L_Child = null
                 or else DOM.Core.Nodes.Node_Name (N => L_Child) = List_Tag
               then

                  --  No match or list found, attach right child incl. all
                  --  children to left.

                  Append_Child
                    (Node      => Left,
                     New_Child => DOM.Core.Nodes.Clone_Node
                       (N    => R_Child,
                        Deep => True));
               else
                  Merge (Left     => L_Child,
                         Right    => R_Child,
                         List_Tag => List_Tag);
               end if;
            end;

            R_Child := DOM.Core.Nodes.Next_Sibling (N => R_Child);
         end loop;
      end;

      DOM.Core.Nodes.Set_Node_Value
        (N     => Left,
         Value => DOM.Core.Nodes.Node_Value (N => Right));

      declare
         Attrs : constant DOM.Core.Named_Node_Map
           := DOM.Core.Nodes.Attributes (N => Right);
         Node  : DOM.Core.Node;
      begin
         for I in 0 .. DOM.Core.Nodes.Length (Map => Attrs) - 1 loop
            Node := DOM.Core.Nodes.Item (Map   => Attrs,
                                         Index => I);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Left,
               Name  => DOM.Core.Attrs.Name (Att => Node),
               Value => DOM.Core.Attrs.Value (Att => Node));
         end loop;
      end;
   end Merge;

end Muxml.Utils;
