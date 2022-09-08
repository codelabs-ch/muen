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
with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mutools.Expressions;
with Mutools.Xmldebuglog;
with Mulog;

package body Mutools.Substitutions
is

   -------------------------------------------------------------------------

   procedure Process_Attributes (Data         : Muxml.XML_Data_Type;
                                 Debug_Active : Boolean := False)
   is
      Config_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/*/config/*");
      Attribute_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "//@*[starts-with(.,'$')]");
      Text_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "//text()[starts-with(.,'$')]");
      Node_Access : Mutools.Expressions.Name_To_String_Hashed_Map.Map;

      ----------------------------------------------------------------------

      generic
         with function Getter (Node : DOM.Core.Node) return String;
         with procedure Setter (Node : DOM.Core.Node; Value : String);
         with function Owner (Node : DOM.Core.Node) return DOM.Core.Node;
      procedure Substitution_Loop (Node_List : DOM.Core.Node_List);

      ----------------------------------------------------------------------

      procedure Populate_Node_Access
         (Config_Nodes :     DOM.Core.Node_List;
          Node_Access   : out Mutools.Expressions.Name_To_String_Hashed_Map.Map);

      ----------------------------------------------------------------------

      procedure Populate_Node_Access
         (Config_Nodes :     DOM.Core.Node_List;
          Node_Access   : out Mutools.Expressions.Name_To_String_Hashed_Map.Map)
      is
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Config_Nodes) - 1 loop
            declare
               Node : constant DOM.Core.Node
                  := DOM.Core.Nodes.Item (List  => Config_Nodes, Index => I);
               Node_Name : constant String
                  := DOM.Core.Elements.Get_Attribute
                  (Elem => Node,
                   Name => "name");
               Node_Value : constant String
                  := DOM.Core.Elements.Get_Attribute
                  (Elem => Node,
                   Name => "value");
            begin
               Node_Access.Insert
                  (Key      => Node_Name,
                   New_Item => Node_Value);
            end;
         end loop;
      end Populate_Node_Access;

      ----------------------------------------------------------------------

      procedure Substitution_Loop (Node_List : DOM.Core.Node_List)
      is
         -- TODO: remove these once V811-012 is resolved
         use all type DOM.Core.Node;
         use all type DOM.Core.Node_Types;

         ------------------------------------------------------------------

         -- workaround for V811-012
         -- replace this function by "Owner (Node)" once the bug is resolved
         function Get_Parent (Node : DOM.Core.Node) return DOM.Core.Node;

         ------------------------------------------------------------------

         function Get_Parent (Node : DOM.Core.Node) return DOM.Core.Node
         is
         begin
            if DOM.Core.Nodes.Node_Type (Node) = DOM.Core.Attribute_Node then
               declare
                  Possible_Parents_List : constant DOM.Core.Node_List
                     := McKae.XML.XPath.XIA.XPath_Query
                     (N     => Data.Doc,
                      XPath => "//*[@"
                         & DOM.Core.Attrs.Name (Node)
                         & "='"
                         & Getter (Node => Node)
                         & "']");
                  Parent                : DOM.Core.Node;
                  Attr_List             : DOM.Core.Named_Node_Map;
                  Possibly_Equal_Attr   : DOM.Core.Node;
               begin
                  for I in 0 .. DOM.Core.Nodes.Length (List => Possible_Parents_List) - 1 loop
                     Parent := DOM.Core.Nodes.Item
                        (List  => Possible_Parents_List,
                         Index => I);
                     Attr_List := DOM.Core.Nodes.Attributes (Parent);
                     for J in 0 ..   DOM.Core.Nodes.Length (Map => Attr_List) - 1 loop
                        Possibly_Equal_Attr := DOM.Core.Nodes.Item (Map => Attr_List, Index => J);
                        if Possibly_Equal_Attr = Node then
                           return Parent;
                        end if;
                     end loop;
                  end loop;

                  -- nothing found
                  return null;
               end;
            else
               return Owner (Node);
            end if;
         end Get_Parent;

      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Node_List) - 1 loop
            declare
               Node  : constant DOM.Core.Node
                  := DOM.Core.Nodes.Item
                  (List  => Node_List,
                   Index => I);
               Node_Value : constant String
                  := Getter (Node => Node);
               Reference  : constant String
                  := Node_Value (Node_Value'First + 1 .. Node_Value'Last);
            begin
               if Node_Access.Contains (Reference) then
                  if DOM.Core.Nodes.Node_Type (Node) = DOM.Core.Attribute_Node
                     and then DOM.Core.Attrs.Name (Node) = "variable"
                  then
                     if Debug_Active then
                        Mulog.Log (Msg => Mutools.Xmldebuglog.Get_Log_For_Error_Message
                                      (Node => Get_Parent (Node)));
                     end if;

                     raise Muxml.Validation_Error with
                        "Error when substituting $-reference."
                        & " Found $-reference in 'variable'-attribute."
                        & " Value is '"
                        & Node_Value
                        & "'.";
                  else
                     Setter
                        (Node  => Node,
                         Value => Node_Access (Reference));
                  end if;
               else
                  if Debug_Active then
                     Mulog.Log (Msg => Mutools.Xmldebuglog.Get_Log_For_Error_Message
                                   (Node => Get_Parent (Node)));
                  end if;

                  raise Muxml.Validation_Error with
                     "Found attribute value or text-node with value '"
                     & Node_Value
                     & "' but no variable with name '"
                     & Reference
                     & "' exists";
               end if;
            end;
         end loop;
      end Substitution_Loop;

      ----------------------------------------------------------------------

      procedure Substitute_Attributes is new Substitution_Loop
         (Getter => DOM.Core.Attrs.Value,
          Setter => DOM.Core.Attrs.Set_Value,
          Owner  => DOM.Core.Attrs.Owner_Element);

      procedure Substitute_Text is new Substitution_Loop
         (Getter => DOM.Core.Nodes.Node_Value,
          Setter => DOM.Core.Nodes.Set_Node_Value,
          Owner  => DOM.Core.Nodes.Parent_Node);

   begin
      Populate_Node_Access
         (Config_Nodes => Config_Nodes,
          Node_Access   => Node_Access);

      Substitute_Text (Node_List => Text_Nodes);
      Substitute_Attributes (Node_List => Attribute_Nodes);

   end Process_Attributes;

end Mutools.Substitutions;
