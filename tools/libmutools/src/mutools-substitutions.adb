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

---mmmDEBUG
--use all type DOM.Core.Node;

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

      procedure Substitution_Loop (Node_List : DOM.Core.Node_List) is
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
                  Setter
                     (Node  => Node,
                      Value => Node_Access (Reference));
               else
                  ---mmmDEBUG BEGIN
                  --   DOM.Core.Nodes.Print (Node);
                  --   Mulog.Log (Msg => DOM.Core.Nodes.Node_Type (Node)'Img);
                  --   Mulog.Log (Msg => Boolean'Image
                  --                 (DOM.Core.Nodes.Parent_Node (Node) = null));
                  --   DOM.Core.Nodes.Print (DOM.Core.Attrs.Owner_Element (Node));
                  ---mmmDEBUG END

                  if Debug_Active then
                     -- TODO: reinsert this code once V811-012 is resolved
                     --     Mulog.Log (Msg => " Debug-Info for error: "
                     --                   & ASCII.LF
                     --                   & Mutools.Xmldebuglog.Get_Log_For_Error_Message
                     --                   (Node => Owner (Node)));

                     Mulog.Log (Msg => " Dummy-Debug-Info for error (wait for bug to be resolved)"
                                   & ASCII.LF
                                   & Mutools.Xmldebuglog.Get_Log_For_Error_Message
                                   (Node => Node));
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
