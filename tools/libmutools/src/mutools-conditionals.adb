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

with Ada.Strings.Fixed.Equal_Case_Insensitive;

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Muxml.Utils;

package body Mutools.Conditionals
is

   --  Transfer all children from specified node to parent.
   procedure Transfer_Children
     (Parent : DOM.Core.Node;
      Node   : DOM.Core.Node);

   -------------------------------------------------------------------------

   procedure Evaluate
     (Config : DOM.Core.Node_List;
      Parent : DOM.Core.Node)
   is
      use type DOM.Core.Node;

      Next_Child : DOM.Core.Node;
      Cur_Child  : DOM.Core.Node := DOM.Core.Nodes.First_Child (N => Parent);
   begin
      while Cur_Child /= null loop

         --  Recursively evaluate children before processing conditional.

         Evaluate (Config => Config,
                   Parent => Cur_Child);

         --  Get next child before potentially removing current child from
         --  parent.

         Next_Child := DOM.Core.Nodes.Next_Sibling (N => Cur_Child);

         if DOM.Core.Nodes.Node_Name (N => Cur_Child) = "if" then
            declare
               Value     : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Cur_Child,
                    Name => "value");
               Cfg_Name  : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Cur_Child,
                    Name => "variable");
               Cfg_Value : constant String
                 := Muxml.Utils.Get_Attribute
                   (Nodes     => Config,
                    Ref_Attr  => "name",
                    Ref_Value => Cfg_Name,
                    Attr_Name => "value");
               Dummy     : DOM.Core.Node;
            begin
               if Ada.Strings.Fixed.Equal_Case_Insensitive
                 (Left  => Value,
                  Right => Cfg_Value)
               then
                  Transfer_Children (Parent => Parent,
                                     Node   => Cur_Child);
               end if;

               Dummy := DOM.Core.Nodes.Remove_Child
                 (N         => Parent,
                  Old_Child => Cur_Child);
            end;
         end if;

         Cur_Child := Next_Child;
      end loop;
   end Evaluate;

   -------------------------------------------------------------------------

   procedure Expand (Policy : Muxml.XML_Data_Type)
   is
      Config_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/*/config/*");
      Sections : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query (N     => Policy.Doc,
                                            XPath => "/*");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Sections) - 1 loop
         declare
            Cur_Section : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Sections,
                                      Index => I);
         begin
            Evaluate (Config => Config_Nodes,
                      Parent => Cur_Section);
         end;
      end loop;
   end Expand;

   -------------------------------------------------------------------------

   procedure Transfer_Children
     (Parent : DOM.Core.Node;
      Node   : DOM.Core.Node)
   is
      use type DOM.Core.Node;

      Cur_Child : DOM.Core.Node;
   begin
      loop
         Cur_Child := DOM.Core.Nodes.First_Child (N => Node);
         exit when Cur_Child = null;

         Cur_Child := DOM.Core.Nodes.Remove_Child
           (N         => Node,
            Old_Child => Cur_Child);
         Cur_Child := DOM.Core.Nodes.Insert_Before
           (N         => Parent,
            New_Child => Cur_Child,
            Ref_Child => Node);
      end loop;
   end Transfer_Children;

end Mutools.Conditionals;
