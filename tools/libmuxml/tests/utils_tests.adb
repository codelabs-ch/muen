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

with Ada.Exceptions;

with DOM.Core.Nodes;
with DOM.Core.Documents;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Muxml.Utils;

package body Utils_Tests
is

   use Ahven;
   use Muxml;

   -------------------------------------------------------------------------

   procedure Append_Child
   is
      use type DOM.Core.Node;

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node, Child : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element (Doc      => Data.Doc,
                                                 Tag_Name => "node");
      Child := DOM.Core.Documents.Create_Element (Doc     => Data.Doc,
                                                  Tag_Name => "child");
      Utils.Append_Child (Node      => Node,
                          New_Child => Child);

      Assert (Condition => DOM.Core.Nodes.Has_Child_Nodes (N => Node),
              Message   => "Error appending child");
      Assert (Condition => DOM.Core.Nodes.First_Child (N => Node) = Child,
              Message   => "Child mismatch");
   end Append_Child;

   -------------------------------------------------------------------------

   procedure Get_Ancestor_Node
   is
      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "top");
      Node := DOM.Core.Nodes.Append_Child
        (N         => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "child1"));
      Node := DOM.Core.Nodes.Append_Child
        (N         => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "child2"));

      declare
         use type DOM.Core.Node;

         Ancestor : DOM.Core.Node
           := Utils.Ancestor_Node
             (Node  => Node,
              Level => 2);
      begin
         Assert (Condition => DOM.Core.Nodes.Node_Name (N => Ancestor) = "top",
                 Message   => "Ancestor mismatch (1)");

         Ancestor := Utils.Ancestor_Node (Node  => Ancestor,
                                          Level => 1);
         Assert (Condition => Ancestor = null,
                 Message   => "Ancestor mismatch (2)");
      end;
   end Get_Ancestor_Node;

   -------------------------------------------------------------------------

   procedure Get_Attribute
   is
      use type DOM.Core.Node;

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "parent");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "parentAttr",
         Value => "parent_attribute");
      Utils.Append_Child
        (Node      => Data.Doc,
         New_Child => Node);
      Node := DOM.Core.Nodes.Append_Child
        (N         => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "child"));
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "childAttr",
         Value => "child_attribute");

      Utils.Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "grandchild"));

      Assert (Condition => Utils.Get_Attribute
              (Doc   => Data.Doc,
               XPath => "parent",
               Name  => "parentAttr") = "parent_attribute",
              Message   => "Attribute mismatch (1)");
      Assert (Condition => Utils.Get_Attribute
              (Doc   => Data.Doc,
               XPath => "/parent/child",
               Name  => "childAttr") = "child_attribute",
              Message   => "Attribute mismatch (2)");
      Assert (Condition => Utils.Get_Attribute
              (Doc   => Data.Doc,
               XPath => "parent",
               Name  => "nonexistent") = "",
              Message   => "Attribute mismatch (3)");
      Assert (Condition => Utils.Get_Attribute
              (Doc   => Data.Doc,
               XPath => "//grandchild",
               Name  => "nonexistent") = "",
              Message   => "Attribute mismatch (4)");
      Assert (Condition => Utils.Get_Attribute
              (Doc   => Data.Doc,
               XPath => "nonexistent",
               Name  => "someAttribute") = "",
              Message   => "Attribute mismatch (5)");
   end Get_Attribute;

   -------------------------------------------------------------------------

   procedure Get_Element
   is
      use type DOM.Core.Node;

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "parent");
      Utils.Append_Child
        (Node      => Data.Doc,
         New_Child => Node);
      Node := DOM.Core.Nodes.Append_Child
        (N         => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "child"));
      Utils.Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "grandchild"));

      Assert (Condition => DOM.Core.Nodes.Node_Name
              (N => Utils.Get_Element
               (Doc   => Data.Doc,
                XPath => "parent")) = "parent",
              Message   => "Element mismatch (1)");
      Assert (Condition => DOM.Core.Nodes.Node_Name
              (N => Utils.Get_Element
               (Doc   => Data.Doc,
                XPath => "/parent/child")) = "child",
              Message   => "Element mismatch (2)");
      Assert (Condition => DOM.Core.Nodes.Node_Name
              (N => Utils.Get_Element
               (Doc   => Data.Doc,
                XPath => "//grandchild")) = "grandchild",
              Message   => "Element mismatch (3)");
      Assert (Condition => Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "nonexistent") = null,
              Message   => "Element mismatch (4)");
   end Get_Element;

   -------------------------------------------------------------------------

   procedure Get_Element_Value
   is
      use type DOM.Core.Node;

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "parent");
      Utils.Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Text_Node
           (Doc  => Data.Doc,
            Data => "parent text"));
      Utils.Append_Child
        (Node      => Data.Doc,
         New_Child => Node);
      Node := DOM.Core.Nodes.Append_Child
        (N         => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "child"));
      Utils.Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Text_Node
           (Doc  => Data.Doc,
            Data => "child text"));
      Utils.Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "grandchild"));

      Assert (Condition => Utils.Get_Element_Value
              (Doc   => Data.Doc,
               XPath => "parent") = "parent text",
              Message   => "Element value mismatch (1)");
      Assert (Condition => Utils.Get_Element_Value
              (Doc   => Data.Doc,
               XPath => "parent/child") = "child text",
              Message   => "Element value mismatch (2)");
      Assert (Condition => Utils.Get_Element_Value
              (Doc   => Data.Doc,
               XPath => "//grandchild") = "",
              Message   => "Element value mismatch (3)");
      Assert (Condition => Utils.Get_Element_Value
              (Doc   => Data.Doc,
               XPath => "nonexistent") = "",
              Message   => "Element value mismatch (4)");
   end Get_Element_Value;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "XML utility tests");
      T.Add_Test_Routine
        (Routine => Append_Child'Access,
         Name    => "Append XML child node");
      T.Add_Test_Routine
        (Routine => Merge_Nodes'Access,
         Name    => "Merge XML nodes");
      T.Add_Test_Routine
        (Routine => Merge_Nodes_Name_Mismatch'Access,
         Name    => "Merge XML nodes (name mismatch)");
      T.Add_Test_Routine
        (Routine => Merge_Nodes_With_List'Access,
         Name    => "Merge XML nodes (list elements)");
      T.Add_Test_Routine
        (Routine => Get_Element'Access,
         Name    => "Get element node");
      T.Add_Test_Routine
        (Routine => Get_Element_Value'Access,
         Name    => "Get element value");
      T.Add_Test_Routine
        (Routine => Get_Attribute'Access,
         Name    => "Get attribute value");
      T.Add_Test_Routine
        (Routine => Get_Ancestor_Node'Access,
         Name    => "Get ancestor node");
      T.Add_Test_Routine
        (Routine => Remove_Child'Access,
         Name    => "Remove child node");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Merge_Nodes
   is
      Data : XML_Data_Type;
      Impl : DOM.Core.DOM_Implementation;
      Doc  : constant DOM.Core.Document
        := DOM.Core.Create_Document (Implementation => Impl);
      Node, Tmp : DOM.Core.Node;
   begin
      Parse (Data => Data,
             Kind => VCPU_Profile,
             File => "data/vcpu_profile.xml");

      --  Construct the following XML structure:
      --  <vcpu><segments><cs selector="16#ffff#>text</cs></segments></vcpu>

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Doc,
         Tag_Name => "cs");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "access",
         Value => "16#cafe#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "selector",
         Value => "16#ffff#");
      Utils.Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Text_Node
           (Doc  => Doc,
            Data => "text"));
      Tmp := DOM.Core.Documents.Create_Element
        (Doc      => Doc,
         Tag_Name => "segments");
      Utils.Append_Child (Node      => Tmp,
                          New_Child => Node);
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Doc,
         Tag_Name => "vcpu");
      Utils.Append_Child (Node      => Node,
                          New_Child => Tmp);
      Utils.Append_Child
        (Node      => Doc,
         New_Child => Node);

      Assert (Condition => Utils.Get_Attribute
              (Doc   => Data.Doc,
               XPath => "/vcpu/segments/cs",
               Name  => "selector") = "16#0008#",
              Message   => "Unexpected cs selector attribute in vcpu policy");

      Utils.Merge (Left  => Data.Doc,
                   Right => Doc);

      Assert (Condition => Utils.Get_Attribute
              (Doc   => Data.Doc,
               XPath => "/vcpu/segments/cs",
               Name  => "access") = "16#cafe#",
              Message   => "Error merging XML nodes: cs access");
      Assert (Condition => Utils.Get_Attribute
              (Doc   => Data.Doc,
               XPath => "/vcpu/segments/cs",
               Name  => "selector") = "16#ffff#",
              Message   => "Error merging XML nodes: cs selector");
   end Merge_Nodes;

   -------------------------------------------------------------------------

   procedure Merge_Nodes_Name_Mismatch
   is
      Impl : DOM.Core.DOM_Implementation;
      Doc  : constant DOM.Core.Document
        := DOM.Core.Create_Document (Implementation => Impl);
      Node_A, Node_B : DOM.Core.Node;
   begin
      Node_A := DOM.Core.Documents.Create_Element
        (Doc      => Doc,
         Tag_Name => "A");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node_A,
         Name  => "attr",
         Value => "foobar");
      Utils.Append_Child (Node      => Doc,
                          New_Child => Node_A);

      Node_B := DOM.Core.Documents.Create_Element
        (Doc      => Doc,
         Tag_Name => "B");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node_B,
         Name  => "attr",
         Value => "16#cafe#");

      Utils.Merge (Left  => Node_A,
                   Right => Node_B);

      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node_A,
               Name => "attr") = "foobar",
              Message   => "Node B merged into Node A");
   end Merge_Nodes_Name_Mismatch;

   -------------------------------------------------------------------------

   procedure Merge_Nodes_With_List
   is
      Data : XML_Data_Type;
      Impl : DOM.Core.DOM_Implementation;
      Doc  : constant DOM.Core.Document
        := DOM.Core.Create_Document (Implementation => Impl);
      Node, Tmp, MSRs_Node : DOM.Core.Node;
   begin
      Parse (Data => Data,
             Kind => VCPU_Profile,
             File => "data/vcpu_profile.xml");

      MSRs_Node := DOM.Core.Nodes.Item
        (List  => McKae.XML.XPath.XIA.XPath_Query
           (N     => Data.Doc,
            XPath => "/vcpu/registers/msrs"),
         Index => 0);

      --  Construct the following XML structure:
      --  <msrs><msr start="16#0174#"/></msrs>

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Doc,
         Tag_Name => "msr");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "start",
         Value => "16#0174#");
      Tmp := DOM.Core.Documents.Create_Element
        (Doc      => Doc,
         Tag_Name => "msrs");
      Utils.Append_Child (Node      => Tmp,
                          New_Child => Node);

      Utils.Merge (Left     => MSRs_Node,
                   Right    => Tmp,
                   List_Tag => "msr");

      declare
         MSR_Count : constant Natural := DOM.Core.Nodes.Length
           (List => McKae.XML.XPath.XIA.XPath_Query
              (N     => Data.Doc,
               XPath => "/vcpu/registers/msrs/msr"));
      begin
         Assert (Condition => MSR_Count = 3,
                 Message   => "Error merging child element list");
      end;
   end Merge_Nodes_With_List;

   -------------------------------------------------------------------------

   procedure Remove_Child
   is
      Node     : DOM.Core.Node;
      Dom_Impl : DOM.Core.DOM_Implementation;
      Doc      : constant DOM.Core.Document
        := DOM.Core.Create_Document (Implementation => Dom_Impl);
   begin
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Doc,
         Tag_Name => "elem");

      Utils.Append_Child (Node      => Doc,
                          New_Child => Node);

      Assert (Condition => DOM.Core.Nodes.Has_Child_Nodes (N => Doc),
              Message   => "Unable to add child to document");

      Utils.Remove_Child (Node       => Doc,
                          Child_Name => "elem");

      Assert (Condition => not DOM.Core.Nodes.Has_Child_Nodes (N => Doc),
              Message   => "Error removing child node");

      begin
         Utils.Remove_Child (Node       => Doc,
                             Child_Name => "elem");
         Fail (Message => "Exception expected");

      exception
         when E : Utils.XML_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unable to remove child 'elem' from node '#document'",
                    Message   => "Exception mismatch");
      end;
   end Remove_Child;

end Utils_Tests;
