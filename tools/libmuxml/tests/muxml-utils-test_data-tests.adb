--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Muxml.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Muxml.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_1_Get_Attribute (Gnattest_T : in out Test);
   procedure Test_Get_Attribute_7606a9 (Gnattest_T : in out Test) renames Test_1_Get_Attribute;
--  id:2.2/7606a922e00da111/Get_Attribute/1/0/
   procedure Test_1_Get_Attribute (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

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
      Append_Child
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

      Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "grandchild"));

      Assert (Condition => Get_Attribute
              (Doc   => Data.Doc,
               XPath => "parent",
               Name  => "parentAttr") = "parent_attribute",
              Message   => "Attribute mismatch (1)");
      Assert (Condition => Get_Attribute
              (Doc   => Data.Doc,
               XPath => "/parent/child",
               Name  => "childAttr") = "child_attribute",
              Message   => "Attribute mismatch (2)");
      Assert (Condition => Get_Attribute
              (Doc   => Data.Doc,
               XPath => "parent",
               Name  => "nonexistent") = "",
              Message   => "Attribute mismatch (3)");
      Assert (Condition => Get_Attribute
              (Doc   => Data.Doc,
               XPath => "//grandchild",
               Name  => "nonexistent") = "",
              Message   => "Attribute mismatch (4)");
      Assert (Condition => Get_Attribute
              (Doc   => Data.Doc,
               XPath => "nonexistent",
               Name  => "someAttribute") = "",
              Message   => "Attribute mismatch (5)");
--  begin read only
   end Test_1_Get_Attribute;
--  end read only


--  begin read only
   procedure Test_Set_Attribute (Gnattest_T : in out Test);
   procedure Test_Set_Attribute_9ffe3d (Gnattest_T : in out Test) renames Test_Set_Attribute;
--  id:2.2/9ffe3df58c900cd0/Set_Attribute/1/0/
   procedure Test_Set_Attribute (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "elem");
      Append_Child (Node      => Data.Doc,
                    New_Child => Node);

      Set_Attribute (Doc   => Data.Doc,
                     XPath => "/elem",
                     Name  => "test",
                     Value => "value");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "test") = "value",
              Message   => "Attribute value mismatch");

      begin
         Set_Attribute (Doc   => Data.Doc,
                        XPath => "/nonexistent",
                        Name  => "foo",
                        Value => "bar");
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : XML_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unable to set attribute 'foo' to value 'bar' - No "
                    & "element found at XPath '/nonexistent'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Set_Attribute;
--  end read only


--  begin read only
   procedure Test_1_Get_Element (Gnattest_T : in out Test);
   procedure Test_Get_Element_d0ef1a (Gnattest_T : in out Test) renames Test_1_Get_Element;
--  id:2.2/d0ef1aa70f2c2b3a/Get_Element/1/0/
   procedure Test_1_Get_Element (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type DOM.Core.Node;

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "parent");
      Append_Child
        (Node      => Data.Doc,
         New_Child => Node);
      Node := DOM.Core.Nodes.Append_Child
        (N         => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "child"));
      Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "grandchild"));

      Assert (Condition => DOM.Core.Nodes.Node_Name
              (N => Get_Element
               (Doc   => Data.Doc,
                XPath => "parent")) = "parent",
              Message   => "Element mismatch (1)");
      Assert (Condition => DOM.Core.Nodes.Node_Name
              (N => Get_Element
               (Doc   => Data.Doc,
                XPath => "/parent/child")) = "child",
              Message   => "Element mismatch (2)");
      Assert (Condition => DOM.Core.Nodes.Node_Name
              (N => Get_Element
               (Doc   => Data.Doc,
                XPath => "//grandchild")) = "grandchild",
              Message   => "Element mismatch (3)");
      Assert (Condition => Get_Element
              (Doc   => Data.Doc,
               XPath => "nonexistent") = null,
              Message   => "Element mismatch (4)");
      Assert (Condition => Get_Element
              (Doc   => null,
               XPath => "/") = null,
              Message   => "Element mismatch (5)");

      begin
         declare
            Dummy : constant DOM.Core.Node := Get_Element
              (Doc   => Data.Doc,
               XPath => "");
         begin
            Assert (Condition => False,
                    Message   => "Exception expected");
         end;

      exception
         when E : XML_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "No XPath given",
                    Message   => "Exception message mismatch");
      end;
--  begin read only
   end Test_1_Get_Element;
--  end read only


--  begin read only
   procedure Test_Get_Element_Value (Gnattest_T : in out Test);
   procedure Test_Get_Element_Value_69bad0 (Gnattest_T : in out Test) renames Test_Get_Element_Value;
--  id:2.2/69bad06be5a13405/Get_Element_Value/1/0/
   procedure Test_Get_Element_Value (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "parent");
      Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Text_Node
           (Doc  => Data.Doc,
            Data => "parent text"));
      Append_Child
        (Node      => Data.Doc,
         New_Child => Node);
      Node := DOM.Core.Nodes.Append_Child
        (N         => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "child"));
      Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Text_Node
           (Doc  => Data.Doc,
            Data => "child text"));
      Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "grandchild"));

      Assert (Condition => Get_Element_Value
              (Doc   => Data.Doc,
               XPath => "parent") = "parent text",
              Message   => "Element value mismatch (1)");
      Assert (Condition => Get_Element_Value
              (Doc   => Data.Doc,
               XPath => "parent/child") = "child text",
              Message   => "Element value mismatch (2)");
      Assert (Condition => Get_Element_Value
              (Doc   => Data.Doc,
               XPath => "//grandchild") = "",
              Message   => "Element value mismatch (3)");
      Assert (Condition => Get_Element_Value
              (Doc   => Data.Doc,
               XPath => "nonexistent") = "",
              Message   => "Element value mismatch (4)");
--  begin read only
   end Test_Get_Element_Value;
--  end read only


--  begin read only
   procedure Test_Set_Element_Value (Gnattest_T : in out Test);
   procedure Test_Set_Element_Value_74b2a4 (Gnattest_T : in out Test) renames Test_Set_Element_Value;
--  id:2.2/74b2a414a94397f6/Set_Element_Value/1/0/
   procedure Test_Set_Element_Value (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "elem");
      Append_Child (Node      => Data.Doc,
                    New_Child => Node);
      Add_Child (Parent     => Node,
                 Child_Name => "child");

      Set_Element_Value  (Doc   => Data.Doc,
                          XPath => "/elem",
                          Value => "value");
      Assert (Condition => Get_Element_Value
              (Doc   => Data.Doc,
               XPath => "/elem") = "value",
              Message   => "Element value mismatch");

      Set_Element_Value  (Doc   => Data.Doc,
                          XPath => "/elem",
                          Value => "newvalue");
      Assert (Condition => Get_Element_Value
              (Doc   => Data.Doc,
               XPath => "/elem") = "newvalue",
              Message   => "New element value mismatch");

      Set_Element_Value  (Doc   => Node,
                          XPath => "child",
                          Value => "childvalue");
      Assert (Condition => Get_Element_Value
              (Doc   => Data.Doc,
               XPath => "/elem/child") = "childvalue",
              Message   => "Child value mismatch");

      begin
         Set_Element_Value  (Doc   => Data.Doc,
                             XPath => "/nonexistent",
                             Value => "foobar");
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : XML_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unable to set element value to 'foobar' - No "
                    & "element found at XPath '/nonexistent'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Set_Element_Value;
--  end read only


--  begin read only
   procedure Test_Contains (Gnattest_T : in out Test);
   procedure Test_Contains_5dfe22 (Gnattest_T : in out Test) renames Test_Contains;
--  id:2.2/5dfe2270196ad4e4/Contains/1/0/
   procedure Test_Contains (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl   : DOM.Core.DOM_Implementation;
      Data   : XML_Data_Type;
      List   : DOM.Core.Node_List;
      Node_A : DOM.Core.Node;
      Node_B : DOM.Core.Node;
   begin
      Assert (Condition => not Contains (List => List,
                                         Node => Node_A),
              Message   => "Empty list contains empty node");

      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node_A := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "foo");
      Assert (Condition => not Contains (List => List,
                                         Node => Node_A),
              Message   => "Empty list contains node");

      DOM.Core.Append_Node (List => List,
                            N    => Node_A);
      Assert (Condition => Contains (List => List,
                                     Node => Node_A),
              Message   => "List does not contain node (1)");

      Assert (Condition => not Contains (List => List,
                                         Node => Node_B),
              Message   => "List contains empty node");

      Node_B := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "bar");
      Assert (Condition => not Contains (List => List,
                                         Node => Node_B),
              Message   => "List contains node");

      DOM.Core.Append_Node (List => List,
                            N    => Node_B);
      Assert (Condition => Contains (List => List,
                                     Node => Node_B),
              Message   => "List does not contain node (2)");
--  begin read only
   end Test_Contains;
--  end read only


--  begin read only
   procedure Test_2_Get_Element (Gnattest_T : in out Test);
   procedure Test_Get_Element_710ca6 (Gnattest_T : in out Test) renames Test_2_Get_Element;
--  id:2.2/710ca65d3e87fd3c/Get_Element/0/0/
   procedure Test_2_Get_Element (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      use type DOM.Core.Node;

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
      List : DOM.Core.Node_List;
   begin
      Assert (Condition => Muxml.Utils.Get_Element
              (Nodes     => List,
               Ref_Attr  => "name",
               Ref_Value => "foo") = null,
              Message   => "Element in empty list");

      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "bar");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "bar");
      DOM.Core.Append_Node
        (List => List,
         N    => Node);
      Assert (Condition => Muxml.Utils.Get_Element
              (Nodes     => List,
               Ref_Attr  => "name",
               Ref_Value => "foo") = null,
              Message   => "Element in 'bar' list");

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "foo");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "foo");
      DOM.Core.Append_Node
        (List => List,
         N    => Node);
      Assert (Condition => Muxml.Utils.Get_Element
              (Nodes     => List,
               Ref_Attr  => "name",
               Ref_Value => "foo") = Node,
              Message   => "Element not found");
--  begin read only
   end Test_2_Get_Element;
--  end read only


--  begin read only
   procedure Test_Get_Elements (Gnattest_T : in out Test);
   procedure Test_Get_Elements_e02b1f (Gnattest_T : in out Test) renames Test_Get_Elements;
--  id:2.2/e02b1ff46879608e/Get_Elements/1/0/
   procedure Test_Get_Elements (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      use type DOM.Core.Node;

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
      List : DOM.Core.Node_List;
   begin
      Assert (Condition => DOM.Core.Nodes.Length
              (List => Get_Elements
               (Nodes     => List,
                Ref_Attr  => "name",
                Ref_Value => "foo")) = 0,
              Message   => "Elements in empty list");

      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "bar");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "bar");
      DOM.Core.Append_Node
        (List => List,
         N    => Node);
      Assert (Condition => DOM.Core.Nodes.Length
              (List => Get_Elements
               (Nodes     => List,
                Ref_Attr  => "name",
                Ref_Value => "foo")) = 0,
              Message   => "Element in 'bar' list");

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "foo");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "foo");
      DOM.Core.Append_Node
        (List => List,
         N    => Node);
      declare
         Matches : constant DOM.Core.Node_List
           := Get_Elements
             (Nodes     => List,
              Ref_Attr  => "name",
              Ref_Value => "foo");
      begin
         Assert (Condition => DOM.Core.Nodes.Length
                 (List => Matches) = 1,
                 Message   => "Element not found (1)");
         Assert (Condition => DOM.Core.Nodes.Item
                 (List  => Matches,
                  Index => 0) = Node,
                 Message   => "Invalid match found");
      end;

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "foo");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "foo");
      DOM.Core.Append_Node
        (List => List,
         N    => Node);
      Assert (Condition => DOM.Core.Nodes.Length
              (List => Get_Elements
               (Nodes     => List,
                Ref_Attr  => "name",
                Ref_Value => "foo")) = 2,
              Message   => "Elements not found (2)");
--  begin read only
   end Test_Get_Elements;
--  end read only


--  begin read only
   procedure Test_2_Get_Attribute (Gnattest_T : in out Test);
   procedure Test_Get_Attribute_002470 (Gnattest_T : in out Test) renames Test_2_Get_Attribute;
--  id:2.2/002470113061adf7/Get_Attribute/0/0/
   procedure Test_2_Get_Attribute (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
      List : DOM.Core.Node_List;
   begin
      Assert (Condition => Muxml.Utils.Get_Attribute
              (Nodes     => List,
               Ref_Attr  => "name",
               Ref_Value => "foo",
               Attr_Name => "attr") = "",
              Message   => "Element with attr in empty list");

      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "bar");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "attr",
         Value => "bar");
      DOM.Core.Append_Node
        (List => List,
         N    => Node);
      Assert (Condition => Muxml.Utils.Get_Attribute
              (Nodes     => List,
               Ref_Attr  => "name",
               Ref_Value => "foo",
               Attr_Name => "attr") = "",
              Message   => "Element with attr in 'bar' list");

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "foo");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "foo");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "attr",
         Value => "value");
      DOM.Core.Append_Node
        (List => List,
         N    => Node);
      Assert (Condition => Muxml.Utils.Get_Attribute
              (Nodes     => List,
               Ref_Attr  => "name",
               Ref_Value => "foo",Attr_Name => "attr") = "value",
              Message   => "Element with attr not found");
--  begin read only
   end Test_2_Get_Attribute;
--  end read only


--  begin read only
   procedure Test_3_Get_Element (Gnattest_T : in out Test);
   procedure Test_Get_Element_93bcee (Gnattest_T : in out Test) renames Test_3_Get_Element;
--  id:2.2/93bceee1efa8b897/Get_Element/0/0/
   procedure Test_3_Get_Element (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;
      use type DOM.Core.Node;

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
      List : DOM.Core.Node_List;
   begin
      Assert (Condition => Get_Element
              (Nodes => List,
               Refs  => ((Name  => To_Unbounded_String ("name"),
                          Value => To_Unbounded_String ("foo")),
                         (Name  => To_Unbounded_String ("type"),
                          Value => To_Unbounded_String ("bar")))) = null,
              Message   => "Element in empty list");

      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "bar");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "bar");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "type",
         Value => "bar");
      DOM.Core.Append_Node
        (List => List,
         N    => Node);
      Assert (Condition => Get_Element
              (Nodes => List,
               Refs  => ((Name  => To_Unbounded_String ("name"),
                          Value => To_Unbounded_String ("foo")),
                         (Name  => To_Unbounded_String ("type"),
                          Value => To_Unbounded_String ("bar")))) = null,
              Message   => "Element in 'bar' list");

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "foo");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "foo");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "type",
         Value => "foo");
      DOM.Core.Append_Node
        (List => List,
         N    => Node);
      Assert (Condition => Get_Element
              (Nodes => List,
               Refs  => ((Name  => To_Unbounded_String ("name"),
                          Value => To_Unbounded_String ("foo")),
                         (Name  => To_Unbounded_String ("type"),
                          Value => To_Unbounded_String ("bar")))) = null,
              Message   => "Element 'foo' with type 'foo' found");

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "foobar");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "foo");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "type",
         Value => "bar");
      DOM.Core.Append_Node
        (List => List,
         N    => Node);
      Assert (Condition => Get_Element
              (Nodes => List,
               Refs  => ((Name  => To_Unbounded_String ("name"),
                          Value => To_Unbounded_String ("foo")),
                         (Name  => To_Unbounded_String ("type"),
                          Value => To_Unbounded_String ("bar")))) = Node,
              Message   => "Element not found");
--  begin read only
   end Test_3_Get_Element;
--  end read only


--  begin read only
   procedure Test_3_Get_Attribute (Gnattest_T : in out Test);
   procedure Test_Get_Attribute_55e944 (Gnattest_T : in out Test) renames Test_3_Get_Attribute;
--  id:2.2/55e944c46d226e1f/Get_Attribute/0/0/
   procedure Test_3_Get_Attribute (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;
      use type DOM.Core.Node;

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
      List : DOM.Core.Node_List;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "bar");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "bar");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "type",
         Value => "bar");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "value",
         Value => "bar");
      DOM.Core.Append_Node
        (List => List,
         N    => Node);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "foo");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "foo");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "type",
         Value => "foo");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "value",
         Value => "foo");
      DOM.Core.Append_Node
        (List => List,
         N    => Node);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "foobar");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "foo");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "type",
         Value => "bar");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "value",
         Value => "foobar");
      DOM.Core.Append_Node
        (List => List,
         N    => Node);

      Assert (Condition => Get_Attribute
              (Nodes     => List,
               Refs      => ((Name  => To_Unbounded_String ("name"),
                              Value => To_Unbounded_String ("foo")),
                             (Name  => To_Unbounded_String ("type"),
                              Value => To_Unbounded_String ("bar"))),
               Attr_Name => "value") = "foobar",
              Message   => "Attribute mismatch not found");
--  begin read only
   end Test_3_Get_Attribute;
--  end read only


--  begin read only
   procedure Test_Remove_Elements (Gnattest_T : in out Test);
   procedure Test_Remove_Elements_2dcc79 (Gnattest_T : in out Test) renames Test_Remove_Elements;
--  id:2.2/2dcc79c974ea7d6c/Remove_Elements/1/0/
   procedure Test_Remove_Elements (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type DOM.Core.Node;

      Impl   : DOM.Core.DOM_Implementation;
      Data   : XML_Data_Type;
      Node   : DOM.Core.Node;
      Parent : DOM.Core.Node;
     begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Parent := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "parent");
      Append_Child
        (Node      => Data.Doc,
         New_Child => Parent);
      Node := DOM.Core.Nodes.Append_Child
        (N         => Parent,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "child_1"));
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "child");
      Node := DOM.Core.Nodes.Append_Child
        (N         => Parent,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "child_2"));
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "child");
      Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "grandchild_1"));
      Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "grandchild_2"));

      Remove_Elements (Doc   => Data.Doc,
                       XPath => "/parent/child_2/grandchild_2");
      Assert (Condition => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/parent/child_2/grandchild_2") = null,
              Message   => "Element not removed (1)");
      Assert (Condition => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/parent/child_2/grandchild_1") /= null,
              Message   => "Sibling removed (1)");

      Remove_Elements (Doc   => Data.Doc,
                       XPath => "/parent/*[@name='child']");
      Assert (Condition => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/parent/*") = null,
              Message   => "Element not removed (2)");
      Assert (Condition => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/parent") /= null,
              Message   => "Parent element removed");

      --  Must not raise an exception.

      Remove_Elements (Doc   => Data.Doc,
                       XPath => "nonexistent");
--  begin read only
   end Test_Remove_Elements;
--  end read only


--  begin read only
   procedure Test_Append (Gnattest_T : in out Test);
   procedure Test_Append_6bcf00 (Gnattest_T : in out Test) renames Test_Append;
--  id:2.2/6bcf005e971aed10/Append/1/0/
   procedure Test_Append (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl        : DOM.Core.DOM_Implementation;
      Data        : XML_Data_Type;
      Node        : DOM.Core.Node;
      Left, Right : DOM.Core.Node_List;
   begin
      Append (Left  => Left,
              Right => Right);
      Assert (Condition => DOM.Core.Nodes.Length (List => Left) = 0,
              Message   => "Length not zero");

      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "node");
      Append_Child
        (Node      => Data.Doc,
         New_Child => Node);

      Left := DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Doc      => Data.Doc,
         Tag_Name => "node");
      Assert (Condition => DOM.Core.Nodes.Length (List => Left) = 1,
              Message   => "Left length not 1:" & DOM.Core.Nodes.Length
                (List => Left)'Img);

      Right := Left;

      Append (Left  => Left,
              Right => Right);
      Assert (Condition => DOM.Core.Nodes.Length (List => Left) = 2,
              Message   => "Length not 2:" & DOM.Core.Nodes.Length
                (List => Left)'Img);
--  begin read only
   end Test_Append;
--  end read only


--  begin read only
   procedure Test_Append_Child (Gnattest_T : in out Test);
   procedure Test_Append_Child_0b6f31 (Gnattest_T : in out Test) renames Test_Append_Child;
--  id:2.2/0b6f317beeb588d9/Append_Child/1/0/
   procedure Test_Append_Child (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

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
      Append_Child (Node      => Node,
                    New_Child => Child);

      Assert (Condition => DOM.Core.Nodes.Has_Child_Nodes (N => Node),
              Message   => "Error appending child");
      Assert (Condition => DOM.Core.Nodes.First_Child (N => Node) = Child,
              Message   => "Child mismatch");
--  begin read only
   end Test_Append_Child;
--  end read only


--  begin read only
   procedure Test_1_Insert_Before (Gnattest_T : in out Test);
   procedure Test_Insert_Before_dad83c (Gnattest_T : in out Test) renames Test_1_Insert_Before;
--  id:2.2/dad83cdf3cb585d7/Insert_Before/1/0/
   procedure Test_1_Insert_Before (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type DOM.Core.Node;

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node, Child1, Child2, Ref_Child : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "node");
      Ref_Child := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "refchild");
      Append_Child (Node      => Node,
                    New_Child => Ref_Child);

      Child1 := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "child1");
      Insert_Before (Parent    => Node,
                     New_Child => Child1,
                     Ref_Child => "refchild");
      Assert (Condition => Child1 = DOM.Core.Nodes.Previous_Sibling
              (N => Ref_Child),
              Message   => "Child not inserted before reference child");

      Child2 := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "child2");
      Insert_Before (Parent    => Node,
                     New_Child => Child2,
                     Ref_Child => "nonexistent");
      Assert (Condition => Child2 = DOM.Core.Nodes.Last_Child (N => Node),
              Message   => "Child with nonexistent ref node not appended");
--  begin read only
   end Test_1_Insert_Before;
--  end read only


--  begin read only
   procedure Test_Merge (Gnattest_T : in out Test);
   procedure Test_Merge_64a439 (Gnattest_T : in out Test) renames Test_Merge;
--  id:2.2/64a439c9547e9313/Merge/1/0/
   procedure Test_Merge (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Positive
      is
         Data : Muxml.XML_Data_Type;
         Impl : DOM.Core.DOM_Implementation;
         Doc  : constant DOM.Core.Document
           := DOM.Core.Create_Document (Implementation => Impl);
         Node, Tmp : DOM.Core.Node;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.VCPU_Profile,
                      File => "data/vcpu_profile.xml");

         --  Construct the following XML structure:
         --  <vcpu>
         --   <registers>
         --    <segments>
         --     <cs selector="16#ffff# access="16#cafe#">text</cs>
         --    </segments>
         --   </registers>
         --  </vcpu>

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
         Append_Child
           (Node      => Node,
            New_Child => DOM.Core.Documents.Create_Text_Node
              (Doc  => Doc,
               Data => "text"));
         Tmp := DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "segments");
         Append_Child (Node      => Tmp,
                       New_Child => Node);
         Node := DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "registers");
         Append_Child (Node      => Node,
                       New_Child => Tmp);
         Tmp := DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "vcpu");
         Append_Child (Node      => Tmp,
                       New_Child => Node);
         Append_Child
           (Node      => Doc,
            New_Child => Tmp);

         Assert
           (Condition => Get_Attribute
              (Doc   => Data.Doc,
               XPath => "/vcpu/registers/segments/cs",
               Name  => "selector") = "16#0008#",
            Message   => "Unexpected cs selector attribute in vcpu policy");

         Merge (Left  => DOM.Core.Documents.Get_Element (Doc => Data.Doc),
                Right => DOM.Core.Documents.Get_Element (Doc => Doc));

         Assert (Condition => Get_Attribute
                 (Doc   => Data.Doc,
                  XPath => "/vcpu/registers/segments/cs",
                  Name  => "access") = "16#cafe#",
                 Message   => "Error merging XML nodes: cs access");
         Assert (Condition => Get_Attribute
                 (Doc   => Data.Doc,
                  XPath => "/vcpu/registers/segments/cs",
                  Name  => "selector") = "16#ffff#",
                 Message   => "Error merging XML nodes: cs selector");
      end Positive;

      ----------------------------------------------------------------------

      procedure Nodes_Name_Mismatch
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
         Append_Child (Node      => Doc,
                       New_Child => Node_A);

         Node_B := DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "B");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node_B,
            Name  => "attr",
            Value => "16#cafe#");

         Merge (Left  => Node_A,
                Right => Node_B);

         Assert (Condition => DOM.Core.Elements.Get_Attribute
                 (Elem => Node_A,
                  Name => "attr") = "foobar",
                 Message   => "Node B merged into Node A");
      end Nodes_Name_Mismatch;

      ----------------------------------------------------------------------

      procedure Nodes_With_List
      is
         use Ada.Strings.Unbounded;

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
               XPath => "/vcpu/msrs"),
            Index => 0);

         --  Construct the following XML structure:
         --  <msrs>
         --   <msr start="16#000c#"/>
         --   <other/>
         --   <msr start="16#0174#"/>
         --  </msrs>

         Tmp := DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "msrs");
         Node := DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "msr");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "start",
            Value => "16#000c#");
         Append_Child (Node      => Tmp,
                       New_Child => Node);
         Node := DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "other");
         Append_Child (Node      => Tmp,
                       New_Child => Node);
         Node := DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "msr");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "start",
            Value => "16#0174#");
         Append_Child (Node      => Tmp,
                       New_Child => Node);

         Merge (Left      => MSRs_Node,
                Right     => Tmp,
                List_Tags => (1 => To_Unbounded_String ("msr")));

         declare
            MSR_Count : constant Natural := DOM.Core.Nodes.Length
              (List => McKae.XML.XPath.XIA.XPath_Query
                 (N     => Data.Doc,
                  XPath => "/vcpu/msrs/msr"));
            Last_Child : constant DOM.Core.Node
              := DOM.Core.Nodes.Last_Child
                (N => Get_Element
                   (Doc   => Data.Doc,
                    XPath => "/vcpu/msrs"));
         begin
            Assert (Condition => MSR_Count = 4,
                    Message   => "Error merging child element list");
            Assert (Condition => DOM.Core.Nodes.Node_Name (N => Last_Child)
                    = "other",
                    Message   => "Unexpected order of merged children");
         end;
      end Nodes_With_List;
   begin
      Positive;
      Nodes_Name_Mismatch;
      Nodes_With_List;
--  begin read only
   end Test_Merge;
--  end read only


--  begin read only
   procedure Test_Ancestor_Node (Gnattest_T : in out Test);
   procedure Test_Ancestor_Node_314569 (Gnattest_T : in out Test) renames Test_Ancestor_Node;
--  id:2.2/3145695d1e1d2313/Ancestor_Node/1/0/
   procedure Test_Ancestor_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

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
           := Ancestor_Node
             (Node  => Node,
              Level => 2);
      begin
         Assert (Condition => DOM.Core.Nodes.Node_Name (N => Ancestor) = "top",
                 Message   => "Ancestor mismatch (1)");

         Ancestor := Ancestor_Node (Node  => Ancestor,
                                    Level => 1);
         Assert (Condition => Ancestor = null,
                 Message   => "Ancestor mismatch (2)");
      end;
--  begin read only
   end Test_Ancestor_Node;
--  end read only


--  begin read only
   procedure Test_Add_Child (Gnattest_T : in out Test);
   procedure Test_Add_Child_5fb1c1 (Gnattest_T : in out Test) renames Test_Add_Child;
--  id:2.2/5fb1c1e130edefb3/Add_Child/1/0/
   procedure Test_Add_Child (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type DOM.Core.Node;

      Impl   : DOM.Core.DOM_Implementation;
      Data   : XML_Data_Type;
      Parent : DOM.Core.Node;
      Ref_1  : DOM.Core.Node;
      Ref_2  : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Parent := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "parent");
      Append_Child
        (Node      => Data.Doc,
         New_Child => Parent);

      Add_Child (Parent     => Parent,
                 Child_Name => "child",
                 Ref_Names  => (1 => U ("nonexistent_1"),
                                2 => U ("nonexistent_2")));

      Assert (Condition => not DOM.Core.Nodes.Has_Child_Nodes (N => Parent),
              Message   => "Child inserted with nonexistent ref");

      Ref_1 := DOM.Core.Nodes.Append_Child
        (N         => Parent,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "ref_child_1"));

      Add_Child (Parent     => Parent,
                 Child_Name => "child_1",
                 Ref_Names  => (1 => U ("ref_child_1")));

      declare
         Child_Node : constant DOM.Core.Node
           := DOM.Core.Nodes.First_Child (N => Parent);
      begin
         Assert (Condition => DOM.Core.Nodes.Node_Name (N => Child_Node)
                 = "child_1",
                 Message   => "Child not inserted at front");
         Assert (Condition => DOM.Core.Nodes.Next_Sibling
                 (N => Child_Node) = Ref_1,
                 Message   => "Child not inserted before ref child");
      end;

      Ref_2 := DOM.Core.Nodes.Append_Child
        (N         => Parent,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "ref_child_2"));

      Add_Child (Parent     => Parent,
                 Child_Name => "child_2",
                 Ref_Names  => (1 => U ("nonexistent"),
                                2 => U ("ref_child_2")));
      declare
         Child_Node : constant DOM.Core.Node
           := Get_Element (Doc   => Parent,
                           XPath => "child_2");
      begin
         Assert (Condition => DOM.Core.Nodes.Node_Name (N => Child_Node)
                 = "child_2",
                 Message   => "Child not inserted (2)");
         Assert
           (Condition => DOM.Core.Nodes.Next_Sibling (N => Child_Node) = Ref_2,
            Message   => "Child not inserted before ref child (2)");
         Assert
           (Condition => DOM.Core.Nodes.Previous_Sibling
              (N => Child_Node) = Ref_1,
            Message   => "Child not inserted after ref child 1");
      end;

      Add_Child (Parent     => Parent,
                 Child_Name => "child_2",
                 Ref_Names  => (1 => U ("nonexistent"),
                                2 => U ("ref_child_2")));
      Assert (Condition => DOM.Core.Nodes.Length
              (List => DOM.Core.Nodes.Child_Nodes (N => Parent)) = 4,
              Message   => "Child inserted despite being present");
      
      Add_Child (Parent     => Parent,
                 Child_Name => "child_3");
      declare
         Child_Node : constant DOM.Core.Node
           := Get_Element (Doc   => Parent,
                           XPath => "child_3");
      begin
         Assert (Condition => DOM.Core.Nodes.Last_Child (N => Parent)
                 = Child_Node,
                 Message   => "Child not appended");
      end;
--  begin read only
   end Test_Add_Child;
--  end read only


--  begin read only
   procedure Test_2_Insert_Before (Gnattest_T : in out Test);
   procedure Test_Insert_Before_f81b27 (Gnattest_T : in out Test) renames Test_2_Insert_Before;
--  id:2.2/f81b270b5065cd5f/Insert_Before/0/0/
   procedure Test_2_Insert_Before (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type DOM.Core.Node;

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node, Child1, Child2, Child3, Ref_Child1, Ref_Child2 : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "node");
      Ref_Child1 := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "refchild1");
      Insert_Before (Parent    => Node,
                     New_Child => Ref_Child1,
                     Ref_Names => No_Tags);
      Assert (Condition => DOM.Core.Nodes.Last_Child (N => Node) = Ref_Child1,
              Message   => "Child not inserted with no ref names");

      Child1 := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "child1");
      Insert_Before (Parent    => Node,
                     New_Child => Child1,
                     Ref_Names => (1 => U ("refchild2"),
                                   2 => U ("refchild1")));
      Assert (Condition => Child1 = DOM.Core.Nodes.Previous_Sibling
              (N => Ref_Child1),
              Message   => "Child not inserted before ref node");

      Ref_Child2 := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "refchild2");
      Insert_Before (Parent    => Node,
                     New_Child => Ref_Child2,
                     Ref_Names => (1 => U ("nonexistent")));
      Assert (Condition => DOM.Core.Nodes.Last_Child (N => Node) = Ref_Child2,
              Message   => "Child with nonexistent ref node not appended");

      Child2 := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "child2");
      Insert_Before (Parent    => Node,
                     New_Child => Child2,
                     Ref_Names => (1 => U ("refchild2"),
                                   2 => U ("refchild1")));
      Assert (Condition => Child2 = DOM.Core.Nodes.Previous_Sibling
              (N => Ref_Child2),
              Message   => "Child not inserted before ref node 2");

      Child3 := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "child1");
      Insert_Before (Parent      => Node,
                     New_Child   => Child3,
                     Ref_Names   => No_Tags);
      Assert (Condition => Child1 = DOM.Core.Nodes.Previous_Sibling
              (N => Child3),
              Message   => "Child not inserted consecutively");
--  begin read only
   end Test_2_Insert_Before;
--  end read only


--  begin read only
   procedure Test_Remove_Child (Gnattest_T : in out Test);
   procedure Test_Remove_Child_540ca0 (Gnattest_T : in out Test) renames Test_Remove_Child;
--  id:2.2/540ca0eb2b0d8bd4/Remove_Child/1/0/
   procedure Test_Remove_Child (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Node     : DOM.Core.Node;
      Dom_Impl : DOM.Core.DOM_Implementation;
      Doc      : constant DOM.Core.Document
        := DOM.Core.Create_Document (Implementation => Dom_Impl);
   begin
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Doc,
         Tag_Name => "elem");

      Append_Child (Node      => Doc,
                    New_Child => Node);

      Assert (Condition => DOM.Core.Nodes.Has_Child_Nodes (N => Doc),
              Message   => "Unable to add child to document");

      Remove_Child (Node       => Doc,
                    Child_Name => "elem");

      Assert (Condition => not DOM.Core.Nodes.Has_Child_Nodes (N => Doc),
              Message   => "Error removing child node");

      begin
         Remove_Child (Node       => Doc,
                       Child_Name => "elem");
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : XML_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unable to remove child 'elem' from node '#document'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Remove_Child;
--  end read only


--  begin read only
   procedure Test_2_Get_Matching (Gnattest_T : in out Test);
   procedure Test_Get_Matching_d1f4df (Gnattest_T : in out Test) renames Test_2_Get_Matching;
--  id:2.2/d1f4dfb542781e55/Get_Matching/0/0/
   procedure Test_2_Get_Matching (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested in XPath-variant of function");
--  begin read only
   end Test_2_Get_Matching;
--  end read only


--  begin read only
   procedure Test_1_Get_Matching (Gnattest_T : in out Test);
   procedure Test_Get_Matching_4157ee (Gnattest_T : in out Test) renames Test_1_Get_Matching;
--  id:2.2/4157ee13aba27ad5/Get_Matching/1/0/
   procedure Test_1_Get_Matching (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data         : XML_Data_Type;
      Impl         : DOM.Core.DOM_Implementation;
      Parent, Node : DOM.Core.Node;
      Result       : Matching_Pairs_Type;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Parent := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "parent");
      Append_Child
        (Node      => Data.Doc,
         New_Child => Parent);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "memory");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "mem1");
      Append_Child
        (Node      => Parent,
         New_Child => Node);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "memory");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "mem2");
      Append_Child
        (Node      => Parent,
         New_Child => Node);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "memory");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "mem2");
      Append_Child
        (Node      => Parent,
         New_Child => Node);

      Result := Get_Matching
        (XML_Data    => Data,
         Left_XPath  => "/parent/memory",
         Right_XPath => "/parent/memory",
         Match       => Match_Name'Access);
      Assert (Condition => DOM.Core.Nodes.Length (List => Result.Left) = 3,
              Message   => "Left match count not 3");
      Assert (Condition => DOM.Core.Nodes.Length (List => Result.Right) = 3,
              Message   => "Right match count not 3");

      Result := Get_Matching
        (XML_Data       => Data,
         Left_XPath     => "/parent/memory",
         Right_XPath    => "/parent/memory",
         Match_Multiple => True,
         Match          => Match_Name'Access);
      Assert (Condition => DOM.Core.Nodes.Length (List => Result.Left) = 5,
              Message   => "Left match count not 5");
      Assert (Condition => DOM.Core.Nodes.Length (List => Result.Right) = 5,
              Message   => "Right match count not 5");
--  begin read only
   end Test_1_Get_Matching;
--  end read only


--  begin read only
   procedure Test_1_Get_Bounds (Gnattest_T : in out Test);
   procedure Test_Get_Bounds_473fcc (Gnattest_T : in out Test) renames Test_1_Get_Bounds;
--  id:2.2/473fcceda2e4309f/Get_Bounds/1/0/
   procedure Test_1_Get_Bounds (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Dom_Impl     : DOM.Core.DOM_Implementation;
      Policy       : Muxml.XML_Data_Type;
      Parent, Node : DOM.Core.Node;
      Lower, Upper : Interfaces.Unsigned_64;
   begin
      Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);

      Parent := DOM.Core.Documents.Create_Element (Doc      => Policy.Doc,
                                                   Tag_Name => "parent");

      Node := DOM.Core.Documents.Create_Element (Doc      => Policy.Doc,
                                                 Tag_Name => "node");
      DOM.Core.Elements.Set_Attribute (Elem  => Node,
                                       Name  => "number",
                                       Value => "99");
      Node := DOM.Core.Nodes.Append_Child (N         => Parent,
                                           New_Child => Node);
      Node := DOM.Core.Documents.Create_Element (Doc      => Policy.Doc,
                                                 Tag_Name => "node");
      DOM.Core.Elements.Set_Attribute (Elem  => Node,
                                       Name  => "number",
                                       Value => "12");
      Node := DOM.Core.Nodes.Append_Child (N         => Parent,
                                           New_Child => Node);
      Node := DOM.Core.Documents.Create_Element (Doc      => Policy.Doc,
                                                 Tag_Name => "node");
      DOM.Core.Elements.Set_Attribute (Elem  => Node,
                                       Name  => "number",
                                       Value => "54");
      Node := DOM.Core.Nodes.Append_Child (N         => Parent,
                                           New_Child => Node);
      Node := DOM.Core.Documents.Create_Element (Doc      => Policy.Doc,
                                                 Tag_Name => "node");
      DOM.Core.Elements.Set_Attribute (Elem  => Node,
                                       Name  => "number",
                                       Value => "1");
      Node := DOM.Core.Nodes.Append_Child (N         => Parent,
                                           New_Child => Node);

      Get_Bounds (Nodes     => DOM.Core.Elements.Get_Elements_By_Tag_Name
                  (Elem => Parent,
                   Name => "node"),
                  Attr_Name => "number",
                  Lower     => Lower,
                  Upper     => Upper);

      Assert (Condition => Lower = 1,
              Message   => "Lower bound mismatch:" & Lower'Img);
      Assert (Condition => Upper = 99,
              Message   => "Upper bound mismatch:" & Upper'Img);
--  begin read only
   end Test_1_Get_Bounds;
--  end read only


--  begin read only
   procedure Test_2_Get_Bounds (Gnattest_T : in out Test);
   procedure Test_Get_Bounds_37124c (Gnattest_T : in out Test) renames Test_2_Get_Bounds;
--  id:2.2/37124c1f4d014ca3/Get_Bounds/0/0/
   procedure Test_2_Get_Bounds (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type DOM.Core.Node;

      Dom_Impl                           : DOM.Core.DOM_Implementation;
      Policy                             : Muxml.XML_Data_Type;
      Parent, Node1, Node2, Lower, Upper : DOM.Core.Node;
   begin
      Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);

      Parent := DOM.Core.Documents.Create_Element
        (Doc      => Policy.Doc,
         Tag_Name => "parent");

      Node1 := DOM.Core.Documents.Create_Element
        (Doc      => Policy.Doc,
         Tag_Name => "node");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node1,
         Name  => "number",
         Value => "22");
      Node1 := DOM.Core.Nodes.Append_Child
        (N         => Parent,
         New_Child => Node1);

      Node1 := DOM.Core.Documents.Create_Element
        (Doc      => Policy.Doc,
         Tag_Name => "node");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node1,
         Name  => "number",
         Value => "12");
      Node1 := DOM.Core.Nodes.Append_Child
        (N         => Parent,
         New_Child => Node1);

      Node2 := DOM.Core.Documents.Create_Element
        (Doc      => Policy.Doc,
         Tag_Name => "node");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node2,
         Name  => "number",
         Value => "99");
      Node2 := DOM.Core.Nodes.Append_Child
        (N         => Parent,
         New_Child => Node2);

      Get_Bounds
        (Nodes     => DOM.Core.Elements.Get_Elements_By_Tag_Name
           (Elem => Parent,
            Name => "node"),
         Attr_Name => "number",
         Lower     => Lower,
         Upper     => Upper);
      Assert (Condition => Node1 = Lower,
              Message   => "Lower node mismatch");
      Assert (Condition => Node2 = Upper,
              Message   => "Upper node mismatch");
--  begin read only
   end Test_2_Get_Bounds;
--  end read only


--  begin read only
   procedure Test_Sum (Gnattest_T : in out Test);
   procedure Test_Sum_67ca21 (Gnattest_T : in out Test) renames Test_Sum;
--  id:2.2/67ca215b69ee6a24/Sum/1/0/
   procedure Test_Sum (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      function Get_Size (Node : DOM.Core.Node) return String
      is
      begin
         return DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "size");
      end Get_Size;

      List : DOM.Core.Node_List;
      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Assert (Condition => Sum
              (Nodes  => List,
               Getter => DOM.Core.Nodes.Node_Value'Access) = 0,
              Message   => "Sum not zero");

      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "e1");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "size",
         Value => "3487");
      DOM.Core.Append_Node (List => List,
                            N    => Node);
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "e2");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "size",
         Value => "124");
      DOM.Core.Append_Node (List => List,
                            N    => Node);
      Assert (Condition => Sum
              (Nodes  => List,
               Getter => Get_Size'Access) = 3611,
              Message   => "Sum mismatch");
--  begin read only
   end Test_Sum;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Muxml.Utils.Test_Data.Tests;
