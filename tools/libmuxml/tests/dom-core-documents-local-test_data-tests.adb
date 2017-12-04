--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into DOM.Core.Documents.Local.Test_Data.

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
package body DOM.Core.Documents.Local.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Adopt_Node (Gnattest_T : in out Test);
   procedure Test_Adopt_Node_6a4a84 (Gnattest_T : in out Test) renames Test_Adopt_Node;
--  id:2.2/6a4a8407a73d6d53/Adopt_Node/1/0/
   procedure Test_Adopt_Node (Gnattest_T : in out Test) is
   --  dom-core-documents-local.ads:32:4:Adopt_Node
--  end read only

      pragma Unreferenced (Gnattest_T);

      Doc_A, Doc_B   : DOM.Core.Document;
      Impl_A, Impl_B : DOM.Core.DOM_Implementation;
      Node           : DOM.Core.Node;
   begin
      Doc_A := DOM.Core.Create_Document (Implementation => Impl_A);
      Node  := DOM.Core.Documents.Create_Element
        (Doc      => Doc_A,
         Tag_Name => "A");

      Doc_B := DOM.Core.Create_Document (Implementation => Impl_B);

      Assert (Condition => DOM.Core.Nodes.Owner_Document (N => Node) = Doc_A,
              Message   => "Node does not belong to document A");

      Node := DOM.Core.Documents.Local.Adopt_Node
        (Doc    => Doc_B,
         Source => Node);
      Assert (Condition => DOM.Core.Nodes.Owner_Document (N => Node) = Doc_B,
              Message   => "Node not adopted");
--  begin read only
   end Test_Adopt_Node;
--  end read only


--  begin read only
   procedure Test_Clone_Node (Gnattest_T : in out Test);
   procedure Test_Clone_Node_891724 (Gnattest_T : in out Test) renames Test_Clone_Node;
--  id:2.2/8917241662f94bd7/Clone_Node/1/0/
   procedure Test_Clone_Node (Gnattest_T : in out Test) is
   --  dom-core-documents-local.ads:34:4:Clone_Node
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Sax.Symbols.Symbol;

      Doc            : DOM.Core.Document;
      Impl           : DOM.Core.DOM_Implementation;
      Node_A, Node_B : DOM.Core.Node;
   begin
      Doc := DOM.Core.Create_Document (Implementation => Impl);
      Node_A := DOM.Core.Documents.Create_Element
        (Doc      => Doc,
         Tag_Name => "A");
      DOM.Core.Elements.Set_Attribute (Elem  => Node_A,
                                       Name  => "attr1",
                                       Value => "foo");
      DOM.Core.Elements.Set_Attribute (Elem  => Node_A,
                                       Name  => "attr2",
                                       Value => "bar");

      Node_B := Clone_Node (N    => Node_A,
                            Deep => True);

      Assert (Condition => Node_B.Attributes.Last = 1,
              Message   => "Attribute count mismatch");
      for I in 0 .. Node_B.Attributes.Last loop
         Assert (Condition => Node_B.Attributes.Items (I).Attr_Name
                 = Node_A.Attributes.Items (I).Attr_Name,
                 Message   => "Attr_Name mismatch" & I'Img);
         Assert (Condition => Node_B.Attributes.Items (I).Attr_Value
                 = Node_A.Attributes.Items (I).Attr_Value,
                 Message   => "Attr_Value mismatch" & I'Img);
         Assert (Condition => Node_B.Attributes.Items (I).Is_Id
                 = Node_A.Attributes.Items (I).Is_Id,
                 Message   => "Is_Id mismatch" & I'Img);
         Assert (Condition => Node_B.Attributes.Items (I).Specified
                 = Node_A.Attributes.Items (I).Specified,
                 Message   => "Specified mismatch" & I'Img);
      end loop;
--  begin read only
   end Test_Clone_Node;
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
end DOM.Core.Documents.Local.Test_Data.Tests;
