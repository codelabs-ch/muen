--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Templates.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Mucfgcheck.Validation_Errors;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;
with McKae.XML.XPath.XIA;
--  begin read only
--  end read only
package body Mucfgcheck.Templates.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Name_Uniqueness_7f1559 (Gnattest_T : in out Test) renames Test_Name_Uniqueness;
--  id:2.2/7f15594730cfb6f0/Name_Uniqueness/1/0/
   procedure Test_Name_Uniqueness (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Node : DOM.Core.Node;
      Data : Muxml.XML_Data_Type;
   begin
      -- positive test with 3 templates
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/system_policy_templateAmend.xml");
      Validation_Errors.Clear;

      Name_Uniqueness (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      -- negative test: two templates with same name
      Node := DOM.Core.Nodes.Item
         (List  => McKae.XML.XPath.XIA.XPath_Query
                      (N     => Data.Doc,
                       XPath => "//template[@name='oneline_mem_template']"),
          Index => 0);
      DOM.Core.Elements.Set_Attribute
         (Elem => Node,
          Name => "name",
          Value => "big_dev_template");
      Validation_Errors.Clear;
      Name_Uniqueness (XML_Data => Data);
      Assert (Condition =>  Validation_Errors.Contains
                               (Msg => "Template names are not unique. "
                                & "Conflicting value: 'big_dev_template'"),
              Message   => "Exception mismatch");

--  begin read only
   end Test_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Template_Integrity (Gnattest_T : in out Test);
   procedure Test_Template_Integrity_fa27ce (Gnattest_T : in out Test) renames Test_Template_Integrity;
--  id:2.2/fa27ce48dc1c3ac1/Template_Integrity/1/0/
   procedure Test_Template_Integrity (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Node, New_Node, Body_Parent : DOM.Core.Node;
      Data                        : Muxml.XML_Data_Type;
   begin
      -- positive test with 3 templates
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/system_policy_templateAmend.xml");

      Validation_Errors.Clear;
      Template_Integrity (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      -- negative test: template with two bodies
      Node := DOM.Core.Nodes.Item
                 (List  => McKae.XML.XPath.XIA.XPath_Query
                              (N     => Data.Doc,
                               XPath => "//template/body"),
                  Index => 1);
      Body_Parent := DOM.Core.Nodes.Parent_Node (N => Node);
      New_Node    := DOM.Core.Documents.Create_Element
                        (Doc      => Data.Doc,
                         Tag_Name => "body");
      New_Node    := DOM.Core.Nodes.Insert_Before
                        (N         => Body_Parent,
                         New_Child => New_Node,
                         Ref_Child => Node);

      Validation_Errors.Clear;
      Template_Integrity (XML_Data => Data);
      Assert (Condition =>  Validation_Errors.Contains
                               (Msg => "Found template definition "
                                       & "with multiple bodies."),
              Message   => "Exception mismatch");

      -- negative test: template without body
      Node     := DOM.Core.Nodes.Remove_Child
                     (N         => Body_Parent,
                      Old_Child => Node);
      New_Node := DOM.Core.Nodes.Remove_Child
                     (N         => DOM.Core.Nodes.Parent_Node (N => New_Node),
                      Old_Child => New_Node);
      DOM.Core.Nodes.Free(New_Node);

      Validation_Errors.Clear;
      Template_Integrity (XML_Data => Data);
      Assert (Condition =>  Validation_Errors.Contains
                              (Msg => "Found template definition without body."),
              Message   => "Exception mismatch");

      -- negative test: template with two parameter blocks
      Node := DOM.Core.Nodes.Insert_Before
                 (N         => Body_Parent,
                  New_Child => Node);
      Node := DOM.Core.Nodes.Item
                 (List  => McKae.XML.XPath.XIA.XPath_Query
                              (N     => Data.Doc,
                               XPath => "//template/parameters"),
                  Index => 0);
      New_Node := DOM.Core.Documents.Create_Element
                     (Doc      => Data.Doc,
                      Tag_Name => "parameters");
      New_Node := DOM.Core.Nodes.Insert_Before
                     (N         => DOM.Core.Nodes.Parent_Node (N => Node),
                      New_Child => New_Node,
                      Ref_Child => Node);

      Validation_Errors.Clear;
      Template_Integrity (XML_Data => Data);
      Assert (Condition =>  Validation_Errors.Contains
              (Msg => "Found template definition"
                      & " with multiple parameter blocks."),
              Message   => "Exception mismatch");

      -- negative test: template without prameter block
      Node := DOM.Core.Nodes.Remove_Child
                 (N         => DOM.Core.Nodes.Parent_Node (N => Node),
                  Old_Child => Node);
      DOM.Core.Nodes.Free(Node);
      New_Node := DOM.Core.Nodes.Remove_Child
                     (N         => DOM.Core.Nodes.Parent_Node (N => New_Node),
                      Old_Child => New_Node);
      DOM.Core.Nodes.Free(New_Node);

      Validation_Errors.Clear;
      Template_Integrity (XML_Data => Data);
      Assert (Condition =>  Validation_Errors.Contains
              (Msg => "Found template definition"
                      & " without parameter declaration."),
              Message   => "Exception mismatch");

--  begin read only
   end Test_Template_Integrity;
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
end Mucfgcheck.Templates.Test_Data.Tests;
