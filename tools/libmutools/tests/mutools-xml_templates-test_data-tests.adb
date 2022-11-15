--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.XML_Templates.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
--with Ada.Directories;

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Test_Utils;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;

with Muxml.Utils;

--  begin read only
--  end read only
package body Mutools.XML_Templates.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Expand (Gnattest_T : in out Test);
   procedure Test_Expand_4a19b8 (Gnattest_T : in out Test) renames Test_Expand;
--  id:2.2/4a19b878eb07fa84/Expand/1/0/
   procedure Test_Expand (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data    : Muxml.XML_Data_Type;
      Output1 : constant String := "obj/output_system_policy_templateAmend.xml";
      Output2 : constant String := "obj/output_test_policy_src.xml";

      procedure Positive_Test
      is
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/system_policy_templateAmend.xml");
         Expand (XML_Data     => Data,
                 Debug_Active => False);
         Muxml.Write (Data => Data,
                      Kind => Muxml.None,
                      File => Output1);
         Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => "data/output_system_policy_templateAmend.xml",
                     Filename2 => Output1),
                 Message   => "Policy mismatch: " & Output1);

         -- test one data without templates
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src.xml");
         Expand (XML_Data     => Data,
                 Debug_Active => False);
         Muxml.Write (Data => Data,
                      Kind => Muxml.None,
                      File => Output2);
         Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => "data/output_test_policy_src.xml",
                     Filename2 => Output2),
                 Message   => "Policy mismatch: " & Output2);
      end Positive_Test;

      procedure Cyclic_Dependency_Of_Templates
      is
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/system_policy_cyclicTemplates.xml");
         Expand (XML_Data     => Data,
                 Debug_Active => False);
         Assert (Condition => False,
                 Message   => "Exception expected (cyclic dependency)");
      exception
         when E : Muxml.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Nesting-depth of templates is at least 100. "
                       & "This may be due to cyclic template-inclusions.",
                    Message   => "Exception message mismatch");
      end Cyclic_Dependency_Of_Templates;

      procedure Use_Nonexisting_Template
      is
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/system_policy_templateAmend.xml");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/useTemplate[@name='template_memory']",
             Name  => "name",
             Value => "nonexisting");
         Expand (XML_Data     => Data,
                 Debug_Active => False);
         Assert (Condition => False,
                 Message   => "Exception expected (nonexisting template)");
      exception
         when E : Muxml.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Could not find a template with name: 'nonexisting'",
                    Message   => "Exception message mismatch");
      end Use_Nonexisting_Template;

   begin
      Positive_Test;
      Cyclic_Dependency_Of_Templates;
      Use_Nonexisting_Template;

--  begin read only
   end Test_Expand;
--  end read only


--  begin read only
   procedure Test_Create_XMLDocument_From_Node (Gnattest_T : in out Test);
   procedure Test_Create_XMLDocument_From_Node_b97a4e (Gnattest_T : in out Test) renames Test_Create_XMLDocument_From_Node;
--  id:2.2/b97a4eef999d0925/Create_XMLDocument_From_Node/1/0/
   procedure Test_Create_XMLDocument_From_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type DOM.Core.Node;

      Data, Output : Muxml.XML_Data_Type;
      Node         : DOM.Core.Node;
   begin
      -- positive test: parse some input, choose a node,
      --   create document, write, compare to fixed file
      --   change node, make sure original node is unchanged
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/system_policy_templateAmend.xml");
      Node := Muxml.Utils.Get_Element
                   (Doc   => Data.Doc,
                    XPath => "/system/scheduling");
      Create_XMLDocument_From_Node (New_Doc  => Output.Doc,
                                    Src_Node => Node);
      Node := Muxml.Utils.Get_Element
                (Doc   => Output.Doc,
                 XPath => "/scheduling/majorFrame/cpu[@id='0']"
                          & "/minorFrame[@subject='lnx']");
      Assert (Condition => Node /= null,
              Message   => "Could not find node in new document");

      Muxml.Utils.Set_Attribute
            (Doc   => Output.Doc,
             XPath => "/scheduling/majorFrame/cpu",
             Name  => "id",
             Value => "1");
      Assert (Condition => "0" = Muxml.Utils.Get_Attribute
                 (Doc   => Data.Doc,
                  XPath => "/system/scheduling/majorFrame/cpu",
                  Name  => "id"),
              Message   => "Change to new document affected old document.");

--  begin read only
   end Test_Create_XMLDocument_From_Node;
--  end read only


--  begin read only
   procedure Test_Compile_Template (Gnattest_T : in out Test);
   procedure Test_Compile_Template_27bc9c (Gnattest_T : in out Test) renames Test_Compile_Template;
--  id:2.2/27bc9cb3b5e0901a/Compile_Template/1/0/
   procedure Test_Compile_Template (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      use Ada.Strings.Unbounded;

      Data                        : Muxml.XML_Data_Type;
      Template_Def, Template_Call : DOM.Core.Node;
      Compiled_Template           : Muxml.XML_Data_Type;
      Used_Prefix                 : Unbounded_String;
      Output_File_Name            : constant String
         := "obj/output_compiled_template.xml";

      procedure Set_Nodes
      is
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/system_policy_templateAmend_withEqualNames.xml");
         Template_Def := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/template[@name='template_memory']");
         Template_Call := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/useTemplate[@name='template_memory']");
      end Set_Nodes;

      procedure Positive_Test
      is
      begin
         Set_Nodes;

         Compile_Template
            (Template       => Template_Def,
             Template_Call  => Template_Call,
             Running_Number => 42,
             Output         => Compiled_Template,
             Used_Prefix    => Used_Prefix);
         Muxml.Write (Data => Compiled_Template,
                      Kind => Muxml.None,
                      File => Output_File_Name);

         Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => "data/output_compiled_template.xml",
                     Filename2 => Output_File_Name),
                 Message   => "Policy mismatch: " & Output_File_Name);
         Assert (Condition => To_String (Used_Prefix) = "t42_",
                 Message   => "Wrong prefix generated");

      end Positive_Test;

      procedure Missing_Parameter
      is
      begin
         Set_Nodes;
         Muxml.Utils.Remove_Elements
            (Doc   => Data.Doc,
             XPath => "/system/useTemplate[@name='template_memory']"
                & "/parameter[@name='id1']");

         Compile_Template
            (Template       => Template_Def,
             Template_Call  => Template_Call,
             Running_Number => 42,
             Output         => Compiled_Template,
             Used_Prefix    => Used_Prefix);
         Assert (Condition => False,
                 Message   => "Exception expected (missing parameter)");

      exception
         when E : Muxml.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Parameter 'id1' in template 'template_memory'"
                       & " has not been assigned by call.",
                    Message   => "Exception message mismatch (missing parameter)");
      end Missing_Parameter;

      procedure Missing_Value_Attribute
      is
         Node : DOM.Core.Node;
      begin
         Set_Nodes;
         Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/useTemplate[@name='template_memory']"
                & "/parameter[@name='id1']");
         DOM.Core.Elements.Remove_Attribute (Elem => Node, Name => "value");
         Compile_Template
            (Template       => Template_Def,
             Template_Call  => Template_Call,
             Running_Number => 42,
             Output         => Compiled_Template,
             Used_Prefix    => Used_Prefix);
         Assert (Condition => False,
                 Message   => "Exception expected (missing value attribute)");

      exception
         when E : Muxml.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Parameter 'id1' in call of template 'template_memory'"
                       & " has no value attribute.",
                    Message   => "Exception message mismatch "
                       & "(missing value attribute)");
      end Missing_Value_Attribute;

      procedure Too_Many_Parameters
      is
         Node, New_Node : DOM.Core.Node;
      begin
         Set_Nodes;
         Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/useTemplate[@name='template_memory']"
                & "/parameter[@name='id1']");
         New_Node := DOM.Core.Nodes.Clone_Node
            (N    => Node,
             Deep => True);
         New_Node := DOM.Core.Nodes.Append_Child
            (N         => DOM.Core.Nodes.Parent_Node (N => Node),
             New_Child => New_Node);
         DOM.Core.Elements.Set_Attribute
            (Elem  => New_Node,
             Name  => "name",
             Value => "copy_of_id1");

         Compile_Template
            (Template       => Template_Def,
             Template_Call  => Template_Call,
             Running_Number => 42,
             Output         => Compiled_Template,
             Used_Prefix    => Used_Prefix);
         Assert (Condition => False,
                 Message   => "Exception expected (missing value attribute)");

      exception
         when E : Muxml.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Parameter 'copy_of_id1' in call of template "
                       & "'template_memory' is unused",
                    Message   => "Exception message mismatch "
                       & "(unused parameter)");
      end Too_Many_Parameters;

      procedure Multiple_Config_Nodes_In_Template
      is
         Template, New_Node : DOM.Core.Node;
      begin
         Set_Nodes;
         Template := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/template[@name='template_memory']");

         New_Node := DOM.Core.Documents.Create_Element
            (Doc      => Data.Doc,
             Tag_Name => "config");
         New_Node := DOM.Core.Nodes.Insert_Before
            (N         => Template,
             New_Child => New_Node,
             Ref_Child => DOM.Core.Nodes.First_Child
                (N => Template));
         New_Node := DOM.Core.Documents.Create_Element
            (Doc      => Data.Doc,
             Tag_Name => "config");
         New_Node := DOM.Core.Nodes.Insert_Before
            (N         => Template,
             New_Child => New_Node,
             Ref_Child => DOM.Core.Nodes.First_Child
                (N => Template));

         Compile_Template
            (Template       => Template_Def,
             Template_Call  => Template_Call,
             Running_Number => 42,
             Output         => Compiled_Template,
             Used_Prefix    => Used_Prefix);
         Assert (Condition => False,
                 Message   => "Exception expected (missing value attribute)");

      exception
         when E : Muxml.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Found template with multiple 'config'-nodes.",
                    Message   => "Exception message mismatch "
                       & "(multiple config nodes)");
      end Multiple_Config_Nodes_In_Template;

   begin
      Positive_Test;
      Missing_Parameter;
      Missing_Value_Attribute;
      Too_Many_Parameters;
      Multiple_Config_Nodes_In_Template;

--  begin read only
   end Test_Compile_Template;
--  end read only


--  begin read only
   procedure Test_Prefix_Variables (Gnattest_T : in out Test);
   procedure Test_Prefix_Variables_8fbb3b (Gnattest_T : in out Test) renames Test_Prefix_Variables;
--  id:2.2/8fbb3bd221cbbff3/Prefix_Variables/1/0/
   procedure Test_Prefix_Variables (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Positive_Test
      is
         Data, Template_Doc : Muxml.XML_Data_Type;
         Output_File_Name   : constant String
                            := "obj/output_prefixed_template.xml";
         Locked_Attr        : Node_Set_Type.Set;

         Template_Def, Root_Node, Dummy, New_Config : DOM.Core.Node;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/system_policy_templateAmend.xml");
         -- prepare a new document to use
         Template_Def := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/template[@name='template_memory']");
         Create_XMLDocument_From_Node (New_Doc  => Template_Doc.Doc,
                                       Src_Node => Template_Def);
         Root_Node := Muxml.Utils.Get_Element
            (Doc   => Template_Doc.Doc,
             XPath => "/template");

         -- transform <parameters> to <config> within the new document
         New_Config := DOM.Core.Documents.Create_Element
            (Doc      => Template_Doc.Doc,
             Tag_Name => "config");
         New_Config := DOM.Core.Nodes.Insert_Before
            (N         => Root_Node,
             New_Child => New_Config,
             Ref_Child => DOM.Core.Nodes.First_Child
                (N => Root_Node));
         Adopt_All_Children
            (Target             => New_Config,
             Parent_Of_Children => Muxml.Utils.Get_Element
                (Doc   => Template_Doc.Doc,
                 XPath => "/template/parameters"),
             Append_Mode        => True);
         Dummy := DOM.Core.Nodes.Remove_Child
            (N         => Root_Node,
             Old_Child => Muxml.Utils.Get_Element
                (Doc   => Template_Doc.Doc,
                 XPath => "/template/parameters"));

         -- execute test
         Prefix_Variables (Root_Node   => Root_Node,
                           Config_Node => New_Config,
                           Prefix      => "xprex_",
                           Locked_Attr => Locked_Attr);
         Muxml.Write (Data => Template_Doc,
                      Kind => Muxml.None,
                      File => Output_File_Name);
         Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => "data/output_prefixed_template.xml",
                     Filename2 => Output_File_Name),
                 Message   => "Policy mismatch: " & Output_File_Name);
      end Positive_Test;
   begin
      Positive_Test;

--  begin read only
   end Test_Prefix_Variables;
--  end read only


--  begin read only
   procedure Test_Adopt_All_Children (Gnattest_T : in out Test);
   procedure Test_Adopt_All_Children_7d9f08 (Gnattest_T : in out Test) renames Test_Adopt_All_Children;
--  id:2.2/7d9f0836abd7dd4d/Adopt_All_Children/1/0/
   procedure Test_Adopt_All_Children (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Positive_Test
      is
         Data             : Muxml.XML_Data_Type;
         Target, Source   : DOM.Core.Node;
         Output_File_Name : constant String
            := "obj/output_adopt_children.xml";
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/system_policy_templateAmend.xml");
         -- append a deep tree
         Source := Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/subjects/subject[@name='lnx']");
         Target := Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/subjects/subject[@name='lnx2']");
         Adopt_All_Children
            (Target             => Target,
             Parent_Of_Children => Source,
             Append_Mode        => True);

         -- insert before target
         Source := Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/hardware/memory");
         Target := Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/template[@name='template_memory']"
                    & "/body/memory/memory");
         Adopt_All_Children
            (Target             => Target,
             Parent_Of_Children => Source,
             Append_Mode        => False);

         -- append empty list
         Source := Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/subjects/subject[@name='lnx']"
                    & "/memory/memory[@logical='extra_mem']");
         Adopt_All_Children
            (Target             => Target,
             Parent_Of_Children => Source,
             Append_Mode        => True);

         Muxml.Write (Data => Data,
                      Kind => Muxml.None,
                      File => Output_File_Name);
         Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => "data/output_adopt_children.xml",
                     Filename2 => Output_File_Name),
                 Message   => "Policy mismatch: " & Output_File_Name);
      end Positive_Test;

   begin
      Positive_Test;

--  begin read only
   end Test_Adopt_All_Children;
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
end Mutools.XML_Templates.Test_Data.Tests;
