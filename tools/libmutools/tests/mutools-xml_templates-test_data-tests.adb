--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.XML_Templates.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;
--with Ada.Directories;
--with Ada.Exceptions;
--with Test_Utils;

with DOM.Core.Nodes;
with DOM.Core.Documents;

with Muxml.Utils;
--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

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
   procedure Test_Expand_150aa9 (Gnattest_T : in out Test) renames Test_Expand;
--  id:2.2/150aa91f5cdabaeb/Expand/1/0/
   procedure Test_Expand (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

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
      Data, Output   : Muxml.XML_Data_Type;
      Node   : DOM.Core.Node;
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
   procedure Test_Compile_Template_f1bf84 (Gnattest_T : in out Test) renames Test_Compile_Template;
--  id:2.2/f1bf84b3080e4862/Compile_Template/1/0/
   procedure Test_Compile_Template (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Compile_Template;
--  end read only


--  begin read only
   procedure Test_Get_Decendent_Nodes_Match (Gnattest_T : in out Test);
   procedure Test_Get_Decendent_Nodes_Match_9e9a46 (Gnattest_T : in out Test) renames Test_Get_Decendent_Nodes_Match;
--  id:2.2/9e9a460086c3824a/Get_Decendent_Nodes_Match/1/0/
   procedure Test_Get_Decendent_Nodes_Match (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Decendent_Nodes_Match;
--  end read only


--  begin read only
   procedure Test_Prefix_Variables (Gnattest_T : in out Test);
   procedure Test_Prefix_Variables_57a903 (Gnattest_T : in out Test) renames Test_Prefix_Variables;
--  id:2.2/57a903e481fddf2c/Prefix_Variables/1/0/
   procedure Test_Prefix_Variables (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Prefix_Variables;
--  end read only


--  begin read only
   procedure Test_Adopt_All_Children (Gnattest_T : in out Test);
   procedure Test_Adopt_All_Children_94acb5 (Gnattest_T : in out Test) renames Test_Adopt_All_Children;
--  id:2.2/94acb53f75a618d4/Adopt_All_Children/1/0/
   procedure Test_Adopt_All_Children (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

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
