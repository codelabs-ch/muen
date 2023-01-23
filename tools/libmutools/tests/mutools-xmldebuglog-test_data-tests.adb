--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Xmldebuglog.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Ada.Strings.Fixed;
with GNAT.Regpat;
with Ada.Unchecked_Conversion;
with Interfaces;

with DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes;
with DOM.Core.Elements;

with Muxml;
with Muxml.Utils;
with Mutools.XML_Templates;

--  begin read only
--  end read only
package body Mutools.Xmldebuglog.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only


--  begin read only
--  end read only

--  begin read only
   procedure Test_Move_Origin_To_Log (Gnattest_T : in out Test);
   procedure Test_Move_Origin_To_Log_7f3a53 (Gnattest_T : in out Test) renames Test_Move_Origin_To_Log;
--  id:2.2/7f3a533fdafd20c7/Move_Origin_To_Log/1/0/
   procedure Test_Move_Origin_To_Log (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Data : Muxml.XML_Data_Type;
   begin
      Clear_Transaction_Log;
      Clear_Backtrace_Log;

      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_short.xml",
                   Add_Location => True);
      Move_Origin_To_Log (Doc => Data.Doc);

      Assert (Condition => 6 = Integer (Nodes_Backtrace_Log_Type.Length
                 (Container => Nodes_Backtrace_Log)),
              Message   => "Length of log not 6: "
                 & Integer (Nodes_Backtrace_Log_Type.Length
                 (Container => Nodes_Backtrace_Log))'Img);
      Assert (Condition => Nodes_Backtrace_Log.Contains
                 (Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system"))
                 and Nodes_Backtrace_Log.Contains
                 (Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/config"))
                 and Nodes_Backtrace_Log.Contains
                 (Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system"))
                 and Nodes_Backtrace_Log.Contains
                 (Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/config/integer"))
                 and Nodes_Backtrace_Log.Contains
                 (Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/config/string"))
                 and Nodes_Backtrace_Log.Contains
                 (Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/subjects"))
                 and Nodes_Backtrace_Log.Contains
                 (Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/subjects/subject")),
              Message => "Some elements were not contained");
      declare
         Origin_Info : Origin_Info_Type
            := Nodes_Backtrace_Log
            (Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/config/string")).Origin_Of_Node;
      begin
         Assert (Condition => Origin_Info.File_Name.Element
                    = "system_policy_short.xml"
                    and Origin_Info.Line = 4
                    and Origin_Info.Column = 93,
                 Message => "Content of origin_info is not correct: "
                    & Node_Backtrace_To_String
                    (Muxml.Utils.Get_Element
                        (Doc   => Data.Doc,
                         XPath => "/system/config/string")));
      end;
--  begin read only
   end Test_Move_Origin_To_Log;
--  end read only


--  begin read only
   procedure Test_Add_Debug_Infos_As_Comments (Gnattest_T : in out Test);
   procedure Test_Add_Debug_Infos_As_Comments_f9d371 (Gnattest_T : in out Test) renames Test_Add_Debug_Infos_As_Comments;
--  id:2.2/f9d371565bf5b090/Add_Debug_Infos_As_Comments/1/0/
   procedure Test_Add_Debug_Infos_As_Comments (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Data : Muxml.XML_Data_Type;

      function Check_Node
         (Data           : Muxml.XML_Data_Type;
          XPath, Comment : String)
         return Boolean
      is
         use DOM.Core;
         use type DOM.Core.Node;

         Node      : DOM.Core.Node
            := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => XPath);
         Node_Prev : DOM.Core.Node
            := DOM.Core.Nodes.Previous_Sibling (N => Node);
      begin
         if Node = null or Node_Prev = null then
            return False;
         elsif DOM.Core.Nodes.Node_Type (N => Node) /= Element_Node or
            DOM.Core.Nodes.Node_Type (N => Node_Prev) /= Comment_Node
         then
            return False;
         end if;

         return GNAT.Regpat.Match
            (Expression => Comment,
             Data       => DOM.Core.Nodes.Node_Value (N => Node_Prev));
      end Check_Node;

   begin
      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => True);
      Merge_All_Steps (Data => Data);
      Add_Debug_Infos_As_Comments (Doc => Data.Doc);

      Assert (Condition => Check_Node
                 (Data    => Data,
                  XPath   => "/system/hardware/memory/memoryBlock[@name='amended_block']",
                  Comment => "Node_Name='memoryBlock', "
                     & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=32, Column=120\), "
                     & "Transaction\(Kind='amend', "
                     & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=31, Column=52\), "
                     & "Xpath='/system/hardware/memory'\), "
                     & "Transaction\(Kind='if', "
                     & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=29, Column=47\), "
                     & "Var_Name='no_caching', Var_Value='UC', "
                     & "Matched=TRUE, Matched_Others=FALSE\), "
                     & "Transaction\(Kind='useTemplate', "
                     & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=92, Column=40\),  "
                     & "Template_Name='template_memory', "
                     & "Call_Parameters\(id1='20', memory_name='extra_mem'\), Prefix='(t[0-9]*_)*'\)"),
              Message   => "Comment mismatch");

      Assert (Condition => Check_Node
                 (Data    => Data,
                  XPath   => "/system/subjects/subject[@name='lnx']/memory/memory[@name='conditional_mem']",
                  Comment =>"Node_Name='memory', "
                     & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', "
                     & "Line=128, Column=112\), Transaction\(Kind='case', "
                     & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', "
                     & "Line=118, Column=45\), Var_Name='sizeOfExtraMem', "
                     & "Var_Value='16#0000_0000#', Matched=TRUE, Matched_Others=TRUE\)"),
              Message   => "Comment mismatch");
--  begin read only
   end Test_Add_Debug_Infos_As_Comments;
--  end read only


--  begin read only
   procedure Test_Add_Transaction_Log_As_Comment (Gnattest_T : in out Test);
   procedure Test_Add_Transaction_Log_As_Comment_bd868b (Gnattest_T : in out Test) renames Test_Add_Transaction_Log_As_Comment;
--  id:2.2/bd868be90bc940e3/Add_Transaction_Log_As_Comment/1/0/
   procedure Test_Add_Transaction_Log_As_Comment (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;

   begin
      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => True);
      Merge_All_Steps (Data => Data);
      Add_Transaction_Log_As_Comment (Doc => Data.Doc);

      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system");
      Node := DOM.Core.Nodes.Previous_Sibling (N => Node);
      Assert (Condition => Node /= null,
              Message   => "Transaction log not found in document");

      declare
         use Ada.Strings.Fixed;
         Log_Text : constant String
            := DOM.Core.Nodes.Node_Value (N => Node);
      begin
         Assert (Condition => 0 < Index (Log_Text, "Transaction_Log"),
                 Message   => "Mismatch of log message");
         Assert (Condition => 0 < Index (Log_Text,
            "Transaction(Kind='useTemplate', "
            & "Node_Origin=(Filename='system_policy_for_xmldebuglog.xml', Line=67, Column=55),  "
            & "Template_Name='oneline_mem_template', Call_Parameters()"),
                 Message   => "Mismatch of log message");
         Assert (Condition => 0 < Index (Log_Text,
            "Transaction(Kind='useTemplate', "
            & "Node_Origin=(Filename='system_policy_for_xmldebuglog.xml', Line=92, Column=40),  "
            & "Template_Name='template_memory', Call_Parameters(id1='20', memory_name='extra_mem')"),
                 Message   => "Mismatch of log message");
         Assert (Condition => 0 < Index (Log_Text,
            "Transaction(Kind='if', "
            & "Node_Origin=(Filename='system_policy_for_xmldebuglog.xml', Line=24, Column=50)"),
                 Message   => "Mismatch of log message");
         Assert (Condition => 0 < Index (Log_Text,
            "Transaction(Kind='amend', "
            & "Node_Origin=(Filename='system_policy_for_xmldebuglog.xml', Line=110, Column=52), "
            & "Xpath='/system/hardware/memory')"),
                 Message   => "Mismatch of log message");
         end;
--  begin read only
   end Test_Add_Transaction_Log_As_Comment;
--  end read only


--  begin read only
   procedure Test_Get_Log_For_Error_Message (Gnattest_T : in out Test);
   procedure Test_Get_Log_For_Error_Message_5e1fe2 (Gnattest_T : in out Test) renames Test_Get_Log_For_Error_Message;
--  id:2.2/5e1fe2082a6d1c13/Get_Log_For_Error_Message/1/0/
   procedure Test_Get_Log_For_Error_Message (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;

   begin
      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => True);
      Clear_Transaction_Log;
      Clear_Backtrace_Log;
      Mutools.XML_Templates.Expand (XML_Data     => Data,
                                    Debug_Active => True);
      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/memory/if/memory");

      Assert (Condition => GNAT.Regpat.Match
                 (Expression => "^Log information for debugging: Node_Name='memory', "
                 & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=25, Column=94\), "
                 & "Transaction\(Kind='useTemplate', "
                 & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=92, Column=40\),  "
                 & "Template_Name='template_memory', "
                     & "Call_Parameters\(id1='20', memory_name='extra_mem'\), Prefix='(t[0-9]*_)*'\)$",
                  Data => Get_Log_For_Error_Message (Node => Node)),
                 Message => "Log message mismatch: "
                 & Get_Log_For_Error_Message (Node => Node));
--  begin read only
   end Test_Get_Log_For_Error_Message;
--  end read only


--  begin read only
   procedure Test_Get_Xpath (Gnattest_T : in out Test);
   procedure Test_Get_Xpath_b6823f (Gnattest_T : in out Test) renames Test_Get_Xpath;
--  id:2.2/b6823f56c83155ed/Get_Xpath/1/0/
   procedure Test_Get_Xpath (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => True);
      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/template[@name='template_memory']/body/memory/if");
      Assert (Condition => Get_Xpath (Node => Node)
                 = "/#document/system/template/body/memory/if",
              Message   => "XPath mismatch: " & Get_Xpath (Node => Node));
--  begin read only
   end Test_Get_Xpath;
--  end read only


--  begin read only
   procedure Test_Add_Usetemplate_Transaction (Gnattest_T : in out Test);
   procedure Test_Add_Usetemplate_Transaction_5ed628 (Gnattest_T : in out Test) renames Test_Add_Usetemplate_Transaction;
--  id:2.2/5ed62807a800c369/Add_Usetemplate_Transaction/1/0/
   procedure Test_Add_Usetemplate_Transaction (Gnattest_T : in out Test) is
--  end read only

      use type Ada.Containers.Count_Type;
      pragma Unreferenced (Gnattest_T);
      Data  : Muxml.XML_Data_Type;
      Node  : DOM.Core.Node;
      Index : Transaction_Log_Index_Type;
   begin
      Clear_Transaction_Log;
      Assert (Condition => Transaction_Log.Length = 0,
              Message   => "Test setup error - log not empty");

      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => True);
      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/useTemplate[@name='template_memory']");
      Index := Add_Usetemplate_Transaction
         (Usetemplate_Node => Node,
          Prefix           => "some_prefix");
      Assert (Condition => Transaction_Log.Length = 1,
              Message   => "Log length not equal 1");

      Index := Add_Usetemplate_Transaction
         (Usetemplate_Node => Node,
          Prefix           => "some_prefix");
      Assert (Condition => Transaction_Log.Length = 2,
              Message   => "Log length not equal 2");
      Assert (Condition => Transaction_To_String (TA => Transaction_Log.Last_Element)
                 = "Transaction(Kind='useTemplate', "
                 & "Node_Origin=(Filename='system_policy_for_xmldebuglog.xml', Line=92, Column=40),  "
                 & "Template_Name='template_memory', Call_Parameters(id1='20', memory_name='extra_mem'), "
                 & "Prefix='some_prefix')",
              Message   => "Transaction Log mismatch: " & Transaction_Log_To_String);
--  begin read only
   end Test_Add_Usetemplate_Transaction;
--  end read only


--  begin read only
   procedure Test_Add_Conditional_Transaction (Gnattest_T : in out Test);
   procedure Test_Add_Conditional_Transaction_1af119 (Gnattest_T : in out Test) renames Test_Add_Conditional_Transaction;
--  id:2.2/1af1195c87211879/Add_Conditional_Transaction/1/0/
   procedure Test_Add_Conditional_Transaction (Gnattest_T : in out Test) is
--  end read only

      use type Ada.Containers.Count_Type;
      pragma Unreferenced (Gnattest_T);
      Data  : Muxml.XML_Data_Type;
      Node  : DOM.Core.Node;
      Index : Transaction_Log_Index_Type;
   begin
      Clear_Transaction_Log;
      Assert (Condition => Transaction_Log.Length = 0,
              Message   => "Test setup error - log not empty");

      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => True);
      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/subjects/subject[@name='lnx']/memory/case");
      Index := Add_Conditional_Transaction
         (Conditional_Node => Node,
          Coditional_Kind  => CASECOND,
          Var_Name         => "sizeOfExtraMem",
          Var_Value        => "some value",
          Matched          => False,
          Matched_Others   => False);
      Assert (Condition => Transaction_Log.Length = 1,
              Message   => "Log length not equal 1");

      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/template[@name='big_dev_template']/body/if");
      Index := Add_Conditional_Transaction
         (Conditional_Node => Node,
          Coditional_Kind  => IFCOND,
          Var_Name         => "param1",
          Var_Value        => "555",
          Matched          => True,
          Matched_Others   => True);
      Assert (Condition => Transaction_Log.Length = 2,
              Message   => "Log length not equal 2");
      Assert (Condition => Transaction_To_String (TA => Transaction_Log.Last_Element)
                 = "Transaction(Kind='if', "
                 & "Node_Origin=(Filename='system_policy_for_xmldebuglog.xml', Line=42, Column=43), "
                 & "Var_Name='param1', Var_Value='555', Matched=TRUE, Matched_Others=TRUE)",
              Message   => "Transaction Log mismatch: " & Transaction_Log_To_String);
--  begin read only
   end Test_Add_Conditional_Transaction;
--  end read only


--  begin read only
   procedure Test_Add_Amend_Transaction (Gnattest_T : in out Test);
   procedure Test_Add_Amend_Transaction_4aca65 (Gnattest_T : in out Test) renames Test_Add_Amend_Transaction;
--  id:2.2/4aca65e6844ff1ba/Add_Amend_Transaction/1/0/
   procedure Test_Add_Amend_Transaction (Gnattest_T : in out Test) is
--  end read only

      use type Ada.Containers.Count_Type;
      pragma Unreferenced (Gnattest_T);
      Data  : Muxml.XML_Data_Type;
      Node  : DOM.Core.Node;
      Index : Transaction_Log_Index_Type;
   begin
      Clear_Transaction_Log;
      Assert (Condition => Transaction_Log.Length = 0,
              Message   => "Test setup error - log not empty");
      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => True);
      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/subjects/subject[@name='lnx']/memory/amend");
      Index := Add_Amend_Transaction
         (Amend_Node => Node,
          Xpath      => "some string...");
      Assert (Condition => Transaction_Log.Length = 1,
              Message   => "Log length not equal 1");

      Index := Add_Amend_Transaction
         (Amend_Node => Node,
          Xpath      => "/any/path/I/like[@name='123']");
      Assert (Condition => Transaction_Log.Length = 2,
              Message   => "Log length not equal 2");
      Assert (Condition => Transaction_To_String (TA => Transaction_Log.Last_Element)
                 = "Transaction(Kind='amend', "
                 & "Node_Origin=(Filename='system_policy_for_xmldebuglog.xml', Line=110, Column=52), "
                 & "Xpath='/any/path/I/like[@name='123']')",
              Message   => "Transaction Log mismatch: " & Transaction_Log_To_String);
--  begin read only
   end Test_Add_Amend_Transaction;
--  end read only


--  begin read only
   procedure Test_Add_Log_For_Node (Gnattest_T : in out Test);
   procedure Test_Add_Log_For_Node_2de797 (Gnattest_T : in out Test) renames Test_Add_Log_For_Node;
--  id:2.2/2de797922ebbc5f2/Add_Log_For_Node/1/0/
   procedure Test_Add_Log_For_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => True);
      Merge_All_Steps (Data => Data);
      Node := DOM.Core.Documents.Create_Element
         (Doc      => Data.Doc,
          Tag_Name => "foobar");
      Muxml.Utils.Append_Child
         (Node      => Data.Doc,
          New_Child => Node);

      Add_Log_For_Node
         (Node      => Node,
          Ancestor  => Muxml.Utils.Get_Element
             (Doc   => Data.Doc,
              XPath => "/system/memory/memory[@name='dummy']"),
          TA_Number => Transaction_Log.First_Index);

      Assert (Condition => GNAT.Regpat.Match
                 (Expression => "Node_Name='foobar', "
                     & "Node_Origin=\(Filename='', Line=0, Column=0\), "
                     & "Transaction\(Kind='if', "
                     & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=24, Column=50\), "
                     & "Var_Name='(t[0-9]*_)*isId1_20', Var_Value='true', Matched=TRUE, Matched_Others=FALSE\), "
                     & "Transaction\(Kind='useTemplate', "
                     & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=67, Column=55\),  "
                     & "Template_Name='oneline_mem_template', Call_Parameters\(\), Prefix='(t[0-9]*_)*'\)",
                  Data => Node_Backtrace_To_String (Node => Node)),
              Message   => "Log mismatch: " & Node_Backtrace_To_String (Node => Node));
--  begin read only
   end Test_Add_Log_For_Node;
--  end read only


--  begin read only
   procedure Test_Remove_Log_Of_Node (Gnattest_T : in out Test);
   procedure Test_Remove_Log_Of_Node_95a50e (Gnattest_T : in out Test) renames Test_Remove_Log_Of_Node;
--  id:2.2/95a50e3335ff3a30/Remove_Log_Of_Node/1/0/
   procedure Test_Remove_Log_Of_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => True);
      Merge_All_Steps (Data => Data);
      Node := Muxml.Utils.Get_Element
             (Doc   => Data.Doc,
              XPath => "/system/memory/memory[@name='dummy']");
      Assert (Condition => Nodes_Backtrace_Log.Contains (Node),
              Message   => "Error is test setup");
      Remove_Log_Of_Node (Node => Node);
      Assert (Condition => not Nodes_Backtrace_Log.Contains (Node),
              Message   => "Node was not removed");
--  begin read only
   end Test_Remove_Log_Of_Node;
--  end read only


--  begin read only
   procedure Test_Remove_Log_Of_Subtree (Gnattest_T : in out Test);
   procedure Test_Remove_Log_Of_Subtree_4dcc81 (Gnattest_T : in out Test) renames Test_Remove_Log_Of_Subtree;
--  id:2.2/4dcc8116fa69973e/Remove_Log_Of_Subtree/1/0/
   procedure Test_Remove_Log_Of_Subtree (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Node : DOM.Core.Node;
      Data : Muxml.XML_Data_Type;

   begin
      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => True);
      Merge_All_Steps (Data => Data);
      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/hardware");

      Assert (Condition => Nodes_Backtrace_Log.Contains (Node),
              Message   => "Error in test setup");

      Remove_Log_Of_Subtree
         (Node =>  Muxml.Utils.Get_Element
             (Doc   => Data.Doc,
              XPath => "/system"),
          XPath => "hardware");
      Assert (Condition => not Nodes_Backtrace_Log.Contains (Node),
              Message   => "Log has not been removed");
      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/hardware/memory/memoryBlock");
      Assert (Condition => not Nodes_Backtrace_Log.Contains (Node),
              Message   => "Log has not been removed");
--  begin read only
   end Test_Remove_Log_Of_Subtree;
--  end read only


--  begin read only
   procedure Test_Copy_Log_Entry (Gnattest_T : in out Test);
   procedure Test_Copy_Log_Entry_34228a (Gnattest_T : in out Test) renames Test_Copy_Log_Entry;
--  id:2.2/34228ac956e75032/Copy_Log_Entry/1/0/
   procedure Test_Copy_Log_Entry (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Origin, Origin_Child1, Origin_Child2 : DOM.Core.Node;
      Target, Target_Child1, Target_Child2 : DOM.Core.Node;
      Data : Muxml.XML_Data_Type;

   begin
      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => True);
      Merge_All_Steps (Data => Data);
      Origin := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/memory");
      Origin_Child1 := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/memory/memory[@name='dummy']");
      Origin_Child2 := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/memory/memory[@name='extra_mem']");

      Target := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/subjects/subject[@name='lnx2']/memory");
      Target_Child1 := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/subjects/subject[@name='lnx2']/memory/memory[@name='dummy2']");
      Target_Child2 := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/subjects/subject[@name='lnx2']/memory/memory[@name='extra_mem2']");
      Assert (Condition => Nodes_Backtrace_Log (Origin) /= Nodes_Backtrace_Log(Target)
                 and Nodes_Backtrace_Log (Origin_Child1) /= Nodes_Backtrace_Log(Target_Child1)
                 and Nodes_Backtrace_Log (Origin_Child2) /= Nodes_Backtrace_Log(Target_Child2),
              Message   => "Error in test setup");

      Copy_Log_Entry (Old_Node => Origin, New_Node => Target, Deep => True);
      Assert (Condition => Nodes_Backtrace_Log (Origin) = Nodes_Backtrace_Log(Target)
                 and Nodes_Backtrace_Log (Origin_Child1) = Nodes_Backtrace_Log(Target_Child1)
                 and Nodes_Backtrace_Log (Origin_Child2) = Nodes_Backtrace_Log(Target_Child2),
              Message   => "Error in test setup");
--  begin read only
   end Test_Copy_Log_Entry;
--  end read only


--  begin read only
   procedure Test_Gather_Backtrace_Info (Gnattest_T : in out Test);
   procedure Test_Gather_Backtrace_Info_4146bd (Gnattest_T : in out Test) renames Test_Gather_Backtrace_Info;
--  id:2.2/4146bde43f5399a1/Gather_Backtrace_Info/1/0/
   procedure Test_Gather_Backtrace_Info (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Node, Node_Child : DOM.Core.Node;
      Data             : Muxml.XML_Data_Type;
      Node_Log         : Node_Backtrace_Type;
   begin
      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => True);
      Clear_Transaction_Log;
      Clear_Backtrace_Log;
      Mutools.XML_Templates.Expand (XML_Data     => Data,
                                    Debug_Active => True);
      Mutools.Xmldebuglog.Move_Origin_To_Log (Doc => Data.Doc);

      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/memory/if");
      Node_Child :=  Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/memory/if/memory");

      Assert (Condition => Node_Backtrace_To_String (Node => Node)
                 = "Node_Name='if', "
                 & "Node_Origin=(Filename='system_policy_for_xmldebuglog.xml', Line=24, Column=50)",
              Message   => "Log message mismatch: "
                 & Node_Backtrace_To_String (Node => Node));
      Assert (Condition => Node_Backtrace_To_String (Node => Node_Child)
                 = "Node_Name='memory', "
                 & "Node_Origin=(Filename='system_policy_for_xmldebuglog.xml', Line=25, Column=94)",
              Message   => "Log message mismatch: "
                 & Node_Backtrace_To_String (Node => Node_Child));

      Node_Log := Nodes_Backtrace_Log (Key => Node_Child);
      Gather_Backtrace_Info (Node => Node, Deep => False);
      Assert (Condition => GNAT.Regpat.Match
                 (Expression => "^Node_Name='if', "
                     & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=24, Column=50\), "
                     & "Transaction\(Kind='useTemplate', "
                     & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=92, Column=40\),  "
                     & "Template_Name='template_memory', "
                     & "Call_Parameters\(id1='20', memory_name='extra_mem'\), Prefix='(t[0-9]*_)*'\)$",
                  Data => Node_Backtrace_To_String (Node => Node)),
                 Message => "Log message mismatch: "
                 & Node_Backtrace_To_String (Node => Node));
      -- log of Node_Child should not change
      Assert (Condition => Nodes_Backtrace_Log (Key => Node_Child) = Node_Log,
              Message   => "Log message mismatch: "
                 & Node_Backtrace_To_String (Node => Node_Child));

      Node_Log := Nodes_Backtrace_Log (Key => Node);
      Gather_Backtrace_Info (Node => Node, Deep => True);
      -- log for Node should not change
      Assert (Condition => Nodes_Backtrace_Log (Key => Node) = Node_Log,
              Message => "Log message mismatch: "
                 & Node_Backtrace_To_String (Node => Node));
      Assert (Condition => GNAT.Regpat.Match
                 (Expression => "^Node_Name='memory', "
                     & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=25, Column=94\), "
                     & "Transaction\(Kind='useTemplate', "
                     & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=92, Column=40\),  "
                     & "Template_Name='template_memory', "
                     & "Call_Parameters\(id1='20', memory_name='extra_mem'\), Prefix='(t[0-9]*_)*'\)$",
                  Data => Node_Backtrace_To_String (Node => Node_Child)),
                 Message => "Log message mismatch: "
                 & Node_Backtrace_To_String (Node => Node_Child));
--  begin read only
   end Test_Gather_Backtrace_Info;
--  end read only


--  begin read only
   procedure Test_Hash (Gnattest_T : in out Test);
   procedure Test_Hash_1a4149 (Gnattest_T : in out Test) renames Test_Hash;
--  id:2.2/1a4149a6a5bbe8f9/Hash/1/0/
   procedure Test_Hash (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      use type Ada.Containers.Hash_Type;

      function Unsigned_64_To_Node is new Ada.Unchecked_Conversion
      (Source => Interfaces.Unsigned_64,
       Target => DOM.Core.Node);

      function Unsigned_32_To_Hash is new Ada.Unchecked_Conversion
         (Source => Interfaces.Unsigned_32,
          Target => Ada.Containers.Hash_Type);

      Fake_Node : DOM.Core.Node;
   begin
      Fake_Node := Unsigned_64_To_Node (16#1100_0110_0011_0101#);
      Assert (Condition => Hash (Fake_Node) = Unsigned_32_To_Hash (16#1111_0011#),
              Message   => "Hash is not correct: " & Hash (Fake_Node)'Img);
--  begin read only
   end Test_Hash;
--  end read only


--  begin read only
   procedure Test_Origin_To_String (Gnattest_T : in out Test);
   procedure Test_Origin_To_String_cdeaf7 (Gnattest_T : in out Test) renames Test_Origin_To_String;
--  id:2.2/cdeaf7034e6348c5/Origin_To_String/1/0/
   procedure Test_Origin_To_String (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Origin : Origin_Info_Type;
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => True);
      Merge_All_Steps (Data => Data);
      Origin := Nodes_Backtrace_Log
         (Key => Muxml.Utils.Get_Element
             (Doc   => Data.Doc,
              Xpath => "/system/hardware")).Origin_Of_Node;
      Assert (Condition => Origin_To_String (Origin => Origin)
                 = "Node_Origin=(Filename='system_policy_for_xmldebuglog.xml', Line=60, Column=14)",
              Message   => "Origin string mismatch: "
                 & Origin_To_String (Origin => Origin));
--  begin read only
   end Test_Origin_To_String;
--  end read only


--  begin read only
   procedure Test_Transaction_To_String (Gnattest_T : in out Test);
   procedure Test_Transaction_To_String_543271 (Gnattest_T : in out Test) renames Test_Transaction_To_String;
--  id:2.2/5432713bdf4a2e4f/Transaction_To_String/1/0/
   procedure Test_Transaction_To_String (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Data : Muxml.XML_Data_Type;
      NB   : Node_Backtrace_Type;
      TA   : Transaction_Type (Transaction_Kind => CONDITIONAL);
   begin
      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => True);
      Merge_All_Steps (Data => Data);

      NB := Nodes_Backtrace_Log
         (Key => Muxml.Utils.Get_Element
             (Doc   => Data.Doc,
              Xpath => "/system/subjects/subject[@name='lnx2']/component/map"));
      TA := Transaction_Log (NB.Conditional_Backtrace.Entries (1));
      Assert (Condition => Transaction_To_String (TA)
                 = "Transaction(Kind='if', "
                 & "Node_Origin=(Filename='system_policy_for_xmldebuglog.xml', Line=145, Column=52), "
                 & "Var_Name='session_count', Var_Value='4', Matched=TRUE, Matched_Others=FALSE)",
              Message   => "Transaction string mismatch: "
                 & Transaction_To_String (TA));
--  begin read only
   end Test_Transaction_To_String;
--  end read only


--  begin read only
   procedure Test_Parse_Origin_Attribute (Gnattest_T : in out Test);
   procedure Test_Parse_Origin_Attribute_8d4892 (Gnattest_T : in out Test) renames Test_Parse_Origin_Attribute;
--  id:2.2/8d489214027dd6fc/Parse_Origin_Attribute/1/0/
   procedure Test_Parse_Origin_Attribute (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => False);
      Node := Muxml.Utils.Get_Element
             (Doc   => Data.Doc,
              Xpath => "/system");
      DOM.Core.Elements.Set_Attribute (Elem => Node,
                                       Name => "originOfNode",
                                       Value => "filename:123:0");
      Assert (Condition => Parse_Origin_Attribute (Node)
                 = Origin_Info_Type'
                 (File_Name => String_Holder_Type.To_Holder ("filename"),
                  Line      => 123,
                  Column    => 0),
              Message   => "Parsing error for Origin: "
                 & Origin_To_String (Parse_Origin_Attribute (Node)));

      DOM.Core.Elements.Set_Attribute (Elem => Node,
                                       Name => "originOfNode",
                                       Value => "filename:new:123:0");
      Assert (Condition => Parse_Origin_Attribute (Node)
                 = Null_Origin_Info,
              Message   => "Parsing error for Origin: "
                 & Origin_To_String (Parse_Origin_Attribute (Node)));
--  begin read only
   end Test_Parse_Origin_Attribute;
--  end read only


--  begin read only
   procedure Test_Node_Backtrace_To_String (Gnattest_T : in out Test);
   procedure Test_Node_Backtrace_To_String_59f76b (Gnattest_T : in out Test) renames Test_Node_Backtrace_To_String;
--  id:2.2/59f76b1a8870d7cd/Node_Backtrace_To_String/1/0/
   procedure Test_Node_Backtrace_To_String (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Muxml.Parse
         (Data         => Data,
          Kind         => Muxml.None,
          File         => "data/system_policy_for_xmldebuglog.xml",
          Add_Location => True);
      Merge_All_Steps (Data => Data);
      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          Xpath => "/system/hardware/memory/memoryBlock[@name='amended_block']");

      Assert (Condition => GNAT.Regpat.Match
                 (Expression => "Node_Name='memoryBlock', "
                     & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=32, Column=120\), "
                     & "Transaction\(Kind='amend', "
                     & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=31, Column=52\), "
                     & "Xpath='/system/hardware/memory'\), "
                     & "Transaction\(Kind='if', "
                     & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=29, Column=47\), "
                     & "Var_Name='no_caching', Var_Value='UC', "
                     & "Matched=TRUE, Matched_Others=FALSE\), "
                     & "Transaction\(Kind='useTemplate', "
                     & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=92, Column=40\),  "
                     & "Template_Name='template_memory', "
                     & "Call_Parameters\(id1='20', memory_name='extra_mem'\), Prefix='(t[0-9]*_)*'\)",
                  Data   => Node_Backtrace_To_String (Node)),
              Message   => "String mismatch: " & Node_Backtrace_To_String (Node));
--  begin read only
   end Test_Node_Backtrace_To_String;
--  end read only


--  begin read only
   procedure Test_Node_Backtrace_Log_To_String (Gnattest_T : in out Test);
   procedure Test_Node_Backtrace_Log_To_String_2a0bef (Gnattest_T : in out Test) renames Test_Node_Backtrace_Log_To_String;
--  id:2.2/2a0befa8bc8d8a10/Node_Backtrace_Log_To_String/1/0/
   procedure Test_Node_Backtrace_Log_To_String (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => True);
      Merge_All_Steps (Data => Data);
      declare
         Log_Text : constant String
            := Node_Backtrace_Log_To_String;
      begin
         Assert (Condition => GNAT.Regpat.Match
                    (Expression => "\(Node_Name='memory', "
            & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=45, Column=101\), "
            & "Transaction\(Kind='amend', "
            & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=43, Column=53\), "
            & "Xpath='/system/hardware/devices'\), Transaction\(Kind='if', "
            & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=42, Column=43\), "
            & "Var_Name='(t[0-9]*_)*param1', Var_Value='22', Matched=TRUE, Matched_Others=FALSE\), "
            & "Transaction\(Kind='useTemplate', Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', "
            & "Line=69, Column=47\),  Template_Name='big_dev_template', Call_Parameters\(param1='22'\),",
                     Data => Log_Text),
                 Message   => "Mismatch of log message");

        Assert (Condition => GNAT.Regpat.Match
                   (Expression => "\(Node_Name='components', "
            & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=97, Column=16\)\)",
                    Data => Log_Text),
                Message   => "Mismatch of log message");

        Assert (Condition => GNAT.Regpat.Match
                   (Expression => "\(Node_Name='memory', "
            & "Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=128, Column=112\), "
            & "Transaction\(Kind='case', Node_Origin=\(Filename='system_policy_for_xmldebuglog\.xml', Line=118, Column=45\), "
            & "Var_Name='sizeOfExtraMem', Var_Value='16#0000_0000#', Matched=TRUE, Matched_Others=TRUE\)\)",
                    Data => Log_Text),
                Message   => "Mismatch of log message");
      end;
--  begin read only
   end Test_Node_Backtrace_Log_To_String;
--  end read only


--  begin read only
   procedure Test_Transaction_Log_To_String (Gnattest_T : in out Test);
   procedure Test_Transaction_Log_To_String_c3bdb3 (Gnattest_T : in out Test) renames Test_Transaction_Log_To_String;
--  id:2.2/c3bdb3608d92c97a/Transaction_Log_To_String/1/0/
   procedure Test_Transaction_Log_To_String (Gnattest_T : in out Test) is
--  end read only

      use Ada.Strings.Fixed;
      pragma Unreferenced (Gnattest_T);
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => True);
      Merge_All_Steps (Data => Data);
      declare
         Log_Text : constant String
            := Transaction_Log_To_String;
      begin
         Assert (Condition => 0 < Index (Log_Text,
            "Transaction(Kind='useTemplate', "
            & "Node_Origin=(Filename='system_policy_for_xmldebuglog.xml', Line=67, Column=55),  "
            & "Template_Name='oneline_mem_template', Call_Parameters()"),
                 Message   => "Mismatch of log message");
         Assert (Condition => 0 < Index (Log_Text,
            "Transaction(Kind='useTemplate', "
            & "Node_Origin=(Filename='system_policy_for_xmldebuglog.xml', Line=92, Column=40),  "
            & "Template_Name='template_memory', Call_Parameters(id1='20', memory_name='extra_mem')"),
                 Message   => "Mismatch of log message");
         Assert (Condition => 0 < Index (Log_Text,
            "Transaction(Kind='if', "
            & "Node_Origin=(Filename='system_policy_for_xmldebuglog.xml', Line=24, Column=50)"),
                 Message   => "Mismatch of log message");
         Assert (Condition => 0 < Index (Log_Text,
            "Transaction(Kind='amend', "
            & "Node_Origin=(Filename='system_policy_for_xmldebuglog.xml', Line=110, Column=52), "
            & "Xpath='/system/hardware/memory')"),
                 Message   => "Mismatch of log message");
         end;
--  begin read only
   end Test_Transaction_Log_To_String;
--  end read only


--  begin read only
   procedure Test_Clear_Transaction_Log (Gnattest_T : in out Test);
   procedure Test_Clear_Transaction_Log_53378e (Gnattest_T : in out Test) renames Test_Clear_Transaction_Log;
--  id:2.2/53378ea54a185b7c/Clear_Transaction_Log/1/0/
   procedure Test_Clear_Transaction_Log (Gnattest_T : in out Test) is
--  end read only

      use type Ada.Containers.Count_Type;
      pragma Unreferenced (Gnattest_T);
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => True);
      Merge_All_Steps (Data => Data);
      Assert (Condition => Transaction_Log.Length > 0,
              Message   => "Error in test setup");

      Clear_Transaction_Log;
      Assert (Condition => Transaction_Log.Length = 0,
              Message   => "Transaction log not cleared");
--  begin read only
   end Test_Clear_Transaction_Log;
--  end read only


--  begin read only
   procedure Test_Clear_Backtrace_Log (Gnattest_T : in out Test);
   procedure Test_Clear_Backtrace_Log_556164 (Gnattest_T : in out Test) renames Test_Clear_Backtrace_Log;
--  id:2.2/556164b199fc8b2f/Clear_Backtrace_Log/1/0/
   procedure Test_Clear_Backtrace_Log (Gnattest_T : in out Test) is
--  end read only

      use type Ada.Containers.Count_Type;
      pragma Unreferenced (Gnattest_T);
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.None,
                   File         => "data/system_policy_for_xmldebuglog.xml",
                   Add_Location => True);
      Merge_All_Steps (Data => Data);
      Assert (Condition => Nodes_Backtrace_Log.Length > 0,
              Message   => "Error in test setup");

      Clear_Backtrace_Log;
      Assert (Condition => Nodes_Backtrace_Log.Length = 0,
              Message   => "Transaction log not cleared");
--  begin read only
   end Test_Clear_Backtrace_Log;
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
end Mutools.Xmldebuglog.Test_Data.Tests;
