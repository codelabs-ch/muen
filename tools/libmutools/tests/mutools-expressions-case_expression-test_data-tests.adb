--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Expressions.Case_Expression.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Ada.Exceptions;
with DOM.Core.Nodes;
with Muxml.Utils;
with Mutools.Expressions.Test_Data;

--  begin read only
--  end read only
package body Mutools.Expressions.Case_Expression.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Equal (Gnattest_T : in out Test);
   procedure Test_Equal_e6f5d0 (Gnattest_T : in out Test) renames Test_Equal;
--  id:2.2/e6f5d05a1cb20095/Equal/1/0/
   procedure Test_Equal (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      L, R :  Value_Type_Tuple;
   begin
      L := Value_Type_Tuple'(Bool_Value   => True,
                             Int_Value    => 0,
                             String_Value => String_Holder_Type.To_Holder ("foo"),
                             Value_Type   => Integer_Type);
      R := Value_Type_Tuple'(Bool_Value   => False,
                             Int_Value    => 0,
                             String_Value => String_Holder_Type.To_Holder ("foobar"),
                             Value_Type   => Integer_Type);

      Assert (Condition => L = L,
              Message   => "Equality expected");
      Assert (Condition => L = R,
              Message   => "Equality expected");

      R.Value_Type := String_Type;
      Assert (Condition => L /= R,
              Message   => "Inequality expected");

      L.Value_Type := String_Type;
      Assert (Condition => L /= R,
              Message   => "Inequality expected");

      L.String_Value := String_Holder_Type.To_Holder ("foobar");
      Assert (Condition => L = R,
              Message   => "Equality expected");

--  begin read only
   end Test_Equal;
--  end read only


--  begin read only
   procedure Test_To_String (Gnattest_T : in out Test);
   procedure Test_To_String_fb4562 (Gnattest_T : in out Test) renames Test_To_String;
--  id:2.2/fb45621eca001881/To_String/1/0/
   procedure Test_To_String (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      L :  Value_Type_Tuple;
   begin
      L := Value_Type_Tuple'(Bool_Value   => True,
                             Int_Value    => 0,
                             String_Value => String_Holder_Type.To_Holder ("foo"),
                             Value_Type   => Integer_Type);
      Assert (Condition => "INTEGER_TYPE 0" = To_String (L),
              Message => "Strings not equal: " & "'INTEGER_TYPE 0'  '"
                 & To_String (L)
                 & "'");

      L.Value_Type := String_Type;
      Assert (Condition => "STRING_TYPE foo" = To_String (L),
              Message   => "Strings not equal: " & "'INTEGER_TYPE foo'  '"
                 & To_String (L)
                 & "'");

      L.Value_Type := Boolean_Type;
      Assert (Condition => "BOOLEAN_TYPE TRUE" = To_String (L),
              Message   => "Strings not equal: " & "'BOOLEAN_TYPE TRUE'  '"
                 & To_String (L)
                 & "'");

--  begin read only
   end Test_To_String;
--  end read only


--  begin read only
   procedure Test_Evaluate_Case_Node_Frame (Gnattest_T : in out Test);
   procedure Test_Evaluate_Case_Node_Frame_1e49f2 (Gnattest_T : in out Test) renames Test_Evaluate_Case_Node_Frame;
--  id:2.2/1e49f2e5cfed5d5b/Evaluate_Case_Node_Frame/1/0/
   procedure Test_Evaluate_Case_Node_Frame (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data                        : Muxml.XML_Data_Type;
      Node_Access                 :  Access_Hashmaps_Type;
      Backtrace                   : String_Vector.Vector;
      In_Node, Ret_Node, Ref_Node : DOM.Core.Node;

      procedure Positive_Test
      is
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src_evalStringConcatCase.xml");
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

         ---- positive tests
         -- when matches
         In_Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case1_string']/case");
         Ref_Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case1_string']/case"
                & "/when[@value='foobar']");
         Evaluate_Case_Node_Frame
            (Case_Node   => In_Node,
             Return_Node => Ret_Node,
             Backtrace   => Backtrace,
             Node_Access => Node_Access);
         Assert (Condition => Ret_Node = Ref_Node,
                 Message   => "Node mismatch");

         -- others matches
         In_Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case2_bool']/case");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case2_bool']/case/when[@value='4']",
             Name  => "value",
             Value => "19");
         Ref_Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case2_bool']/case/"
                & "/others");
         Evaluate_Case_Node_Frame
            (Case_Node   => In_Node,
             Return_Node => Ret_Node,
             Backtrace   => Backtrace,
             Node_Access => Node_Access);
         Assert (Condition => Ret_Node = Ref_Node,
                 Message   => "Node mismatch");

         -- nothing matches
         In_Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case4_int']/case");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case4_int']/case/when[@value='true']",
             Name  => "value",
             Value => "false");
         Ref_Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case4_int']/case/"
                & "/others");
         Evaluate_Case_Node_Frame
            (Case_Node   => In_Node,
             Return_Node => Ret_Node,
             Backtrace   => Backtrace,
             Node_Access => Node_Access);
         Assert (Condition => Ret_Node = null,
                 Message   => "Node mismatch");
      end Positive_Test;

      ----------------------------------------------------------------------

      procedure When_Type_Mismatch
      is
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src_evalStringConcatCase.xml");
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);
         In_Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case1_string']/case");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case1_string']/case/when[@value='foo']",
             Name  => "value",
             Value => "$feature_enabled");
         Evaluate_Case_Node_Frame
            (Case_Node   => In_Node,
             Return_Node => Ret_Node,
             Backtrace   => Backtrace,
             Node_Access => Node_Access);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Invalid_Expression =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Found case node where variable types do not match. "
                       & "Case-variable type is 'STRING_TYPE' when-variable "
                       & "type is 'BOOLEAN_TYPE'",
                    Message   => "Exception message mismatch ("
                       &  Ada.Exceptions.Exception_Message (X => E)
                       & ")");
      end When_Type_Mismatch;

      ----------------------------------------------------------------------

      procedure Type_Cast_Failure
      is
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src_evalStringConcatCase.xml");
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);
         In_Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case2_bool']/case");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case2_bool']/case/when[@value='4']",
             Name  => "value",
             Value => "0.4");
         Evaluate_Case_Node_Frame
            (Case_Node   => In_Node,
             Return_Node => Ret_Node,
             Backtrace   => Backtrace,
             Node_Access => Node_Access);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Invalid_Expression =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Found when-node with value '0.4' which cannot be "
                       & "cast to INTEGER_TYPE",
                    Message   => "Exception message mismatch ("
                       &  Ada.Exceptions.Exception_Message (X => E)
                       & ")");
      end Type_Cast_Failure;

      ----------------------------------------------------------------------

      procedure Multiple_Matches
      is
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src_evalStringConcatCase.xml");
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);
         In_Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case4_int']/case"
                & "/when/case");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case4_int']/case"
                & "/when/case/when[@value='$foo']",
             Name  => "value",
             Value => "foobar");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case4_int']/case"
                & "/when/case/when[@value='foo']",
             Name  => "value",
             Value => "foobar");
         Evaluate_Case_Node_Frame
            (Case_Node   => In_Node,
             Return_Node => Ret_Node,
             Backtrace   => Backtrace,
             Node_Access => Node_Access);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Invalid_Expression =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Found case node where multiple values match. "
                       & "Case-variable value is 'STRING_TYPE foobar'",
                    Message   => "Exception message mismatch ("
                       &  Ada.Exceptions.Exception_Message (X => E)
                       & ")");
      end Multiple_Matches;

   begin
      Positive_Test;

      --- fault tests
      When_Type_Mismatch;
      Type_Cast_Failure;
      Multiple_Matches;

--  begin read only
   end Test_Evaluate_Case_Node_Frame;
--  end read only


--  begin read only
   procedure Test_Case_Expression_Evaluation (Gnattest_T : in out Test);
   procedure Test_Case_Expression_Evaluation_f7cb18 (Gnattest_T : in out Test) renames Test_Case_Expression_Evaluation;
--  id:2.2/f7cb1847098060d8/Case_Expression_Evaluation/1/0/
   procedure Test_Case_Expression_Evaluation (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data          : Muxml.XML_Data_Type;
      Node_Access   : Access_Hashmaps_Type;
      Backtrace     : String_Vector.Vector;
      In_Node, Node : DOM.Core.Node;
      Output        : Value_Type_Tuple;

      procedure Positive_Test
      is
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src_evalStringConcatCase.xml");
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);
         In_Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case4_int']");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/config/boolean[@name='feature_enabled']",
             Name  => "value",
             Value => "false");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case4_int']"
                & "/case/when/case/when[@value='$foo']",
             Name  => "value",
             Value => "bar");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/config/string[@name='foo']",
             Name  => "value",
             Value => "nono");
         Case_Expression_Evaluation
            (Expr_Node     => In_Node,
             Value_Of_Case => Output,
             Backtrace     => Backtrace,
             Node_Access   => Node_Access);
         Assert (Condition => Output.Value_Type = Integer_Type and
                              Output.Int_Value = 0,
                 Message   => "Value mismatch: " & To_String (Output));
      end Positive_Test;

      ----------------------------------------------------------------------

      procedure Multiple_Case_Children is
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src_evalStringConcatCase.xml");
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);
         In_Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case1_string']");
         Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case1_string']/case");
         Node := DOM.Core.Nodes.Clone_Node (N => Node, Deep => True);
         Node := DOM.Core.Nodes.Append_Child
            (N         => In_Node,
             New_Child => Node);
         Case_Expression_Evaluation
            (Expr_Node     => In_Node,
             Value_Of_Case => Output,
             Backtrace     => Backtrace,
             Node_Access   => Node_Access);
        Assert (Condition => False,
                Message   => "Exception expected");
      exception
         when E : Invalid_Expression =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Case-expression 'case1_string' has 2 case-child nodes."
                       & " Should have exactly one.",
                    Message   => "Exception message mismatch: "
                   & Ada.Exceptions.Exception_Message (X => E));
      end Multiple_Case_Children;

      ----------------------------------------------------------------------

      procedure Multiple_When_Children is
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src_evalStringConcatCase.xml");
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);
         In_Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case1_string']"
                & "/case/when[@value='foobar']");
         Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case1_string']"
                & "/case/when[@value='foobar']/variable");
         Node := DOM.Core.Nodes.Clone_Node (N => Node, Deep => True);
         Node := DOM.Core.Nodes.Append_Child
            (N         => In_Node,
             New_Child => Node);
         In_Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case1_string']");

         Case_Expression_Evaluation
            (Expr_Node     => In_Node,
             Value_Of_Case => Output,
             Backtrace     => Backtrace,
             Node_Access   => Node_Access);
        Assert (Condition => False,
                Message   => "Exception expected");
      exception
         when E : Invalid_Expression =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "When-Node inside of Case has 2 child nodes. Should have one.",
                    Message   => "Exception message mismatch: "
                   & Ada.Exceptions.Exception_Message (X => E));
      end Multiple_When_Children;

      ----------------------------------------------------------------------

      procedure No_Matching_When is
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src_evalStringConcatCase.xml");
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);
         In_Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case1_string']");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case1_string']"
                & "/case/when[@value='foobar']",
             Name  => "value",
             Value => "notfoobar");
        Case_Expression_Evaluation
            (Expr_Node     => In_Node,
             Value_Of_Case => Output,
             Backtrace     => Backtrace,
             Node_Access   => Node_Access);
        Assert (Condition => False,
                Message   => "Exception expected");
      exception
         when E : Invalid_Expression =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Found case-node in expression where none of the "
                       & "actuals matches. Case-variable has name 'foo'",
                    Message   => "Exception message mismatch: "
                   & Ada.Exceptions.Exception_Message (X => E));
      end No_Matching_When;

      ----------------------------------------------------------------------

      procedure When_Types_Mismatch is
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src_evalStringConcatCase.xml");
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);
         In_Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case4_int']");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='case4_int']"
                & "/case/when/case/when/variable[@name='scount']",
             Name  => "name",
             Value => "name1");
        Case_Expression_Evaluation
            (Expr_Node     => In_Node,
             Value_Of_Case => Output,
             Backtrace     => Backtrace,
             Node_Access   => Node_Access);
        Assert (Condition => False,
                Message   => "Exception expected");
      exception
         when E : Invalid_Expression =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Case expression contains values of multiple types: "
                       & "'INTEGER_TYPE' and 'STRING_TYPE'",
                    Message   => "Exception message mismatch: "
                   & Ada.Exceptions.Exception_Message (X => E));
      end When_Types_Mismatch;

   begin
      Positive_Test;
      Multiple_Case_Children;
      Multiple_When_Children;
      No_Matching_When;
      When_Types_Mismatch;

--  begin read only
   end Test_Case_Expression_Evaluation;
--  end read only


--  begin read only
   procedure Test_Get_Value_Of_Reference_Debug (Gnattest_T : in out Test);
   procedure Test_Get_Value_Of_Reference_Debug_6f56db (Gnattest_T : in out Test) renames Test_Get_Value_Of_Reference_Debug;
--  id:2.2/6f56dbded8f152d9/Get_Value_Of_Reference_Debug/1/0/
   procedure Test_Get_Value_Of_Reference_Debug (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Node_Access : Access_Hashmaps_Type;
   begin
      Node_Access.Input.Insert (Key      => "in_bool",
                                New_Item => null);
      Node_Access.Output_Boolean.Insert (Key      => "out_bool1",
                                         New_Item => True);
      Node_Access.Output_Boolean.Insert (Key      => "out_bool2",
                                         New_Item => False);
      Node_Access.Output_Integer.Insert (Key      => "out_int",
                                         New_Item => 42);
      Assert (Condition => "FALSE" = Get_Value_Of_Reference_Debug
                 (Ref_Name    => "out_bool2",
                  Node_Access => Node_Access),
              Message   => "Value mismatch");
      Assert (Condition => "42" = Get_Value_Of_Reference_Debug
                 (Ref_Name    => "out_int",
                  Node_Access => Node_Access),
              Message   => "Value mismatch");
      Assert (Condition => "" = Get_Value_Of_Reference_Debug
                 (Ref_Name    => "in_bool",
                  Node_Access => Node_Access),
              Message   => "Value mismatch");
      Assert (Condition => "" = Get_Value_Of_Reference_Debug
                 (Ref_Name    => "something else",
                  Node_Access => Node_Access),
              Message   => "Value mismatch");

--  begin read only
   end Test_Get_Value_Of_Reference_Debug;
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
end Mutools.Expressions.Case_Expression.Test_Data.Tests;
