--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Expressions.Test_Data.

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
package body Mutools.Expressions.Test_Data.Tests is

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

      ----------------------------------------------------------------------

      procedure No_Expressions
      is
         Output : constant String := "obj/config_no_expressions.xml";
         Data   : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src.xml");

         Muxml.Utils.Remove_Elements (Doc   => Data.Doc,
                                      XPath => "/system/expressions");

         Expand (Policy => Data);

         Muxml.Write (Data => Data,
                      Kind => Muxml.None,
                      File => Output);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/config_no_expressions.xml",
                  Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);

         Ada.Directories.Delete_File (Name => Output);
      end No_Expressions;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Output : constant String := "obj/config_expressions.xml";
         Data   : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src.xml");

         Expand (Policy => Data);

         Muxml.Write (Data => Data,
                      Kind => Muxml.None,
                      File => Output);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/config_expressions.xml",
                  Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);

         Ada.Directories.Delete_File (Name => Output);
      end Positive_Test;
   begin
      Positive_Test;
      No_Expressions;
--  begin read only
   end Test_Expand;
--  end read only


--  begin read only
   procedure Test_Bool_Value (Gnattest_T : in out Test);
   procedure Test_Bool_Value_7e141a (Gnattest_T : in out Test) renames Test_Bool_Value;
--  id:2.2/7e141a8fc7135188/Bool_Value/1/0/
   procedure Test_Bool_Value (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Invalid_Boolean
      is
         Impl : DOM.Core.DOM_Implementation;
         Data : Muxml.XML_Data_Type;
         Node : DOM.Core.Node;
      begin
         Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

         Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "foobar");
         Muxml.Utils.Append_Child
           (Node      => Data.Doc,
            New_Child => Node);

         declare
            Dummy : Boolean;
         begin
            Dummy := Bool_Value (Policy => Data,
                                 Node   => Node);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Invalid_Expression =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Invalid boolean type 'foobar'",
                       Message   => "Exception message mismatch");
         end;
      end Invalid_Boolean;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src.xml");

         Assert (Condition => Bool_Value
                 (Policy => Data,
                  Node   => Muxml.Utils.Get_Element
                    (Doc   => Data.Doc,
                     XPath => "/system/expressions/expression"
                     & "[@name='truth']/boolean")),
                 Message   => "Boolean value mismatch (1)");

         Assert (Condition => Bool_Value
                 (Policy => Data,
                  Node   => Muxml.Utils.Get_Element
                    (Doc   => Data.Doc,
                     XPath => "/system/expressions/expression"
                     & "[@name='iommu_on']/variable")),
                 Message   => "Boolean value mismatch (2)");
      end Positive_Test;
   begin
      Positive_Test;
      Invalid_Boolean;
--  begin read only
   end Test_Bool_Value;
--  end read only


--  begin read only
   procedure Test_Int_Value (Gnattest_T : in out Test);
   procedure Test_Int_Value_1fbfca (Gnattest_T : in out Test) renames Test_Int_Value;
--  id:2.2/1fbfcac9d59ea29e/Int_Value/1/0/
   procedure Test_Int_Value (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Invalid_Integer
      is
         Impl : DOM.Core.DOM_Implementation;
         Data : Muxml.XML_Data_Type;
         Node : DOM.Core.Node;
      begin
         Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

         Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "foobar");
         Muxml.Utils.Append_Child
           (Node      => Data.Doc,
            New_Child => Node);

         declare
            Dummy : Integer;
         begin
            Dummy := Int_Value (Policy => Data,
                                Node   => Node);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Invalid_Expression =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Invalid integer type 'foobar'",
                       Message   => "Exception message mismatch");
         end;
      end Invalid_Integer;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src.xml");

         Assert (Condition => 4 = Int_Value
                 (Policy => Data,
                  Node   => Muxml.Utils.Get_Element
                    (Doc   => Data.Doc,
                     XPath => "/system/expressions/expression"
                     & "[@name='session2_enabled']/gt/variable")),
                 Message   => "Integer value mismatch (1)");
         Assert (Condition => 1 = Int_Value
                 (Policy => Data,
                  Node   => Muxml.Utils.Get_Element
                    (Doc   => Data.Doc,
                     XPath => "/system/expressions/expression"
                     & "[@name='session2_enabled']/gt/integer")),
                 Message   => "Integer value mismatch (2)");
      end Positive_Test;
   begin
      Positive_Test;
      Invalid_Integer;
--  begin read only
   end Test_Int_Value;
--  end read only


--  begin read only
   procedure Test_Expression (Gnattest_T : in out Test);
   procedure Test_Expression_a0f744 (Gnattest_T : in out Test) renames Test_Expression;
--  id:2.2/a0f744435817e29a/Expression/1/0/
   procedure Test_Expression (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Gt_Missing_Child
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src.xml");

         Muxml.Utils.Remove_Elements
           (Doc   => Data.Doc,
            XPath => "/system/expressions/expression[@name='session2_enabled']"
            & "/gt/variable");

         declare
            Dummy : Boolean;
         begin
            Dummy := Expression
              (Policy => Data,
               Node   => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/expressions/expression"
                  & "[@name='session2_enabled']/gt"));
            Assert (Condition => False,
                    Message   => "Exception expected (missing child gt)");

         exception
            when E : Invalid_Expression =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Operator 'gt' requires two child elements",
                       Message   => "Exception message mismatch (missing "
                       & "child gt)");
         end;
      end Gt_Missing_Child;

      ----------------------------------------------------------------------

      procedure Lt_Missing_Child
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src.xml");

         Muxml.Utils.Remove_Elements
           (Doc   => Data.Doc,
            XPath => "/system/expressions/expression[@name='is_below_max']"
            & "/lt/variable");

         declare
            Dummy : Boolean;
         begin
            Dummy := Expression
              (Policy => Data,
               Node   => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/expressions/expression"
                  & "[@name='is_below_max']/lt"));
            Assert (Condition => False,
                    Message   => "Exception expected (missing child lt)");

         exception
            when E : Invalid_Expression =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Operator 'lt' requires two child elements",
                       Message   => "Exception message mismatch (missing "
                       & "child lt)");
         end;
      end Lt_Missing_Child;

      ----------------------------------------------------------------------

      procedure Not_Missing_Child
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src.xml");

         Muxml.Utils.Remove_Elements
           (Doc   => Data.Doc,
            XPath => "/system/expressions/expression[@name='iommu_disabled']"
            & "/not/variable");

         declare
            Dummy : Boolean;
         begin
            Dummy := Expression
              (Policy => Data,
               Node   => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/expressions/expression"
                  & "[@name='iommu_disabled']/not"));
            Assert (Condition => False,
                    Message   => "Exception expected (missing child not)");

         exception
            when E : Invalid_Expression =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Operator 'not' requires one child element",
                       Message   => "Exception message mismatch (missing "
                       & "child not)");
         end;
      end Not_Missing_Child;

      ----------------------------------------------------------------------

      procedure Missing_Operator
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src.xml");

         Muxml.Utils.Remove_Elements
           (Doc   => Data.Doc,
            XPath => "/system/expressions/expression[@name='session2_enabled']"
            & "/gt");

         declare
            Dummy : Boolean;
         begin
            Dummy := Expression
              (Policy => Data,
               Node   => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/expressions/expression"
                  & "[@name='session2_enabled']"));
            Assert (Condition => False,
                    Message   => "Exception expected (missing op)");

         exception
            when E : Invalid_Expression =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Expression 'session2_enabled': Missing operator",
                       Message   => "Exception message mismatch (missing op)");
         end;
      end Missing_Operator;

      ----------------------------------------------------------------------

      procedure Invalid_Expression_Term
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src.xml");

         declare
            Expr  : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/expressions/expression"
                 & "[@name='session2_enabled']");
            Dummy : Boolean;
         begin
            Muxml.Utils.Remove_Child
              (Node       => Expr,
               Child_Name => "gt");
            Muxml.Utils.Append_Child
              (Node      => Expr,
               New_Child => DOM.Core.Documents.Create_Element
                 (Doc      => Data.Doc,
                  Tag_Name => "invalid_term"));

            Dummy := Expression (Policy => Data,
                                 Node   => Expr);
            Assert (Condition => False,
                    Message   => "Exception expected (invalid term)");

         exception
            when E : Invalid_Expression =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Expression 'session2_enabled': Invalid expression "
                       & "term 'invalid_term'",
                       Message   => "Exception message mismatch "
                       & "(invalid term)");
         end;
      end Invalid_Expression_Term;

      ----------------------------------------------------------------------

      procedure Invalid_Expression_Evaluation
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src.xml");

         Muxml.Utils.Remove_Elements
           (Doc   => Data.Doc,
            XPath => "/system/expressions/expression[@name='nested_expr']"
            & "/and/expression[@name='sub_expression']/or/expression");

         declare
            Dummy : Boolean;
         begin
            Dummy := Expression
              (Policy => Data,
               Node   => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/expressions/expression/and/expression"
                  & "[@name='sub_expression']"));
            Assert (Condition => False,
                    Message   => "Exception expected (invalid expr eval)");

         exception
            when E : Invalid_Expression =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Expression 'sub_expression': Operator 'or' requires "
                       & "two child elements",
                       Message   => "Exception message mismatch "
                       & "(invalid expr eval)");
         end;
      end Invalid_Expression_Evaluation;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src.xml");

         Assert (Condition => Expression
                 (Policy => Data,
                  Node   => Muxml.Utils.Get_Element
                    (Doc   => Data.Doc,
                     XPath => "/system/expressions/expression"
                     & "[@name='session2_enabled']")),
                 Message   => "Expression value mismatch (1)");
         Assert (Condition => not Expression
                 (Policy => Data,
                  Node   => Muxml.Utils.Get_Element
                    (Doc   => Data.Doc,
                     XPath => "/system/expressions/expression"
                     & "[@name='session2_disabled']")),
                 Message   => "Expression value mismatch (2)");
         Assert (Condition => Expression
                 (Policy => Data,
                  Node   => Muxml.Utils.Get_Element
                    (Doc   => Data.Doc,
                     XPath => "/system/expressions/expression"
                     & "[@name='int_values']")),
                 Message   => "Expression value mismatch (3)");

         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/config/integer[@name='session_count']",
            Name  => "value",
            Value => "1");

         Assert (Condition => not Expression
                 (Policy => Data,
                  Node   => Muxml.Utils.Get_Element
                    (Doc   => Data.Doc,
                     XPath => "/system/expressions/expression"
                     & "[@name='session2_enabled']")),
                 Message   => "Expression value mismatch (4)");
         Assert (Condition => Expression
                 (Policy => Data,
                  Node   => Muxml.Utils.Get_Element
                    (Doc   => Data.Doc,
                     XPath => "/system/expressions/expression"
                     & "[@name='session2_disabled']")),
                 Message   => "Expression value mismatch (5)");
      end Positive_Test;
   begin
      Positive_Test;
      Missing_Operator;
      Gt_Missing_Child;
      Lt_Missing_Child;
      Not_Missing_Child;
      Invalid_Expression_Term;
      Invalid_Expression_Evaluation;
--  begin read only
   end Test_Expression;
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
end Mutools.Expressions.Test_Data.Tests;
