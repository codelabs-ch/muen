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

with Ada.Strings.Unbounded;
with Mutools.System_Config;
with McKae.XML.XPath.XIA;

--  begin read only
--  end read only
package body Mutools.Expressions.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

   procedure Initialize_Node_Access_Testing
      (Data        :        Muxml.XML_Data_Type;
       Node_Access : in out Access_Hashmaps_Type);


   procedure Initialize_Node_Access_Testing
      (Data        :        Muxml.XML_Data_Type;
       Node_Access : in out Access_Hashmaps_Type)
   is
      Config_And_Exprs : constant DOM.Core.Node_List
         := McKae.XML.XPath.XIA.XPath_Query
               (N     => Data.Doc,
                XPath =>  "/*/config/boolean | "
                   & "/*/config/integer | "
                   & "/*/config/string | "
                   & "/*/expressions/expression");
   begin
      Node_Access.Input.Clear;
      Node_Access.Output_Boolean.Clear;
      Node_Access.Output_Integer.Clear;
      Node_Access.Output_String.Clear;

      Initialize_Node_Access
         (Node_Access      => Node_Access,
          Config_And_Exprs => Config_And_Exprs);
   end Initialize_Node_Access_Testing;

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
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
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
            Dummy := Bool_Value (Backtrace => Backtrace,
                                 Node_Access => Node_Access,
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
         Data             : Muxml.XML_Data_Type;
         Backtrace        : String_Vector.Vector;
         Node_Access      :  Access_Hashmaps_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src.xml");

         Initialize_Node_Access_Testing (Data => Data, Node_Access => Node_Access);

         Assert (Condition => not Bool_Value
                    (Backtrace   => Backtrace,
                     Node_Access => Node_Access,
                     Node        => Muxml.Utils.Get_Element
                        (Doc   => Data.Doc,
                         XPath => "/system/expressions/expression"
                            & "[@name='not_false']/not/boolean")),
                 Message   => "Boolean value mismatch (1)");

         Assert (Condition => Bool_Value
                 (Backtrace => Backtrace,
                  Node_Access => Node_Access,
                  Node   => Muxml.Utils.Get_Element
                    (Doc   => Data.Doc,
                     XPath => "/system/expressions/expression"
                     & "[@name='iommu_disabled']/not/variable")),
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
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
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
            Dummy := Int_Value (Backtrace => Backtrace,
                                Node_Access => Node_Access,
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
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src.xml");

         Initialize_Node_Access_Testing (Data => Data, Node_Access => Node_Access);

         Assert (Condition => 4 = Int_Value
                 (Backtrace => Backtrace,
                  Node_Access => Node_Access,
                  Node   => Muxml.Utils.Get_Element
                    (Doc   => Data.Doc,
                     XPath => "/system/expressions/expression"
                     & "[@name='session2_enabled']/gt/variable")),
                 Message   => "Integer value mismatch (1)");
         Assert (Condition => 1 = Int_Value
                 (Backtrace => Backtrace,
                  Node_Access => Node_Access,
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
   procedure Test_String_Value (Gnattest_T : in out Test);
   procedure Test_String_Value_f74087 (Gnattest_T : in out Test) renames Test_String_Value;
--  id:2.2/f740879297b0eec4/String_Value/1/0/
   procedure Test_String_Value (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Data : Muxml.XML_Data_Type;
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src_concatenation.xml");

         Initialize_Node_Access_Testing (Data => Data, Node_Access => Node_Access);

         Assert (Condition => "ram" = String_Value
                 (Backtrace => Backtrace,
                  Node_Access => Node_Access,
                  Node   => Muxml.Utils.Get_Element
                    (Doc   => Data.Doc,
                     XPath => "/system/expressions/expression"
                     & "[@name='compositeName']/concatenation/variable")),
                 Message   => "Integer value mismatch (1)");
         Assert (Condition => "1" = String_Value
                 (Backtrace => Backtrace,
                  Node_Access => Node_Access,
                  Node   => Muxml.Utils.Get_Element
                    (Doc   => Data.Doc,
                     XPath => "/system/expressions/expression"
                     & "[@name='compositeName']/concatenation/string")),
                 Message   => "Integer value mismatch (2)");
      end Positive_Test;

      ----------------------------------------------------------------------

      procedure Invalid_String
      is
         Data : Muxml.XML_Data_Type;
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
      begin
          Muxml.Parse (Data => Data,
                       Kind => Muxml.None,
                       File => "data/test_policy_src_concatenation.xml");
          Initialize_Node_Access_Testing (Data => Data, Node_Access => Node_Access);
         declare

            Node : DOM.Core.Node
                 :=  Muxml.Utils.Get_Element
                       (Doc   => Data.Doc,
                        XPath => "/system/expressions/expression"
                        & "[@name='truth']/boolean");
            Dummy : Ada.Strings.Unbounded.Unbounded_String;
         begin
            Dummy :=  Ada.Strings.Unbounded.To_Unbounded_String
                         (String_Value (Backtrace => Backtrace,
                                        Node_Access => Node_Access,
                                        Node   => Node));
            Assert (Condition => False,
                    Message   => "Exception expected");
         exception
            when E : Invalid_Expression =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Invalid string type 'boolean'",
                       Message   => "Exception message mismatch");
         end;

         declare
            Node : DOM.Core.Node
                 :=  Muxml.Utils.Get_Element
                       (Doc   => Data.Doc,
                        XPath => "/system/expressions/expression"
                        & "[@name='session2_enabled']/gt/variable");
            Dummy : Ada.Strings.Unbounded.Unbounded_String;
         begin

            Dummy :=  Ada.Strings.Unbounded.To_Unbounded_String
                         (String_Value (Backtrace => Backtrace,
                                        Node_Access => Node_Access,
                                        Node   => Node));
            Assert (Condition => False,
                    Message   => "Exception expected");
         exception
            when E : Muxml.Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                          = "A string variable or expression points to node with "
                          & "type 'integer' which is not string valued",
                       Message   => "Exception message mismatch");
         end;
      end Invalid_String;

   begin
      Positive_Test;
      Invalid_String;
--  begin read only
   end Test_String_Value;
--  end read only


--  begin read only
   procedure Test_Boolean_Expression (Gnattest_T : in out Test);
   procedure Test_Boolean_Expression_959f9e (Gnattest_T : in out Test) renames Test_Boolean_Expression;
--  id:2.2/959f9e9a477bb3b1/Boolean_Expression/1/0/
   procedure Test_Boolean_Expression (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Gt_Missing_Child
      is
         Data : Muxml.XML_Data_Type;
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
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
            Initialize_Node_Access_Testing (Data => Data, Node_Access => Node_Access);
            Dummy := Boolean_Expression
               (Backtrace => Backtrace,
                Node_Access => Node_Access,
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
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
      begin
         Muxml.Parse
            (Data => Data,
             Kind => Muxml.None,
             File => "data/test_policy_src.xml");
         Initialize_Node_Access_Testing (Data => Data, Node_Access => Node_Access);

         Muxml.Utils.Remove_Elements
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='is_below_max']"
             & "/lt/variable");

         declare
            Dummy : Boolean;
         begin
            Dummy := Boolean_Expression
               (Backtrace => Backtrace,
                Node_Access => Node_Access,
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
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
      begin
         Muxml.Parse
            (Data => Data,
             Kind => Muxml.None,
             File => "data/test_policy_src.xml");
         Initialize_Node_Access_Testing (Data => Data, Node_Access => Node_Access);

         Muxml.Utils.Remove_Elements
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='iommu_disabled']"
             & "/not/variable");

         declare
            Dummy : Boolean;
         begin
            Dummy := Boolean_Expression
               (Backtrace => Backtrace,
                Node_Access => Node_Access,
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
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
      begin
         Muxml.Parse
            (Data => Data,
             Kind => Muxml.None,
             File => "data/test_policy_src.xml");
         Initialize_Node_Access_Testing (Data => Data, Node_Access => Node_Access);

         Muxml.Utils.Remove_Elements
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='session2_enabled']"
             & "/gt");

         declare
            Dummy : Boolean;
         begin
            Dummy := Boolean_Expression
               (Backtrace => Backtrace,
                Node_Access => Node_Access,
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
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
      begin
         Muxml.Parse
            (Data => Data,
             Kind => Muxml.None,
             File => "data/test_policy_src.xml");
         Initialize_Node_Access_Testing (Data => Data, Node_Access => Node_Access);

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

            Dummy := Boolean_Expression (Backtrace => Backtrace,
                                         Node_Access => Node_Access,
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
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
      begin
         Muxml.Parse
            (Data => Data,
             Kind => Muxml.None,
             File => "data/test_policy_src.xml");
         Initialize_Node_Access_Testing (Data => Data, Node_Access => Node_Access);

         Muxml.Utils.Remove_Elements
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='nested_expr']"
             & "/and/expression[@name='sub_expression']/or/expression");

         declare
            Dummy : Boolean;
         begin
            Dummy := Boolean_Expression
               (Backtrace => Backtrace,
                Node_Access => Node_Access,
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
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
      begin
         Muxml.Parse
            (Data => Data,
             Kind => Muxml.None,
             File => "data/test_policy_src.xml");
         Initialize_Node_Access_Testing (Data => Data, Node_Access => Node_Access);

         Assert (Condition => Boolean_Expression
                 (Backtrace => Backtrace,
                  Node_Access => Node_Access,
                  Node   => Muxml.Utils.Get_Element
                  (Doc   => Data.Doc,
                   XPath => "/system/expressions/expression"
                   & "[@name='session2_enabled']")),
                 Message   => "Expression value mismatch (1)");
         Assert (Condition => not Boolean_Expression
                 (Backtrace => Backtrace,
                  Node_Access => Node_Access,
                  Node   => Muxml.Utils.Get_Element
                  (Doc   => Data.Doc,
                   XPath => "/system/expressions/expression"
                   & "[@name='session2_disabled']")),
                 Message   => "Expression value mismatch (2)");
         Assert (Condition => Boolean_Expression
                 (Backtrace => Backtrace,
                  Node_Access => Node_Access,
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

         -- we have to reinitialize because function like Bool_Value lookup
         -- values in Node_Access.Output. Hence, Set_Attribute has no effect
         -- without reinitializing.
         Initialize_Node_Access_Testing (Data => Data, Node_Access => Node_Access);
         Assert (Condition => not Boolean_Expression
                 (Backtrace => Backtrace,
                  Node_Access => Node_Access,
                  Node   => Muxml.Utils.Get_Element
                  (Doc   => Data.Doc,
                   XPath => "/system/expressions/expression"
                   & "[@name='session2_enabled']")),
                 Message   => "Expression value mismatch (4)");
         Assert (Condition => Boolean_Expression
                 (Backtrace => Backtrace,
                  Node_Access => Node_Access,
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
   end Test_Boolean_Expression;
   --  end read only

   --  begin read only
   procedure Test_String_Expression (Gnattest_T : in out Test);
   procedure Test_String_Expression_1b0a9f (Gnattest_T : in out Test) renames Test_String_Expression;
   --  id:2.2/1b0a9f52602f5a02/String_Expression/1/0/
   procedure Test_String_Expression (Gnattest_T : in out Test) is
      --  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Data : Muxml.XML_Data_Type;
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src_concatenation.xml");
         Initialize_Node_Access_Testing (Data => Data, Node_Access => Node_Access);

         Assert (Condition => "1ramram_foo" = String_Expression
                   (Backtrace => Backtrace,
                    Node_Access => Node_Access,
                    Node   => Muxml.Utils.Get_Element
                       (Doc   => Data.Doc,
                        XPath => "/system/expressions/expression"
                        & "[@name='compositeName']")),
                 Message   => "Expression value mismatch");
      end Positive_Test;

      ----------------------------------------------------------------------

      procedure No_Child
      is
         Data : Muxml.XML_Data_Type;
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src_concatenation.xml");
         Initialize_Node_Access_Testing (Data => Data, Node_Access => Node_Access);

         Muxml.Utils.Remove_Elements
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression"
             & "[@name='compositeName']/concatenation/*");
         declare
            Dummy : Ada.Strings.Unbounded.Unbounded_String;
         begin
            Dummy :=  Ada.Strings.Unbounded.To_Unbounded_String
               (String_Expression
                   (Backtrace => Backtrace,
                    Node_Access => Node_Access,
                    Node   => Muxml.Utils.Get_Element
                                (Doc   => Data.Doc,
                                 XPath => "/system/expressions/expression"
                                          & "[@name='compositeName']")));
            Assert (Condition => False,
                    Message   => "Exception expected");
         exception
            when E : Invalid_Expression =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                          = "Concatenation-expression 'compositeName' "
                            & "has less than two children",
                       Message   => "Exception message mismatch "
                       & "(invalid concatenation)");
         end;
      end No_Child;

      ----------------------------------------------------------------------

      procedure One_Child
      is
         Data : Muxml.XML_Data_Type;
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src_concatenation.xml");
         Initialize_Node_Access_Testing (Data => Data, Node_Access => Node_Access);

         Muxml.Utils.Remove_Elements
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression"
             & "[@name='compositeName']/concatenation/variable");
         Muxml.Utils.Remove_Elements
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression"
             & "[@name='compositeName']/concatenation/string[@value='1']");
         declare
            Dummy : Ada.Strings.Unbounded.Unbounded_String;
         begin
            Dummy :=  Ada.Strings.Unbounded.To_Unbounded_String
               (String_Expression
                   (Backtrace => Backtrace,
                    Node_Access => Node_Access,
                    Node   => Muxml.Utils.Get_Element
                                (Doc   => Data.Doc,
                                 XPath => "/system/expressions/expression"
                                          & "[@name='compositeName']")));
            Assert (Condition => False,
                    Message   => "Exception expected");
         exception
            when E : Invalid_Expression =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                          = "Concatenation-expression 'compositeName' "
                            & "has less than two children",
                       Message   => "Exception message mismatch "
                       & "(invalid concatenation)");
         end;
      end One_Child;

   begin
      Positive_Test;
      No_Child;
      One_Child;

      --  begin read only
   end Test_String_Expression;
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
