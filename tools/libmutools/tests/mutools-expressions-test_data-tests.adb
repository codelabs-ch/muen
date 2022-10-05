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
with DOM.Core.Nodes;

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
   procedure Test_Expand_4a19b8 (Gnattest_T : in out Test) renames Test_Expand;
--  id:2.2/4a19b878eb07fa84/Expand/1/0/
   procedure Test_Expand (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Output : constant String := "obj/output_config_expressions.xml";
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
                 (Filename1 => "data/output_config_expressions.xml",
                  Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);

         Ada.Directories.Delete_File (Name => Output);
      end Positive_Test;

      ----------------------------------------------------------------------

      procedure No_Expressions
      is
         Output : constant String := "obj/output_config_no_expressions.xml";
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
                 (Filename1 => "data/output_config_no_expressions.xml",
                  Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);

         Ada.Directories.Delete_File (Name => Output);
      end No_Expressions;

      ----------------------------------------------------------------------

      procedure No_Config_No_Expressions
      is
         Output : constant String := "obj/output_no_config_no_expressions.xml";
         Data   : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy_src.xml");

         Muxml.Utils.Remove_Elements (Doc   => Data.Doc,
                                      XPath => "/system/expressions");
         Muxml.Utils.Remove_Elements (Doc   => Data.Doc,
                                      XPath => "/system/config");

         Expand (Policy => Data);

         Muxml.Write (Data => Data,
                      Kind => Muxml.None,
                      File => Output);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/output_no_config_no_expressions.xml",
                  Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);

         Ada.Directories.Delete_File (Name => Output);
      end No_Config_No_Expressions;

   begin
      Positive_Test;
      No_Expressions;
      No_Config_No_Expressions;

--  begin read only
   end Test_Expand;
--  end read only


--  begin read only
   procedure Test_Parse_Dollar_Braced_References (Gnattest_T : in out Test);
   procedure Test_Parse_Dollar_Braced_References_6a0135 (Gnattest_T : in out Test) renames Test_Parse_Dollar_Braced_References;
--  id:2.2/6a013511d726990a/Parse_Dollar_Braced_References/1/0/
   procedure Test_Parse_Dollar_Braced_References (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      use all type Fragment_Vector.Vector;

      Text_Foo : constant Fragment_Entry
         := Fragment_Entry'
         (Value => String_Holder_Type.To_Holder ("foo"),
          Value_Type => Text_Type);
      Ref_Foo : constant Fragment_Entry
         := Fragment_Entry'
         (Value => String_Holder_Type.To_Holder ("foo"),
          Value_Type => Reference_Type);
      Ref_Bar : constant Fragment_Entry
         := Fragment_Entry'
         (Value => String_Holder_Type.To_Holder ("bar"),
          Value_Type => Reference_Type);
      Text_Special_Char : constant Fragment_Entry
         := Fragment_Entry'
         (Value => String_Holder_Type.To_Holder ("stuff _'["),
          Value_Type => Text_Type);
      Ref_Special_Char : constant Fragment_Entry
         := Fragment_Entry'
         (Value => String_Holder_Type.To_Holder ("stuff _'["),
          Value_Type => Reference_Type);

      Output, Correct_Output : Fragment_Vector.Vector;

      function To_String (Vector : Fragment_Vector.Vector) return String
      is
         use Ada.Strings.Unbounded;
         Output : Unbounded_String;
      begin
         for Element of Vector loop
            Ada.Strings.Unbounded.Append
               (Source => Output,
                New_Item => To_Unbounded_String
                   ("(" & Element.Value_Type'Img & ", " & Element.Value.Element & ") "));
         end loop;
         return Ada.Strings.Unbounded.To_String (Output);
      end To_String;

      procedure Test_Faulty_String (Input : String)
      is
      begin
         Output := Parse_Dollar_Braced_References (Input_String => Input);
         Assert (Condition => False,
                 Message   => "Exception expected (" & Input & ")");
      exception
         when E : Invalid_Expression =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "EvalString got invaild value '" & Input & "'",
                    Message   => "Exception message mismatch (" & Input & ")");
      end Test_Faulty_String;

   begin
      -- multiple references and texts
      Output := Parse_Dollar_Braced_References
         (Input_String => "foo${foo}${stuff _'[}stuff _'[${bar}");
      Fragment_Vector.Clear (Correct_Output);
      Fragment_Vector.Append (Container => Correct_Output,
                              New_Item  => Text_Foo);
      Fragment_Vector.Append (Container => Correct_Output,
                              New_Item  => Ref_Foo);
      Fragment_Vector.Append (Container => Correct_Output,
                              New_Item  => Ref_Special_Char);
      Fragment_Vector.Append (Container => Correct_Output,
                              New_Item  => Text_Special_Char);
      Fragment_Vector.Append (Container => Correct_Output,
                              New_Item  => Ref_Bar);

      Assert (Condition => Output = Correct_Output,
              Message   => "Output mismatch (1): """
                 & To_String (Output)
                 & """ != """
                 & To_String (Correct_Output)
                 & """");

      -- empty input
      Output := Parse_Dollar_Braced_References (Input_String => "");
      Fragment_Vector.Clear (Correct_Output);

      Assert (Condition => Output = Correct_Output,
              Message   => "Output mismatch (2): """
                 & To_String (Output)
                 & """ != """
                 & To_String (Correct_Output)
                 & """");

      -- no $, {, } at all
      Output := Parse_Dollar_Braced_References (Input_String => "foo");
      Fragment_Vector.Clear (Correct_Output);
      Fragment_Vector.Append (Container => Correct_Output,
                              New_Item  => Text_Foo);

      Assert (Condition => Output = Correct_Output,
              Message   => "Output mismatch (3): """
                 & To_String (Output)
                 & """ != """
                 & To_String (Correct_Output)
                 & """");

      -- one reference and othing else
      Output := Parse_Dollar_Braced_References (Input_String => "${foo}");
      Fragment_Vector.Clear (Correct_Output);
      Fragment_Vector.Append (Container => Correct_Output,
                              New_Item  => Ref_Foo);

      Assert (Condition => Output = Correct_Output,
              Message   => "Output mismatch (4): """
                 & To_String (Output)
                 & """ != """
                 & To_String (Correct_Output)
                 & """");

      -- empty reference ${}
      -- Notice: an empty reference might be a mistake but its ok for the parser
      -- Initialize_Node_Access refuses to include empty keys.
      Output := Parse_Dollar_Braced_References (Input_String => "foo${}");
      Fragment_Vector.Clear (Correct_Output);
      Fragment_Vector.Append (Container => Correct_Output,
                              New_Item  => Text_Foo);
      Fragment_Vector.Append (Container => Correct_Output,
                              New_Item  => Fragment_Entry'
                                 (Value => String_Holder_Type.To_Holder (""),
                                  Value_Type => Reference_Type));

      Assert (Condition => Output = Correct_Output,
              Message   => "Output mismatch (5): """
                 & To_String (Output)
                 & """ != """
                 & To_String (Correct_Output)
                 & """");

      -- Tests with broken input
      Test_Faulty_String (Input => "$foo");
      Test_Faulty_String (Input => "{foo}");
      Test_Faulty_String (Input => "$${bar}foo");
      Test_Faulty_String (Input => "bar{foo");
      Test_Faulty_String (Input => "${foo}bar}");
      Test_Faulty_String (Input => "${foo");

--  begin read only
   end Test_Parse_Dollar_Braced_References;
--  end read only


--  begin read only
   procedure Test_Bool_Value (Gnattest_T : in out Test);
   procedure Test_Bool_Value_6271ca (Gnattest_T : in out Test) renames Test_Bool_Value;
--  id:2.2/6271ca7b3bd9bacc/Bool_Value/1/0/
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

         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

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
   procedure Test_Int_Value_6408a2 (Gnattest_T : in out Test) renames Test_Int_Value;
--  id:2.2/6408a27db967ed80/Int_Value/1/0/
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

         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

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
   procedure Test_String_Value_7bae15 (Gnattest_T : in out Test) renames Test_String_Value;
--  id:2.2/7bae15ffd4b32a3f/String_Value/1/0/
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

         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

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
          Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);
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
   procedure Test_Evaluate_Boolean (Gnattest_T : in out Test);
   procedure Test_Evaluate_Boolean_f8ab09 (Gnattest_T : in out Test) renames Test_Evaluate_Boolean;
--  id:2.2/f8ab09719ce83a53/Evaluate_Boolean/1/0/
   procedure Test_Evaluate_Boolean (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data        : Muxml.XML_Data_Type;
      Node_Access : Access_Hashmaps_Type;
      Backtrace   : String_Vector.Vector;

   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/test_policy_src_evalStringConcatCase.xml");

      Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

      -- Positive_Test: Expression_Case
      Assert (Condition =>  Evaluate_Boolean
                 (Node        => Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/expressions/expression[@name='case2_bool']"),
                  Backtrace   => Backtrace,
                  Node_Access => Node_Access),
              Message => "Boolean value mismatch");

      -- Positive_Test: Expression_Boolean
      Assert (Condition => not Evaluate_Boolean
                 (Node        => Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/expressions/expression[@name='two_is_not_two']"),
                  Backtrace   => Backtrace,
                  Node_Access => Node_Access),
              Message => "Boolean value mismatch");

      -- Positive_Test: Boolean_Constant
      Assert (Condition => Evaluate_Boolean
                 (Node        => Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/config/boolean[@name='feature_enabled']"),
                  Backtrace   => Backtrace,
                  Node_Access => Node_Access),
              Message => "Boolean value mismatch");

      -- Positive_Test_Boolean_DollarRef
      Assert (Condition => Evaluate_Boolean
                 (Node        => Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/config/boolean[@name='dependent_feature']"),
                  Backtrace   => Backtrace,
                  Node_Access => Node_Access),
              Message => "Boolean value mismatch");

      --- Fault-Tests
      -- Expression_Case_IntType
      declare
         Dummy : Boolean;
      begin
         Dummy := Evaluate_Boolean
            (Node        => Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/expressions/expression[@name='case3_int']"),
             Backtrace   => Backtrace,
             Node_Access => Node_Access);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Muxml.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "A Boolean variable or expression points to expression "
                       & "with name 'case3_int' which is not Boolean valued",
                    Message   => "Exception message mismatch");
      end;

      -- Expression_EvalString
      declare
         Dummy : Boolean;
      begin
         Dummy := Evaluate_Boolean
            (Node        => Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/expressions/expression[@name='evalStringExpr1']"),
             Backtrace   => Backtrace,
             Node_Access => Node_Access);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Muxml.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "A Boolean variable or expression points to expression "
                       & "with name 'evalStringExpr1' which is not Boolean valued",
                    Message   => "Exception message mismatch");
      end;

      -- Config_String
      declare
         Dummy : Boolean;
      begin
         Dummy := Evaluate_Boolean
            (Node        => Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/config/string[@name='foo']"),
             Backtrace   => Backtrace,
             Node_Access => Node_Access);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Muxml.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "A Boolean variable or expression points to node "
                       & "with type 'string' which is not Boolean valued",
                    Message   => "Exception message mismatch");
      end;

   --  begin read only
   end Test_Evaluate_Boolean;
   --  end read only


--  begin read only
   procedure Test_Evaluate_Integer (Gnattest_T : in out Test);
   procedure Test_Evaluate_Integer_91250c (Gnattest_T : in out Test) renames Test_Evaluate_Integer;
--  id:2.2/91250c0dd1fcf6a8/Evaluate_Integer/1/0/
   procedure Test_Evaluate_Integer (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data        : Muxml.XML_Data_Type;
      Node_Access : Access_Hashmaps_Type;
      Backtrace   : String_Vector.Vector;

   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/test_policy_src_evalStringConcatCase.xml");
      Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

      -- Positive_Test: Expression_Case
      Assert (Condition => -4 =  Evaluate_Integer
                 (Node        => Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/expressions/expression[@name='case3_int']"),
                  Backtrace   => Backtrace,
                  Node_Access => Node_Access),
              Message => "Integer value mismatch");

      -- Positive_Test: Integer_Constant
      -- session_count is already evaluated - delete it!
      Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);
      Assert (Condition => 4 = Evaluate_Integer
                 (Node        => Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/config/integer[@name='session_count']"),
                  Backtrace   => Backtrace,
                  Node_Access => Node_Access),
              Message => "Integer value mismatch");

      -- Positive_Test: Integer_DollarRef
      Assert (Condition => 4 = Evaluate_Integer
                 (Node        => Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/config/integer[@name='scount']"),
                  Backtrace   => Backtrace,
                  Node_Access => Node_Access),
              Message => "Integer value mismatch");

      --- Fault-Tests
      -- Expression_Case_Bool
      declare
         Dummy : Integer;
      begin
         Dummy := Evaluate_Integer
            (Node        => Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/expressions/expression[@name='case2_bool']"),
             Backtrace   => Backtrace,
             Node_Access => Node_Access);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Muxml.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "An integer variable or expression points to expression "
                       & "with name 'case2_bool' which is not integer valued",
                    Message   => "Exception message mismatch");
      end;

      -- Expression_EvalString
      declare
         Dummy : Integer;
      begin
         Dummy := Evaluate_Integer
            (Node        => Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/expressions/expression[@name='evalStringExpr1']"),
             Backtrace   => Backtrace,
             Node_Access => Node_Access);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Muxml.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "An integer variable or expression points to expression "
                       & "with name 'evalStringExpr1' which is not integer valued",
                    Message   => "Exception message mismatch");
      end;

      -- Config_String
      declare
         Dummy : Integer;
      begin
         Dummy := Evaluate_Integer
            (Node        => Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/config/string[@name='foo']"),
             Backtrace   => Backtrace,
             Node_Access => Node_Access);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Muxml.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "An integer variable or expression points to node "
                       & "with type 'string' which is not integer valued",
                    Message   => "Exception message mismatch");
      end;

--  begin read only
   end Test_Evaluate_Integer;
--  end read only


--  begin read only
   procedure Test_Evaluate_String (Gnattest_T : in out Test);
   procedure Test_Evaluate_String_5296ba (Gnattest_T : in out Test) renames Test_Evaluate_String;
--  id:2.2/5296bac2d0226064/Evaluate_String/1/0/
   procedure Test_Evaluate_String (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data        : Muxml.XML_Data_Type;
      Node_Access : Access_Hashmaps_Type;
      Backtrace   : String_Vector.Vector;

   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/test_policy_src_evalStringConcatCase.xml");

      Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

      -- Positive_Test: Expression_Case
      Assert (Condition => "foobar" = Evaluate_String
                 (Node        => Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/expressions/expression[@name='case1_string']"),
                  Backtrace   => Backtrace,
                  Node_Access => Node_Access),
              Message => "String value mismatch");

      -- Positive_Test: Expression_Concat
      Assert (Condition => "" = Evaluate_String
                 (Node        => Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/expressions/expression[@name='empty']"),
                  Backtrace   => Backtrace,
                  Node_Access => Node_Access),
              Message => "String value mismatch");


      -- Positive_Test: Expression_EvalString
      Assert (Condition => "foobar{" = Evaluate_String
                 (Node        => Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/expressions/expression[@name='evalStringExpr4']"),
                  Backtrace   => Backtrace,
                  Node_Access => Node_Access),
              Message => "String value mismatch");

      -- Positive_Test: String_Constant
      Assert (Condition => "myname" = Evaluate_String
                 (Node        => Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/config/string[@name='name1']"),
                  Backtrace   => Backtrace,
                  Node_Access => Node_Access),
              Message => "Boolean value mismatch");

      -- Positive_Test: String_DollarRef
      Assert (Condition => "myname" = Evaluate_String
                 (Node        => Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/config/string[@name='name2']"),
                  Backtrace   => Backtrace,
                  Node_Access => Node_Access),
              Message => "Boolean value mismatch");

      --- Fault-Tests
      -- Expression_Case_IntType
      declare
         Dummy : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Dummy := Ada.Strings.Unbounded.To_Unbounded_String
            (Evaluate_String
                (Node        => Muxml.Utils.Get_Element
                    (Doc   => Data.Doc,
                     XPath => "/system/expressions/expression[@name='case3_int']"),
                 Backtrace   => Backtrace,
                 Node_Access => Node_Access));
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Muxml.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "A string variable or expression points to expression "
                       & "with name 'case3_int' which is not string valued",
                    Message   => "Exception message mismatch");
      end;

      -- Expression_Boolean
      declare
         Dummy : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Dummy := Ada.Strings.Unbounded.To_Unbounded_String
            (Evaluate_String
                (Node        => Muxml.Utils.Get_Element
                    (Doc   => Data.Doc,
                     XPath => "/system/expressions/expression[@name='not_false']"),
                 Backtrace   => Backtrace,
                 Node_Access => Node_Access));
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Muxml.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "A string variable or expression points to expression "
                       & "with name 'not_false' which is not string valued",
                    Message   => "Exception message mismatch");
      end;

      -- Config_Boolean
      declare
         Dummy : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Dummy := Ada.Strings.Unbounded.To_Unbounded_String
            (Evaluate_String
                (Node        => Muxml.Utils.Get_Element
                    (Doc   => Data.Doc,
                     XPath => "/system/config/boolean[@name='feature_enabled']"),
                 Backtrace   => Backtrace,
                 Node_Access => Node_Access));
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Muxml.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "A string variable or expression points to node "
                       & "with type 'boolean' which is not string valued",
                    Message   => "Exception message mismatch");
      end;


--  begin read only
   end Test_Evaluate_String;
--  end read only


--  begin read only
   procedure Test_Boolean_Expression (Gnattest_T : in out Test);
   procedure Test_Boolean_Expression_f912b6 (Gnattest_T : in out Test) renames Test_Boolean_Expression;
--  id:2.2/f912b62486f97dd7/Boolean_Expression/1/0/
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
            Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
               (Data        => Data,
                Node_Access => Node_Access);
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
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

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
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

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
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

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
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

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
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

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
             File => "data/test_policy_src_evalStringConcatCase.xml");
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

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

         -- we have to reinitialize because functions like Bool_Value lookup
         -- values in Node_Access.Output. Hence, Set_Attribute has no effect
         -- without reinitializing.
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);
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
   procedure Test_String_Expression_bf0374 (Gnattest_T : in out Test) renames Test_String_Expression;
--  id:2.2/bf037476627840c0/String_Expression/1/0/
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
                      File => "data/test_policy_src_evalStringConcatCase.xml");
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

         Assert (Condition => "start_mynamemynamefoobar" = String_Expression
                   (Backtrace => Backtrace,
                    Node_Access => Node_Access,
                    Node   => Muxml.Utils.Get_Element
                       (Doc   => Data.Doc,
                        XPath => "/system/expressions/expression"
                        & "[@name='myconcatenation']")),
                 Message   => "Expression value mismatch");

         Assert (Condition => "/path/../to/*[@name='stuff']//node" = String_Expression
                    (Backtrace => Backtrace,
                     Node_Access => Node_Access,
                     Node   => Muxml.Utils.Get_Element
                        (Doc   => Data.Doc,
                         XPath => "/system/expressions/expression"
                            & "[@name='evalStringExpr2']")),
                 Message   => "Expression value mismatch");
      end Positive_Test;

      ----------------------------------------------------------------------

      procedure Concat_No_Child
      is
         Data : Muxml.XML_Data_Type;
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src_concatenation.xml");
        Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

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
      end Concat_No_Child;

      ----------------------------------------------------------------------

      procedure Concat_One_Child
      is
         Data : Muxml.XML_Data_Type;
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src_concatenation.xml");
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

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
      end Concat_One_Child;

      ----------------------------------------------------------------------

      procedure Eval_String_Ref_To_Empty
      is
         Data : Muxml.XML_Data_Type;
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src_evalStringConcatCase.xml");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression"
                & "[@name='evalStringExpr1']/evalString",
             Name  => "value",
             Value => "${}");
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

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
                           & "[@name='evalStringExpr1']")));
            Assert (Condition => False,
                    Message   => "Exception expected");
         exception
            when E : Muxml.Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                          = "A variable or expression points to '' which does not exit",
                       Message   => "Exception message mismatch "
                          & "(reference to empty name)");
         end;
      end Eval_String_Ref_To_Empty;

      ----------------------------------------------------------------------

      procedure No_String_Expr
      is
         Data : Muxml.XML_Data_Type;
         Backtrace : String_Vector.Vector;
         Node_Access :  Access_Hashmaps_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src_evalStringConcatCase.xml");
         Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

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
                           & "[@name='session2_enabled']")));
            Assert (Condition => False,
                    Message   => "Exception expected");
         exception
            when E : Invalid_Expression =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                          = "String-expression 'session2_enabled' has an "
                          & "unknown child operation with name 'gt'",
                       Message   => "Exception message mismatch "
                          & "(ref with empty name)");
         end;
      end No_String_Expr;
   begin
      Positive_Test;
      Concat_No_Child;
      Concat_One_Child;
      Eval_String_Ref_To_Empty;
      No_String_Expr;

--  begin read only
   end Test_String_Expression;
--  end read only


--  begin read only
   procedure Test_Get_Defining_Node (Gnattest_T : in out Test);
   procedure Test_Get_Defining_Node_dc856d (Gnattest_T : in out Test) renames Test_Get_Defining_Node;
--  id:2.2/dc856de415d6f425/Get_Defining_Node/1/0/
   procedure Test_Get_Defining_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data        : Muxml.XML_Data_Type;
      Node_Access :  Access_Hashmaps_Type;
      Node        : DOM.Core.Node;
   begin
      -- Positive test
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/test_policy_src_evalStringConcatCase.xml");
      Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);

      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/expressions/expression"
             & "[@name='foobar']");
      Assert (Condition => Node = Get_Defining_Node
                 (Var_Name => "foobar",
                  Node_Access => Node_Access),
              Message => "Node mismatch.");

      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/config/string[@name='name1']");
      Assert (Condition => Node = Get_Defining_Node
                 (Var_Name => "name1",
                  Node_Access => Node_Access),
              Message => "Node mismatch.");

      -- negative test
      Assert (Condition => Node = Get_Defining_Node
                 (Var_Name => "name9",
                  Node_Access => Node_Access),
              Message => "Node mismatch.");
      Assert (Condition => False,
              Message => "Exception expected");
   exception
      when E : Muxml.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "A variable or expression points to 'name9' which does not exit",
                 Message   => "Exception message mismatch ");
         --  begin read only
   end Test_Get_Defining_Node;
--  end read only


--  begin read only
   procedure Test_Get_Expr_Type (Gnattest_T : in out Test);
   procedure Test_Get_Expr_Type_5baff0 (Gnattest_T : in out Test) renames Test_Get_Expr_Type;
--  id:2.2/5baff00e3ef020b5/Get_Expr_Type/1/0/
   procedure Test_Get_Expr_Type (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data        : Muxml.XML_Data_Type;
      Node        : DOM.Core.Node;

   begin
      -- Positive tests
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/test_policy_src_evalStringConcatCase.xml");

      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/expressions/expression[@name='myconcatenation']");
      Assert (Condition => String_Expr_Type = Get_Expr_Type
                 (Expr => Node),
              Message => "Type mismatch.");
      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/expressions/expression[@name='evalStringExpr1']");
      Assert (Condition => String_Expr_Type = Get_Expr_Type
                 (Expr => Node),
              Message => "Type mismatch.");
      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/expressions/expression[@name='case2_bool']");
      Assert (Condition => Case_Expr_Type = Get_Expr_Type
                 (Expr => Node),
              Message => "Type mismatch.");
      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/expressions/expression[@name='int_values_2']");
      Assert (Condition => Boolean_Expr_Type = Get_Expr_Type
                 (Expr => Node),
              Message => "Type mismatch.");

      --negative tests
      declare
         Dummy : Expression_Toplevel_Type;
      begin
         Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions");
         Dummy := Get_Expr_Type (Expr => Node);
         Assert (Condition => False,
                 Message => "Exception expected");
      exception
         when E : Invalid_Expression =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Expression with name '' begins with illegal operator 'expression'",
                    Message   => "Exception message mismatch ");
      end;
      declare
         Dummy : Expression_Toplevel_Type;
      begin
         Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='myconcatenation']/"
                & "concatenation/variable[@name='name1']");
         Dummy := Get_Expr_Type (Expr => Node);
         Assert (Condition => False,
                 Message => "Exception expected");
      exception
         when E : Invalid_Expression =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Expression with name 'name1' is empty",
                    Message   => "Exception message mismatch ");
      end;

   --  begin read only
   end Test_Get_Expr_Type;
   --  end read only


--  begin read only
   procedure Test_Get_Nth_Child_Node (Gnattest_T : in out Test);
   procedure Test_Get_Nth_Child_Node_c65a86 (Gnattest_T : in out Test) renames Test_Get_Nth_Child_Node;
--  id:2.2/c65a861f50565975/Get_Nth_Child_Node/1/0/
   procedure Test_Get_Nth_Child_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data        : Muxml.XML_Data_Type;
      Node        : DOM.Core.Node;

   begin
      -- Positive tests
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/test_policy_src_evalStringConcatCase.xml");

      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/expressions/expression[@name='evalStringExpr2']");
      Assert (Condition => Node = Get_Nth_Child_Node
                 (Parent => Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/expressions"),
                  N      => 3),
              Message => "Node mismatch.");
      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/expressions/expression[@name='myconcatenation']");
      Assert (Condition => Node = Get_Nth_Child_Node
                 (Parent => Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/expressions"),
                  N      => 1),
              Message => "Node mismatch.");
      Assert (Condition => null = Get_Nth_Child_Node
                 (Parent => Muxml.Utils.Get_Element
                     (Doc   => Data.Doc,
                      XPath => "/system/config/string[@name='name1']"),
                  N      => 1),
              Message => "Node mismatch.");


   --  begin read only
   end Test_Get_Nth_Child_Node;
--  end read only


--  begin read only
   procedure Test_Expand_Single_Node (Gnattest_T : in out Test);
   procedure Test_Expand_Single_Node_61a401 (Gnattest_T : in out Test) renames Test_Expand_Single_Node;
--  id:2.2/61a401c399414e5f/Expand_Single_Node/1/0/
   procedure Test_Expand_Single_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data        : Muxml.XML_Data_Type;
      Node_Access :  Access_Hashmaps_Type;
      Backtrace   : String_Vector.Vector;
      Node        : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/test_policy_src_evalStringConcatCase.xml");
      Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);


      -- positive tests
      Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/expressions/expression[@name='session2_enabled']");
      Expand_Single_Node
         (Node        => Node,
          Backtrace   => Backtrace,
          Node_Access => Node_Access);
      Assert (Condition => Node_Access.Output_Boolean.Contains ("session2_enabled"),
              Message => "Missing containment in output");
      Assert (Condition => Node_Access.Output_Boolean ("session2_enabled") = True,
              Message => "Value mismatch.");

      Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/config/string[@name='name3']");
      Expand_Single_Node
         (Node        => Node,
          Backtrace   => Backtrace,
          Node_Access => Node_Access);
      Assert (Condition => Node_Access.Output_String.Contains ("name3"),
              Message => "Missing containment in output");
      Assert (Condition => Node_Access.Output_String ("name3") = "myname",
              Message => "Value mismatch.");

      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/config/integer[@name='session_count']");
      Mutools.Expressions.Test_Data.Initialize_Node_Access_Testing
            (Data        => Data,
             Node_Access => Node_Access);
      Expand_Single_Node
         (Node        => Node,
          Backtrace   => Backtrace,
          Node_Access => Node_Access);
      Assert (Condition => Node_Access.Output_Integer.Contains ("session_count"),
              Message => "Missing containment in output");
      Assert (Condition => Node_Access.Output_Integer ("session_count") = 4,
              Message => "Value mismatch.");

      --- negative test
      -- attempt to expand text-node
      declare
      begin
          Node := Muxml.Utils.Get_Element
             (Doc   => Data.Doc,
              XPath => "/system/config");
          Node := DOM.Core.Nodes.First_Child (Node);

          Expand_Single_Node
             (Node        => Node,
              Backtrace   => Backtrace,
              Node_Access => Node_Access);
          Assert (Condition => False,
                  Message   => "Exception expected");
      exception
         when E : Invalid_Expression =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Invalid config-variable or expression with type '#text'",
                    Message   => "Exception message mismatch");
      end;

   --  begin read only
   end Test_Expand_Single_Node;
--  end read only


--  begin read only
   procedure Test_Add_To_Backtrace (Gnattest_T : in out Test);
   procedure Test_Add_To_Backtrace_d9db79 (Gnattest_T : in out Test) renames Test_Add_To_Backtrace;
--  id:2.2/d9db79bb9550c8f0/Add_To_Backtrace/1/0/
   procedure Test_Add_To_Backtrace (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Backtrace   : String_Vector.Vector;
   begin
      -- add two elements to empty backtrace
      Add_To_Backtrace (Backtrace => Backtrace, Name => "n1");
      Add_To_Backtrace (Backtrace => Backtrace, Name => "n2");
      Add_To_Backtrace (Backtrace => Backtrace, Name => "n3");
      Assert (Condition => String_Vector.Contains
                 (Container => Backtrace,
                  Item      => "n1"),
              Message => "Containment mismatch");
      Assert (Condition => String_Vector.Contains
                 (Container => Backtrace,
                  Item      => "n2"),
              Message => "Containment mismatch");
      Assert (Condition => String_Vector.Contains
                 (Container => Backtrace,
                  Item      => "n3"),
              Message => "Containment mismatch");

      -- trigger exception by adding an element already contained
      Add_To_Backtrace (Backtrace => Backtrace, Name => "n1");
      Assert (Condition => False,
              Message => "Exception expected");

   exception
      when E : Invalid_Expression =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Resolving the value of node with name 'n1' lead to "
                    & "cyclic dependency: n1 > n2 > n3 > n1",
                 Message   => "Exception message mismatch");

--  begin read only
   end Test_Add_To_Backtrace;
--  end read only


--  begin read only
   procedure Test_Initialize_Node_Access (Gnattest_T : in out Test);
   procedure Test_Initialize_Node_Access_1ab9eb (Gnattest_T : in out Test) renames Test_Initialize_Node_Access;
--  id:2.2/1ab9eb02d5c40ed3/Initialize_Node_Access/1/0/
   procedure Test_Initialize_Node_Access (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data             : Muxml.XML_Data_Type;
      Node_Access      : Access_Hashmaps_Type;
      Config_And_Exprs : DOM.Core.Node_List;
   begin
      -- positive test
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/test_policy_src_evalStringConcatCase.xml");
      Config_And_Exprs := McKae.XML.XPath.XIA.XPath_Query
         (N     => Data.Doc,
          XPath =>  "/*/config/boolean | "
             & "/*/config/integer ");
      Initialize_Node_Access
         (Node_Access      => Node_Access,
          Config_And_Exprs => Config_And_Exprs);
      Assert (Condition => Node_Access.Input.Contains ("feature_enabled"),
              Message => "Containment mismatch");
      Assert (Condition => Node_Access.Input.Contains ("dependent_feature"),
              Message => "Containment mismatch");
      Assert (Condition => Node_Access.Input.Contains ("scount"),
              Message => "Containment mismatch");
      Assert (Condition => Node_Access.Input.Contains ("session_count"),
              Message => "Containment mismatch");

      -- check that Node_Access.Output is empty
      Assert (Condition => Node_Access.Output_Boolean.Is_Empty,
              Message => "Containment mismatch");
      Assert (Condition => Node_Access.Output_Integer.Is_Empty,
              Message => "Containment mismatch");
      Assert (Condition => Node_Access.Output_String.Is_Empty,
              Message => "Containment mismatch");

      -- attempt to add variable with empty name
      Muxml.Utils.Set_Attribute
         (Doc   => Data.Doc,
          XPath => "/system/config/string[@name='foo']",
          Name  => "name",
          Value => "");
      Config_And_Exprs := McKae.XML.XPath.XIA.XPath_Query
         (N     => Data.Doc,
          XPath =>  "/*/config/string");
      Initialize_Node_Access
         (Node_Access      => Node_Access,
          Config_And_Exprs => Config_And_Exprs);
      Assert (Condition => False,
              Message => "Exception expected");
   exception
      when E : Invalid_Expression =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Found empty variable or expression name",
                 Message   => "Exception message mismatch");

--  begin read only
   end Test_Initialize_Node_Access;
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
