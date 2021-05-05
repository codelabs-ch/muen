--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Config.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Mucfgcheck.Validation_Errors;
--  begin read only
--  end read only
package body Mucfgcheck.Config.Test_Data.Tests is

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

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise exception.

      Name_Uniqueness (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Duplicate config entry.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/config/boolean[@name='debug_enabled']",
         Name  => "name",
         Value => "feature_enabled");

      Name_Uniqueness (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Multiple config variables with name 'feature_enabled'"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Config_Boolean_Values (Gnattest_T : in out Test);
   procedure Test_Config_Boolean_Values_7f9172 (Gnattest_T : in out Test) renames Test_Config_Boolean_Values;
--  id:2.2/7f917295e735f62f/Config_Boolean_Values/1/0/
   procedure Test_Config_Boolean_Values (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/test_policy_src.xml");

      --  Positive test, must not raise an exception.

      Config_Boolean_Values (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Set non-boolean value.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/config/boolean[@name='feature_enabled']",
         Name  => "value",
         Value => "foobar");

      Config_Boolean_Values (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Boolean with invalid value 'foobar' in config variable "
               & "'feature_enabled'"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Config_Boolean_Values;
--  end read only


--  begin read only
   procedure Test_Config_Integer_Values (Gnattest_T : in out Test);
   procedure Test_Config_Integer_Values_5d87a6 (Gnattest_T : in out Test) renames Test_Config_Integer_Values;
--  id:2.2/5d87a6f7a7b9ae35/Config_Integer_Values/1/0/
   procedure Test_Config_Integer_Values (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/test_policy_src.xml");

      --  Positive test, must not raise an exception.

      Config_Integer_Values (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Set non-numeric value.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/config/integer[@name='session_count']",
         Name  => "value",
         Value => "foobar");

      Config_Integer_Values (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Integer with invalid value 'foobar' in config variable "
               & "'session_count'"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Config_Integer_Values;
--  end read only


--  begin read only
   procedure Test_Expression_Config_Var_Refs (Gnattest_T : in out Test);
   procedure Test_Expression_Config_Var_Refs_ea351f (Gnattest_T : in out Test) renames Test_Expression_Config_Var_Refs;
--  id:2.2/ea351faa18a7216f/Expression_Config_Var_Refs/1/0/
   procedure Test_Expression_Config_Var_Refs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/test_policy_src.xml");

      --  Must not raise an exception.

      Expression_Config_Var_Refs (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Set reference to nonexistent config var.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/expressions/expression/gt/variable"
         & "[@name='session_count']",
         Name  => "name",
         Value => "nonexistent");

      Expression_Config_Var_Refs (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Config variable 'nonexistent' referenced in expression "
               & "'session2_enabled' not defined"),
              Message   => "Exception mismatch (1)");

      --  Remove name attribute from variable reference.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/expressions/expression/gt/variable"
         & "[@name='nonexistent']",
         Name  => "name",
         Value => "");

      Expression_Config_Var_Refs (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Config variable without name attribute in expression "
               & "'session2_enabled'"),
              Message   => "Exception mismatch (2)");
--  begin read only
   end Test_Expression_Config_Var_Refs;
--  end read only


--  begin read only
   procedure Test_Expression_Integer_Values (Gnattest_T : in out Test);
   procedure Test_Expression_Integer_Values_8910e7 (Gnattest_T : in out Test) renames Test_Expression_Integer_Values;
--  id:2.2/8910e7ea682924bf/Expression_Integer_Values/1/0/
   procedure Test_Expression_Integer_Values (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/test_policy_src.xml");

      --  Positive test, must not raise an exception.

      Expression_Integer_Values (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Set non-integer value.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/expressions/expression/gt/integer"
         & "[@value='1']",
         Name  => "value",
         Value => "foobar");

      Expression_Integer_Values (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Integer with invalid value 'foobar' in expression "
               & "'session2_enabled'"),
              Message   => "Exception mismatch (1)");

      --  Remove value attribute from integer element.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/expressions/expression/gt/integer"
         & "[@value='foobar']",
         Name  => "value",
         Value => "");

      Expression_Integer_Values (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Integer without value attribute in expression "
               & "'session2_enabled'"),
              Message   => "Exception mismatch (2)");
--  begin read only
   end Test_Expression_Integer_Values;
--  end read only


--  begin read only
   procedure Test_Expression_Boolean_Values (Gnattest_T : in out Test);
   procedure Test_Expression_Boolean_Values_6cb38d (Gnattest_T : in out Test) renames Test_Expression_Boolean_Values;
--  id:2.2/6cb38df1a49f5220/Expression_Boolean_Values/1/0/
   procedure Test_Expression_Boolean_Values (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/test_policy_src.xml");

      --  Positive test, must not raise an exception.

      Expression_Boolean_Values (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Set non-boolean value.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/expressions/expression[@name='and_expr']"
         & "/and/boolean",
         Name  => "value",
         Value => "foobar");

      Expression_Boolean_Values (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Boolean with invalid value 'foobar' in expression "
               & "'and_expr'"),
              Message   => "Exception mismatch (1)");

      --  Remove value attribute from integer element.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/expressions/expression[@name='and_expr']"
         & "/and/boolean",
         Name  => "value",
         Value => "");

      Expression_Boolean_Values (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Boolean without value attribute in expression "
               & "'and_expr'"),
              Message   => "Exception mismatch (2)");
--  begin read only
   end Test_Expression_Boolean_Values;
--  end read only


--  begin read only
   procedure Test_Conditional_Config_Var_Refs (Gnattest_T : in out Test);
   procedure Test_Conditional_Config_Var_Refs_628dfb (Gnattest_T : in out Test) renames Test_Conditional_Config_Var_Refs;
--  id:2.2/628dfbb40f272bc7/Conditional_Config_Var_Refs/1/0/
   procedure Test_Conditional_Config_Var_Refs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/test_policy_src.xml");

      --  Must not raise an exception.

      Conditional_Config_Var_Refs (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Set reference to nonexistent config var.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/if[@variable='feature_enabled']",
         Name  => "variable",
         Value => "nonexistent");

      Conditional_Config_Var_Refs (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Config variable 'nonexistent' referenced by conditional"
               & " not defined"),
              Message   => "Exception mismatch (1)");

      --  Remove variable attribute from variable reference.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/if[@variable='nonexistent']",
         Name  => "variable",
         Value => "");

      Conditional_Config_Var_Refs (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Conditional without variable attribute"),
              Message   => "Exception mismatch (2)");
--  begin read only
   end Test_Conditional_Config_Var_Refs;
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
end Mucfgcheck.Config.Test_Data.Tests;
