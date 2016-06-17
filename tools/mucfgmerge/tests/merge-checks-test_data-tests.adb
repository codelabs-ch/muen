--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Merge.Checks.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Merge.Checks.Test_Data.Tests is


--  begin read only
   procedure Test_Required_Config_Values (Gnattest_T : in out Test);
   procedure Test_Required_Config_Values_4d8830 (Gnattest_T : in out Test) renames Test_Required_Config_Values;
--  id:2.2/4d883036bb1806e5/Required_Config_Values/1/0/
   procedure Test_Required_Config_Values (Gnattest_T : in out Test) is
   --  merge-checks.ads:25:4:Required_Config_Values
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Missing_Hardware
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_config.xml");

         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/config/string[@name='hardware']",
            Name  => "name",
            Value => "bar");

         begin
            Required_Config_Values (Policy => Data);
            Assert (Condition => False,
                    Message   => "Exception expected (hw)");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Required string config value 'hardware' missing",
                       Message   => "Exception mismatch (hw)");
         end;
      end Missing_Hardware;

      ----------------------------------------------------------------------

      procedure Missing_Platform
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_config.xml");

         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/config/string[@name='platform']",
            Name  => "name",
            Value => "foo");

         begin
            Required_Config_Values (Policy => Data);
            Assert (Condition => False,
                    Message   => "Exception expected (platform)");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Required string config value 'platform' missing",
                       Message   => "Exception mismatch (platform)");
         end;
      end Missing_Platform;

      ----------------------------------------------------------------------

      procedure Missing_System
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_config.xml");

         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/config/string[@name='system']",
            Name  => "name",
            Value => "foobar");

         begin
            Required_Config_Values (Policy => Data);
            Assert (Condition => False,
                    Message   => "Exception expected (system)");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Required string config value 'system' missing",
                       Message   => "Exception mismatch (system)");
         end;
      end Missing_System;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_config.xml");

         --  Must not raise an exception.

         Required_Config_Values (Policy => Data);
      end Positive_Test;
   begin
      Positive_Test;
      Missing_System;
      Missing_Hardware;
      Missing_Platform;
--  begin read only
   end Test_Required_Config_Values;
--  end read only


--  begin read only
   procedure Test_Expression_Config_Var_Refs (Gnattest_T : in out Test);
   procedure Test_Expression_Config_Var_Refs_ea351f (Gnattest_T : in out Test) renames Test_Expression_Config_Var_Refs;
--  id:2.2/ea351faa18a7216f/Expression_Config_Var_Refs/1/0/
   procedure Test_Expression_Config_Var_Refs (Gnattest_T : in out Test) is
   --  merge-checks.ads:28:4:Expression_Config_Var_Refs
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/test_policy.xml");

      --  Must not raise an exception.

      Expression_Config_Var_Refs (Policy => Data);

      --  Set reference to nonexistent config var.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/expressions/expression/gt/variable"
         & "[@name='session_count']",
         Name  => "name",
         Value => "nonexistent");

      begin
         Expression_Config_Var_Refs (Policy => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Config variable 'nonexistent' referenced in expression "
                    & "'session2_enabled' not defined",
                    Message   => "Exception mismatch (1)");
      end;

      --  Remove name attribute from variable reference.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/expressions/expression/gt/variable"
         & "[@name='nonexistent']",
         Name  => "name",
         Value => "");

      begin
         Expression_Config_Var_Refs (Policy => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Config variable without name attribute in expression "
                    & "'session2_enabled'",
                    Message   => "Exception mismatch (2)");
      end;
--  begin read only
   end Test_Expression_Config_Var_Refs;
--  end read only

end Merge.Checks.Test_Data.Tests;