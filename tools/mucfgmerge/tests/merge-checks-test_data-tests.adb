--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Merge.Checks.Test_Data.

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
package body Merge.Checks.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

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
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Merge.Checks.Test_Data.Tests;
