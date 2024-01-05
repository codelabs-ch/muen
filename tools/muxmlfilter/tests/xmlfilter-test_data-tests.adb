--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Xmlfilter.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Test_Utils;
with Ada.Directories;
with Ada.Exceptions;
with Muxml;
--  begin read only
--  end read only
package body Xmlfilter.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_6c1c8f (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/6c1c8ff63395de3b/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Output : constant String := "obj/output.xml";

      ----------------------------------------------------------------------

      procedure Positive_Tests
      is
      begin
         --  Input and Output with schema names
         Xmlfilter.Run
           (Input_Xml_Path     => "data/component.xml",
            Input_Schema_Name  => "Component_Ext",
            Output_Xml_Path    => Output,
            Output_Schema_Name => "Component",
            Output_Schema_Path => "");
         Assert (Condition => Test_Utils.Equal_Files
                   (Filename1 => "data/output_component.xml",
                    Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);
         Ada.Directories.Delete_File (Name => Output);

         --  Filter from "None" to new schema (use schema path)
         Xmlfilter.Run
           (Input_Xml_Path     => "data/arbitrary_xml.xml",
            Input_Schema_Name  => "",
            Output_Xml_Path    => Output,
            Output_Schema_Name => "",
            Output_Schema_Path => "data/minimal_schema.xsd");
         Assert (Condition => Test_Utils.Equal_Files
                   (Filename1 => "data/output_arbitrary_xml.xml",
                    Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);
         Ada.Directories.Delete_File (Name => Output);
      end Positive_Tests;

      ----------------------------------------------------------------------

      procedure Negative_Test_Not_Schema_Complient
      is
      begin
         Xmlfilter.Run
           (Input_Xml_Path     => "data/arbitrary_xml.xml",
            Input_Schema_Name  => "Component_Ext",
            Output_Xml_Path    => Output,
            Output_Schema_Name => "",
            Output_Schema_Path => "data/minimal_schema.xsd");
         Ada.Directories.Delete_File (Name => Output);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Muxml.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "XML validation error - data/arbitrary_xml.xml:1:12: "
                      & "Value not in the enumeration set",
                    Message   => "Exception message mismatch: "
                   & Ada.Exceptions.Exception_Message (X => E));
      end Negative_Test_Not_Schema_Complient;

      ----------------------------------------------------------------------

      procedure Negative_Test_Output_Schema_Missing
      is
      begin
         Xmlfilter.Run
           (Input_Xml_Path     => "data/component.xml",
            Input_Schema_Name  => "Component_Ext",
            Output_Xml_Path    => Output,
            Output_Schema_Name => "",
            Output_Schema_Path => "");
         Ada.Directories.Delete_File (Name => Output);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Output schema must not be 'None'",
                    Message   => "Exception message mismatch: "
                   & Ada.Exceptions.Exception_Message (X => E));
      end Negative_Test_Output_Schema_Missing;

      ----------------------------------------------------------------------

      procedure Negative_Test_Unknown_Schema_Name
      is
      begin
         Xmlfilter.Run
           (Input_Xml_Path     => "data/component.xml",
            Input_Schema_Name  => "Component_Ext_With_Typo",
            Output_Xml_Path    => Output,
            Output_Schema_Name => "Component_Ext",
            Output_Schema_Path => "");
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Xmlfilter.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Unknown schema name 'Component_Ext_With_Typo'",
                    Message   => "Exception message mismatch: "
                   & Ada.Exceptions.Exception_Message (X => E));
      end Negative_Test_Unknown_Schema_Name;

   begin
      Positive_Tests;
      Negative_Test_Not_Schema_Complient;
      Negative_Test_Output_Schema_Missing;
      Negative_Test_Unknown_Schema_Name;

--  begin read only
   end Test_Run;
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
end Xmlfilter.Test_Data.Tests;
