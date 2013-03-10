-----------------------------------------------------------------------
--  AUnit utils - Helper for writing unit tests
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Calendar;

with GNAT.Source_Info;

with Util.Properties;
with Util.Assertions;
with Util.XUnit;
package Util.Tests is

   use Ada.Strings.Unbounded;

   subtype Message_String is Util.XUnit.Message_String;
   subtype Test_Case is Util.XUnit.Test_Case;
   subtype Test_Suite is Util.XUnit.Test_Suite;
   subtype Access_Test_Suite is Util.XUnit.Access_Test_Suite;

   function Format (S : in String) return Message_String renames Util.XUnit.Format;

   type Test is new Util.XUnit.Test with null record;

   --  Get a path to access a test file.
   function Get_Path (File : String) return String;

   --  Get a path to create a test file.
   function Get_Test_Path (File : String) return String;

   --  Get the timeout for the test execution.
   function Get_Test_Timeout (Name : in String) return Duration;

   --  Get the testsuite harness prefix.  This prefix is added to the test class name.
   --  By default it is empty.  It is allows to execute the test harness on different
   --  environment (ex: MySQL or SQLlite) and be able to merge and collect the two result
   --  sets together.
   function Get_Harness_Prefix return String;

   --  Get a test configuration parameter.
   function Get_Parameter (Name    : String;
                           Default : String := "") return String;

   --  Get the test configuration properties.
   function Get_Properties return Util.Properties.Manager;

   --  Get a new unique string
   function Get_Uuid return String;

   --  Get the verbose flag that can be activated with the <tt>-v</tt> option.
   function Verbose return Boolean;

   --  Check that two files are equal.  This is intended to be used by
   --  tests that create files that are then checked against patterns.
   procedure Assert_Equal_Files (T       : in Test_Case'Class;
                                 Expect  : in String;
                                 Test    : in String;
                                 Message : in String := "Test failed";
                                 Source  : String := GNAT.Source_Info.File;
                                 Line    : Natural := GNAT.Source_Info.Line);

   --  Check that the value matches what we expect.
   procedure Assert_Equals is new Assertions.Assert_Equals_T (Value_Type => Integer);
   procedure Assert_Equals is new Assertions.Assert_Equals_T (Value_Type => Character);
   procedure Assert_Equals is new Assertions.Assert_Equals_T (Value_Type => Long_Long_Integer);

   --  Check that the value matches what we expect.
--     procedure Assert (T         : in Test'Class;
--                       Condition : in Boolean;
--                       Message   : in String := "Test failed";
--                       Source    : String := GNAT.Source_Info.File;
--                       Line      : Natural := GNAT.Source_Info.Line);

   --  Check that the value matches what we expect.
   procedure Assert_Equals (T         : in Test'Class;
                            Expect, Value : in Ada.Calendar.Time;
                            Message   : in String := "Test failed";
                            Source    : String := GNAT.Source_Info.File;
                            Line      : Natural := GNAT.Source_Info.Line);

   --  Check that the value matches what we expect.
   procedure Assert_Equals (T         : in Test'Class;
                            Expect, Value : in String;
                            Message   : in String := "Test failed";
                            Source    : String := GNAT.Source_Info.File;
                            Line      : Natural := GNAT.Source_Info.Line);

   --  Check that the value matches what we expect.
   procedure Assert_Equals (T       : in Test'Class;
                            Expect  : in String;
                            Value   : in Unbounded_String;
                            Message : in String := "Test failed";
                            Source    : String := GNAT.Source_Info.File;
                            Line      : Natural := GNAT.Source_Info.Line);

   --  Check that the value matches the regular expression
   procedure Assert_Matches (T       : in Test'Class;
                             Pattern : in String;
                             Value   : in Unbounded_String;
                             Message : in String := "Test failed";
                             Source  : String := GNAT.Source_Info.File;
                             Line    : Natural := GNAT.Source_Info.Line);

   --  Check that the value matches the regular expression
   procedure Assert_Matches (T       : in Test'Class;
                             Pattern : in String;
                             Value   : in String;
                             Message : in String := "Test failed";
                             Source  : String := GNAT.Source_Info.File;
                             Line    : Natural := GNAT.Source_Info.Line);

   --  Check that the file exists.
   procedure Assert_Exists (T        : in Test'Class;
                            File     : in String;
                            Message : in String := "Test failed";
                            Source  : String := GNAT.Source_Info.File;
                            Line    : Natural := GNAT.Source_Info.Line);

   --  Default initialization procedure.
   procedure Initialize_Test (Props : in Util.Properties.Manager);

   --  The main testsuite program.  This launches the tests, collects the
   --  results, create performance logs and set the program exit status
   --  according to the testsuite execution status.
   --
   --  The <b>Initialize</b> procedure is called before launching the unit tests.  It is intended
   --  to configure the tests according to some external environment (paths, database access).
   --
   --  The <b>Finish</b> procedure is called after the test suite has executed.
   generic
      with function Suite return Access_Test_Suite;
      with procedure Initialize (Props : in Util.Properties.Manager) is Initialize_Test;
      with procedure Finish (Status : in Util.XUnit.Status) is null;
   procedure Harness (Name : in String);

end Util.Tests;
