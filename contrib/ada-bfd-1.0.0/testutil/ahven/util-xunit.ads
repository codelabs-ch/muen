-----------------------------------------------------------------------
--  util-xunit - Unit tests on top of AHven
--  Copyright (C) 2011 Stephane Carrez
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

with Ahven;
with Ahven.Framework;
with Ahven.Results;

with Ada.Strings.Unbounded;

with GNAT.Source_Info;

--  The <b>Util.XUnit</b> package exposes a common package definition used by the Ada testutil
--  library.  This implementation exposes an implementation on top of Ahven.
--
--  Ahven is written by Tero Koskinen and licensed under permissive ISC license.
--  See http://ahven.stronglytyped.org/
package Util.XUnit is

   use Ada.Strings.Unbounded;

   type Status is (Success, Failure);

   subtype Message_String is String;
   subtype Test_Suite is Ahven.Framework.Test_Suite;
   type Access_Test_Suite is access all Test_Suite;

   type Test_Access is access all Ahven.Framework.Test_Case'Class;

   type Test_Object;
   type Test_Object_Access is access all Test_Object;

   type Test_Object is record
      Test : Test_Access;
      Next : Test_Object_Access;
   end record;

   --  Register a test object in the test suite.
   procedure Register (T : in Test_Object_Access);

   --  Build a message from a string (Adaptation for AUnit API).
   function Format (S : in String) return Message_String;

   --  Build a message with the source and line number.
   function Build_Message (Message   : in String;
                           Source    : in String;
                           Line      : in Natural) return String;

   --  ------------------------------
   --  A simple test case
   --  ------------------------------
   type Test_Case is abstract new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test_Case);

   procedure Assert (T         : in Test_Case;
                     Condition : in Boolean;
                     Message   : in String := "Test failed";
                     Source    : in String := GNAT.Source_Info.File;
                     Line      : in Natural := GNAT.Source_Info.Line);

   --  Return the name of the test case.
   overriding
   function Get_Name (T : Test_Case) return String;

   --  Test case name (this is the AUnit function that must be implemented).
   function Name (T : in Test_Case) return Message_String is abstract;

   --  Perform the test (AUnit function to implement).
   procedure Run_Test (T : in out Test_Case) is abstract;

   --  ------------------------------
   --  A test with fixture
   --  ------------------------------
   type Test is new Ahven.Framework.Test_Case with null record;

   --  Check that the value matches what we expect.
   procedure Assert (T         : in Test;
                     Condition : in Boolean;
                     Message   : in String := "Test failed";
                     Source    : String := GNAT.Source_Info.File;
                     Line      : Natural := GNAT.Source_Info.Line);

   --  Report passes, skips, failures, and errors from the result collection.
   procedure Report_Results (Result  : in Ahven.Results.Result_Collection;
                             Time    : in Duration);

   --  The main testsuite program.  This launches the tests, collects the
   --  results, create performance logs and set the program exit status
   --  according to the testsuite execution status.
   generic
      with function Suite return Access_Test_Suite;
   procedure Harness (Output : in Ada.Strings.Unbounded.Unbounded_String;
                      XML    : in Boolean;
                      Result : out Status);

end Util.XUnit;
