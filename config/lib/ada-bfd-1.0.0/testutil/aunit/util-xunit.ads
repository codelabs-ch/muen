-----------------------------------------------------------------------
--  util-xunit - Unit tests on top of AUnit
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

with AUnit;
with AUnit.Simple_Test_Cases;
with AUnit.Test_Suites;
with AUnit.Test_Fixtures;
with Ada.Strings.Unbounded;

with GNAT.Source_Info;

--  The <b>Util.XUnit</b> package exposes a common package definition used by the Ada testutil
--  library.  It is intended to hide the details of the AUnit implementation.
--  A quite identical package exist for Ahven implementation.
package Util.XUnit is

   use Ada.Strings.Unbounded;
   use AUnit.Test_Suites;

   subtype Status is AUnit.Status;

   Success : constant Status := AUnit.Success;
   Failure : constant Status := AUnit.Failure;

   subtype Message_String is AUnit.Message_String;
   subtype Test_Suite is AUnit.Test_Suites.Test_Suite;
   subtype Access_Test_Suite is AUnit.Test_Suites.Access_Test_Suite;

   function Format (S : in String) return Message_String renames AUnit.Format;

   type Test_Case is abstract new AUnit.Simple_Test_Cases.Test_Case with null record;

   --  maybe_overriding
   procedure Assert (T         : in Test_Case;
                     Condition : in Boolean;
                     Message   : in String := "Test failed";
                     Source    : in String := GNAT.Source_Info.File;
                     Line      : in Natural := GNAT.Source_Info.Line);

   type Test is abstract new AUnit.Test_Fixtures.Test_Fixture with null record;

   --  maybe_overriding
   procedure Assert (T         : in Test;
                     Condition : in Boolean;
                     Message   : in String := "Test failed";
                     Source    : in String := GNAT.Source_Info.File;
                     Line      : in Natural := GNAT.Source_Info.Line);

   --  The main testsuite program.  This launches the tests, collects the
   --  results, create performance logs and set the program exit status
   --  according to the testsuite execution status.
   generic
      with function Suite return Access_Test_Suite;
   procedure Harness (Output : in Ada.Strings.Unbounded.Unbounded_String;
                      XML    : in Boolean;
                      Result : out Status);

end Util.XUnit;
