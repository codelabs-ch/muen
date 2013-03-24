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

with AUnit.Options;
with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Assertions;

with Util.Tests.Reporter;
package body Util.XUnit is

   procedure Assert (T         : in Test_Case;
                     Condition : in Boolean;
                     Message   : in String := "Test failed";
                     Source    : in String := GNAT.Source_Info.File;
                     Line      : in Natural := GNAT.Source_Info.Line) is
   begin
      AUnit.Assertions.Assert (Condition, Message, Source, Line);
   end Assert;

   procedure Assert (T         : in Test;
                     Condition : in Boolean;
                     Message   : in String := "Test failed";
                     Source    : in String := GNAT.Source_Info.File;
                     Line      : in Natural := GNAT.Source_Info.Line) is
   begin
      AUnit.Assertions.Assert (Condition, Message, Source, Line);
   end Assert;

   --  ------------------------------
   --  The main testsuite program.  This launches the tests, collects the
   --  results, create performance logs and set the program exit status
   --  according to the testsuite execution status.
   --  ------------------------------
   procedure Harness (Output : in Ada.Strings.Unbounded.Unbounded_String;
                      XML    : in Boolean;
                      Result : out Status) is
      use type AUnit.Status;

      function Runner is new AUnit.Run.Test_Runner_With_Status (Suite);

      O  : AUnit.Options.AUnit_Options := AUnit.Options.Default_Options;
   begin
      O.Global_Timer    := True;
      O.Test_Case_Timer := True;
      if XML then
         declare
            Reporter : Util.Tests.Reporter.XML_Reporter;
         begin
            Reporter.File := Output;
            Result := Runner (Reporter, O);
         end;
      else
         declare
            Reporter : AUnit.Reporter.Text.Text_Reporter;
         begin
            Result := Runner (Reporter, O);
         end;
      end if;
   end Harness;

end Util.XUnit;
