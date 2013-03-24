-----------------------------------------------------------------------
--  AUnit utils - Helper for writing unit tests
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

with AUnit.Assertions;
package body Util.Assertions is

   --  ------------------------------
   --  Check that the value matches what we expect.
   --  ------------------------------
   procedure Assert_Equals_T (T       : in AUnit.Assertions.Test'Class;
                              Expect  : in Value_Type;
                              Value   : in Value_Type;
                              Message : in String := "Test failed";
                              Source  : in String := GNAT.Source_Info.File;
                              Line    : in Natural := GNAT.Source_Info.Line) is
      pragma Unreferenced (T);
   begin
      AUnit.Assertions.Assert (Condition => Expect = Value,
                               Message   => Message & ": expecting '"
                               & Value_Type'Image (Expect) & "'"
                               & " value was '"
                               & Value_Type'Image (Value) & "'",
                               Source    => Source,
                               Line      => Line);
   end Assert_Equals_T;

end Util.Assertions;
