-----------------------------------------------------------------------
--  AUnit utils - Helper for writing unit tests
--  Copyright (C) 2009, 2010 Stephane Carrez
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

with Util.XUnit;
with Ada.Unchecked_Conversion;
package body Util.Test_Caller is

   Test     : aliased Util.XUnit.Test_Object;
   Instance : aliased Test_Fixture;

   function To_X is
      new Ada.Unchecked_Conversion (Source => Test_Method,
                                    Target => Ahven.Framework.Object_Test_Routine_Access);

   Added : Boolean := False;

   procedure Add_Test (Suite     : in Util.Tests.Access_Test_Suite;
                       Test_Name : in String;
                       Method    : in Test_Method) is
      pragma Unreferenced (Suite);
   begin
      if not Added then
         Instance.Set_Name (Util.Tests.Get_Harness_Prefix & Name);
         Test.Test := Instance'Access;
         Util.XUnit.Register (Test'Access);
         Added := True;
      end if;
      Ahven.Framework.Add_Test_Routine (Instance, To_X (Method), Test_Name);
   end Add_Test;

end Util.Test_Caller;
