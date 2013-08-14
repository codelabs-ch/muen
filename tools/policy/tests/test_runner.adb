--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Ahven.Text_Runner;
with Ahven.Framework;

with Xml_Tests;
with Writer_Tests;
with Paging_Tests;
with EPT_Paging_Tests;
with IO_Port_Tests;
with MSR_Tests;
with Validation_Tests;
with Templates_Tests;

procedure Test_Runner
is
   use Ahven.Framework;

   S : constant Test_Suite_Access := Create_Suite (Suite_Name => "SKP tests");
begin
   Add_Test (Suite => S.all,
             T     => new Xml_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Writer_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Paging_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new EPT_Paging_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new IO_Port_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new MSR_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Validation_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Templates_Tests.Testcase);

   Ahven.Text_Runner.Run (Suite => S);
   Release_Suite (T => S);
end Test_Runner;
