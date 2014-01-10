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

with OS_Tests;
with Image_Tests;
with Parser_Tests;
with Transform_Tests;

procedure Test_Runner
is
   use Ahven.Framework;

   S : constant Test_Suite_Access := Create_Suite
     (Suite_Name => "Mupack tests");
begin
   Add_Test (Suite => S.all,
             T     => new OS_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Image_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Parser_Tests.Testcase);
   Add_Test (Suite => S.all,
             T     => new Transform_Tests.Testcase);

   Ahven.Text_Runner.Run (Suite => S);
   Release_Suite (T => S);
end Test_Runner;