--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Validate.XML_Processors;

package body Validate_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Execute_Run
   is
   begin
      Validate.Run (Policy => "data/test_policy.xml");

      --  Positive test, no exceptions must occur.

      Validate.XML_Processors.Clear;
   end Execute_Run;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Validate tests");
      T.Add_Test_Routine
        (Routine => Execute_Run'Access,
         Name    => "Run validation process");
   end Initialize;

end Validate_Tests;
