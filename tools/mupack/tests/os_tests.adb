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

with Pack.OS;

package body OS_Tests
is

   use Ahven;
   use Pack;

   -------------------------------------------------------------------------

   procedure Execute_Command
   is
   begin
      OS.Execute (Command => "/bin/echo");

      begin
         OS.Execute (Command => "no_such_command 2>/dev/null");

      exception
         when OS.Command_Failed => null;
      end;
   end Execute_Command;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "OS package tests");
      T.Add_Test_Routine
        (Routine => Execute_Command'Access,
         Name    => "Execute command");
   end Initialize;

end OS_Tests;
