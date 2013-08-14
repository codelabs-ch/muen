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

with Ahven.Framework;

package Writer_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Write kernel policy files.
   procedure Write_Kernel;

   --  Write subject policy files.
   procedure Write_Subjects;

   --  Write system policy files.
   procedure Write_System;

   --  Write binary spec files.
   procedure Write_Binaries;

   --  Write hardware spec file.
   procedure Write_Hardware;

   --  Write scheduling spec file.
   procedure Write_Scheduling;

   --  Write interrupts spec file.
   procedure Write_Interrupts;

end Writer_Tests;
