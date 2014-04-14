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

with Ahven.Framework;

package Subjects_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Add subject binaries.
   procedure Add_Binaries;

   --  Handle subject profile.
   procedure Handle_Profile;

   --  Add tau0.
   procedure Add_Tau0;

   --  Handle subject monitors.
   procedure Handle_Monitors;

   --  Add subject ids.
   procedure Add_Ids;

   --  Add missing subject elements.
   procedure Add_Missing_Elements;

   --  Add channel mappings.
   procedure Add_Channels;

end Subjects_Tests;
