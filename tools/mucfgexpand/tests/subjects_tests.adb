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

with Expanders.Subjects;

with Test_Utils.Expander;

package body Subjects_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Add_Binaries
   is
   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/subjects_binaries.xml",
         Ref_Filename => "data/subjects_binaries.ref.xml",
         Expander     => Expanders.Subjects.Add_Binaries'Access);
   end Add_Binaries;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Subjects expander tests");
      T.Add_Test_Routine
        (Routine => Add_Binaries'Access,
         Name    => "Add binaries");
   end Initialize;

end Subjects_Tests;
