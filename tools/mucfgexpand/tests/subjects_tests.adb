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

   procedure Add_Tau0
   is
   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/subjects_tau0.xml",
         Ref_Filename => "data/subjects_tau0.ref.xml",
         Expander     => Expanders.Subjects.Add_Tau0'Access);
   end Add_Tau0;

   -------------------------------------------------------------------------

   procedure Handle_Monitors
   is
   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/subjects_monitors.xml",
         Ref_Filename => "data/subjects_monitors.ref.xml",
         Expander     => Expanders.Subjects.Handle_Monitors'Access);
   end Handle_Monitors;

   -------------------------------------------------------------------------

   procedure Handle_Profile
   is
   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/subjects_profiles.xml",
         Ref_Filename => "data/subjects_profiles.ref.xml",
         Expander     => Expanders.Subjects.Handle_Profile'Access);
   end Handle_Profile;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Subjects expander tests");
      T.Add_Test_Routine
        (Routine => Add_Binaries'Access,
         Name    => "Add binaries");
      T.Add_Test_Routine
        (Routine => Handle_Profile'Access,
         Name    => "Handle profile");
      T.Add_Test_Routine
        (Routine => Add_Tau0'Access,
         Name    => "Add tau0");
      T.Add_Test_Routine
        (Routine => Handle_Monitors'Access,
         Name    => "Handle monitors");
   end Initialize;

end Subjects_Tests;
