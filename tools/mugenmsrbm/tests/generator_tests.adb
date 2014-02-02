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

with Test_Utils;

with Muxml;

with Msrbm.Generator;

package body Generator_Tests
is

   use Ahven;
   use Msrbm;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Generator tests");
      T.Add_Test_Routine
        (Routine => Write_MSR_Bitmaps'Access,
         Name    => "Write MSR bitmaps");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Write_MSR_Bitmaps
   is
      Policy : Muxml.XML_Data_Type;

      Tau0_Msrbm : constant String := "obj/tau0_msrbm";
      Sub1_Msrbm : constant String := "obj/subject1_msrbm";
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Generator.Write (Output_Dir => "obj",
                       Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/tau0_msrbm.ref",
               Filename2 => Tau0_Msrbm),
              Message => "Tau0 MSR bitmap mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/subject1_msrbm.ref",
               Filename2 => Sub1_Msrbm),
              Message => "Subject 1 MSR bitmap mismatch");
   end Write_MSR_Bitmaps;

end Generator_Tests;
