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

with Pt.Generator;

package body Generator_Tests
is

   use Ahven;
   use Pt;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Generator tests");
      T.Add_Test_Routine
        (Routine => Write_Pagetables'Access,
         Name    => "Write page tables");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Write_Pagetables
   is
      Policy : Muxml.XML_Data_Type;

      Knl_Pt0 : constant String := "obj/kernel_pt_0";
      Knl_Pt1 : constant String := "obj/kernel_pt_1";
      Tau0_Pt : constant String := "obj/tau0_pt";
      Sub1_Pt : constant String := "obj/subject1_pt";
      Sub2_Pt : constant String := "obj/subject2_pt";
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Generator.Write (Output_Dir => "obj",
                       Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/kernel_pt_0.ref",
               Filename2 => Knl_Pt0),
              Message => "Kernel pagetables mismatch (0)");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/kernel_pt_1.ref",
               Filename2 => Knl_Pt1),
              Message => "Kernel pagetables mismatch (1)");

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/tau0_pt.ref",
               Filename2 => Tau0_Pt),
              Message => "Tau0 pagetables mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/subject1_pt.ref",
               Filename2 => Sub1_Pt),
              Message => "Subject 1 pagetables mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/subject2_pt.ref",
               Filename2 => Sub2_Pt),
              Message => "Subject 2 pagetables mismatch");
   end Write_Pagetables;

end Generator_Tests;
