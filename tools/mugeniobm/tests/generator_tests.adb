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

with Ada.Directories;

with Test_Utils;

with Muxml;

with Iobm.Generator;

package body Generator_Tests
is

   use Ahven;
   use Iobm;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Generator tests");
      T.Add_Test_Routine
        (Routine => Write_IO_Bitmaps'Access,
         Name    => "Write I/O bitmaps");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Write_IO_Bitmaps
   is
      Policy : Muxml.XML_Data_Type;

      Tau0_Iobm : constant String := "obj/tau0_iobm";
      Sub1_Iobm : constant String := "obj/subject1_iobm";
      Sub2_Iobm : constant String := "obj/subject2_iobm";
   begin
      Muxml.Parse (Data => Policy,
                   File => "data/test_policy.xml");

      Generator.Write (Output_Dir => "obj",
                       Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/tau0_iobm.ref",
               Filename2 => Tau0_Iobm),
              Message => "Tau0 I/O bitmap mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/subject1_iobm.ref",
               Filename2 => Sub1_Iobm),
              Message => "Subject 1 I/O bitmap mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/subject2_iobm.ref",
               Filename2 => Sub2_Iobm),
              Message => "Subject 2 I/O bitmap mismatch");

      Ada.Directories.Delete_File (Name => Tau0_Iobm);
      Ada.Directories.Delete_File (Name => Sub1_Iobm);
      Ada.Directories.Delete_File (Name => Sub2_Iobm);
   end Write_IO_Bitmaps;

end Generator_Tests;
