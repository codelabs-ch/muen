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

with Sinfo.Generator;

package body Generator_Tests
is

   use Ahven;
   use Sinfo;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Generator tests");
      T.Add_Test_Routine
        (Routine => Write_Subject_Info'Access,
         Name    => "Write subject info");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Write_Subject_Info
   is
      Policy : Muxml.XML_Data_Type;

      Subject_Sinfo : constant String := "obj/lnx_sinfo";
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Generator.Write (Output_Dir => "obj",
                       Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/lnx_sinfo",
               Filename2 => Subject_Sinfo),
              Message => "Subject info file mismatch");

      Ada.Directories.Delete_File (Name => Subject_Sinfo);
   end Write_Subject_Info;

end Generator_Tests;
