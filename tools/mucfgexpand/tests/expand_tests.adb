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

with Muxml;

with Expand;

with Test_Utils;

package body Expand_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Execute_Run
   is
      Filename : constant String := "obj/execute_run.xml";
      Policy   : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Expand.Run (Policy      => Policy,
                  Output_File => Filename);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Filename,
               Filename2 => "data/execute_run.ref.xml"),
              Message   => "Policy mismatch");

      Ada.Directories.Delete_File (Name => Filename);
   end Execute_Run;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Expand tests");
      T.Add_Test_Routine
        (Routine => Execute_Run'Access,
         Name    => "Run expansion process");
   end Initialize;

end Expand_Tests;
