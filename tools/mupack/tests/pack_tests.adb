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

with Pack.Command_Line.Test;

package body Pack_Tests
is

   use Ahven;
   use Pack;

   -------------------------------------------------------------------------

   procedure Execute_Run
   is
      Obj_Dir : constant String := "obj";
      Knl_Src : constant String := "obj1.o";
      Knl_Cpy : constant String := "obj1.o.elf";
      Knl_Bin : constant String := "obj1.o.bin";
   begin
      Command_Line.Test.Set_Input_Dir       (Path => "data");
      Command_Line.Test.Set_Output_Dir      (Path => Obj_Dir);
      Command_Line.Test.Set_Kernel_Filename (Path => Knl_Src);
      Command_Line.Test.Set_Policy          (Path => "data/test_policy.xml");

      Pack.Run;

      Assert (Condition => Ada.Directories.Exists
              (Name => Obj_Dir & "/" & Knl_Cpy),
              Message   => "ELF kernel not found");
      Assert (Condition => Ada.Directories.Exists
              (Name => Obj_Dir & "/" & Knl_Bin),
              Message   => "Binary kernel not found");
      Assert (Condition => Ada.Directories.Exists
              (Name => Obj_Dir & "/" & Knl_Src),
              Message   => "System image not found");

      Ada.Directories.Delete_File (Name => Obj_Dir & "/" & Knl_Bin);
      Ada.Directories.Delete_File (Name => Obj_Dir & "/" & Knl_Cpy);
      Ada.Directories.Delete_File (Name => Obj_Dir & "/" & Knl_Src);
   end Execute_Run;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Pack tests");
      T.Add_Test_Routine
        (Routine => Execute_Run'Access,
         Name    => "Run packaging process");
   end Initialize;

end Pack_Tests;
