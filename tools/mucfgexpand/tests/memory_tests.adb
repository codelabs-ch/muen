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

with Expanders.Memory;

with Test_Utils;

package body Memory_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Add_Binary_Memory
   is
      Filename : constant String := "obj/binary_memory.xml";
      Policy   : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Expanders.Memory.Add_Kernel_Binary (Data => Policy);

      Muxml.Write (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => Filename);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Filename,
               Filename2 => "data/binary_memory.ref.xml"),
              Message   => "Policy mismatch");

      Ada.Directories.Delete_File (Name => Filename);
   end Add_Binary_Memory;

   -------------------------------------------------------------------------

   procedure Add_Stack_Store
   is
      Filename : constant String := "obj/stack_store.xml";
      Policy   : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Expanders.Memory.Add_Stack_Store (Data => Policy);

      Muxml.Write (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => Filename);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Filename,
               Filename2 => "data/stack_store.ref.xml"),
              Message   => "Policy mismatch");

      Ada.Directories.Delete_File (Name => Filename);
   end Add_Stack_Store;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Memory expander tests");
      T.Add_Test_Routine
        (Routine => Add_Binary_Memory'Access,
         Name    => "Add kernel binary memory regions");
      T.Add_Test_Routine
        (Routine => Add_Stack_Store'Access,
         Name    => "Add kernel stack and store memory regions");
   end Initialize;

end Memory_Tests;
