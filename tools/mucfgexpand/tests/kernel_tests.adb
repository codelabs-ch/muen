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

with Expanders.Kernel;

with Test_Utils.Expander;

package body Kernel_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Add_Binary_Mappings
   is
   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/kernel_binary_mappings.xml",
         Ref_Filename => "data/kernel_binary_mappings.ref.xml",
         Expander     => Expanders.Kernel.Add_Binary_Mappings'Access);
   end Add_Binary_Mappings;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Kernel expander tests");
      T.Add_Test_Routine
        (Routine => Add_Binary_Mappings'Access,
         Name    => "Add binary mappings");
   end Initialize;

end Kernel_Tests;
