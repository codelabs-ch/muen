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

with Ahven.Framework;

package Memory_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Add kernel binary memory regions.
   procedure Add_Kernel_Binary;

   --  Add stack and store regions.
   procedure Add_Stack_Store;

   --  Add subject state regions.
   procedure Add_Subject_States;

   --  Add tau0 interface memory region.
   procedure Add_Tau0_Interface;

   --  Add AP trampoline memory region.
   procedure Add_AP_Trampoline;

   --  Add VMXON memory regions.
   procedure Add_VMXON_Regions;

   --  Add VMXON memory regions.
   procedure Add_VMCS_Regions;

   --  Add kernel pagetable memory regions.
   procedure Add_Kernel_PTs;

   --  Add alignment/type attributes.
   procedure Add_Missing_Attributes;

   --  Add I/O and MSR bitmap memory regions.
   procedure Add_Subject_Bitmaps;

   --  Add subject pagetable memory regions.
   procedure Add_Subject_PTs;

end Memory_Tests;
