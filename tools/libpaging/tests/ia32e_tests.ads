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

package IA32e_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Test PML4 serialization.
   procedure PML4_Serialization;

   --  Test PDPT serialization.
   procedure PDPT_Serialization;

   --  Test PD serialization.
   procedure PD_Serialization;

   --  Test PT serialization.
   procedure PT_Serialization;

   --  Test IA-32e paging structure generation.
   procedure Generate_Paging_Structures;

   --  Test IA-32e Multi-PT generation.
   procedure Generate_Multiple_PTs;

   --  Test IA-32e multi paging structure generation.
   procedure Generate_Multiple_Structures;

   --  Test serialization of empty memory layout.
   procedure Serialize_Empty_Layout;

end IA32e_Tests;
