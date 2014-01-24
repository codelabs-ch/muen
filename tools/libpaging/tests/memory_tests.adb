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

with Interfaces;

with Paging.Memory;

package body Memory_Tests
is

   use Ahven;
   use Paging;
   use Paging.Memory;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Memory layout tests");
      T.Add_Test_Routine
        (Routine => Layout_Address_Handling'Access,
         Name    => "Memory layout address handling");
      T.Add_Test_Routine
        (Routine => Layout_Add_Region'Access,
         Name    => "Add memory region to layout");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Layout_Add_Region
   is
      Layout : Memory_Layout_Type := Null_Layout;

      PML4s, PDPTs, PDs, PTs : Natural;
   begin
      Get_Table_Count (Mem_Layout => Layout,
                       PML4_Count => PML4s,
                       PDPT_Count => PDPTs,
                       PD_Count   => PDs,
                       PT_Count   => PTs);
      Assert (Condition => PML4s = 0,
              Message   => "PML4 count not 0");
      Assert (Condition => PDPTs = 0,
              Message   => "PDPT count not 0");
      Assert (Condition => PDs = 0,
              Message   => "PD count not 0");
      Assert (Condition => PTs = 0,
              Message   => "PT count not 0");

      Add_Memory_Region
        (Mem_Layout       => Layout,
         Physical_Address => 16#1000#,
         Virtual_Address  => 16#deafbeef000#,
         Size             => Page_Size,
         Caching          => WB,
         Writable         => True,
         Executable       => False);

      Get_Table_Count (Mem_Layout => Layout,
                       PML4_Count => PML4s,
                       PDPT_Count => PDPTs,
                       PD_Count   => PDs,
                       PT_Count   => PTs);
      Assert (Condition => PML4s = 1,
              Message   => "PML4 count not 1");
      Assert (Condition => PDPTs = 1,
              Message   => "PDPT count not 1");
      Assert (Condition => PDs = 1,
              Message   => "PD count not 1");
      Assert (Condition => PTs = 1,
              Message   => "PT count not 1");
   end Layout_Add_Region;

   -------------------------------------------------------------------------

   procedure Layout_Address_Handling
   is
      use type Interfaces.Unsigned_64;

      Ref_Addr : constant Interfaces.Unsigned_64 := 16#deadbeef000#;
      Layout   : Memory_Layout_Type;
   begin
      Set_Address (Mem_Layout => Layout,
                   Address    => Ref_Addr);

      Assert (Condition => Get_Address (Mem_Layout => Layout) = Ref_Addr,
              Message   => "Layout address mismatch");
   end Layout_Address_Handling;

end Memory_Tests;
