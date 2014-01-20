--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Paging;

package body Paging_Tests
is

   use Ahven;
   use Paging;
   use type Interfaces.Unsigned_64;

   -------------------------------------------------------------------------

   procedure Index_Calculation
   is
      PML4, PDPT, PD, PT : Table_Range;
   begin
      Get_Indexes (Address    => 0,
                   PML4_Index => PML4,
                   PDPT_Index => PDPT,
                   PD_Index   => PD,
                   PT_Index   => PT);
      Assert (Condition => PML4 = 0,
              Message   => "PML4 index mismatch (1)");
      Assert (Condition => PDPT = 0,
              Message   => "PDPT index mismatch (1)");
      Assert (Condition => PD = 0,
              Message   => "PD index mismatch (1)");
      Assert (Condition => PT = 0,
              Message   => "PT index mismatch (1)");

      Get_Indexes (Address    => Interfaces.Unsigned_64'Last,
                   PML4_Index => PML4,
                   PDPT_Index => PDPT,
                   PD_Index   => PD,
                   PT_Index   => PT);
      Assert (Condition => PML4 = Table_Range'Last,
              Message   => "PML4 index mismatch (2)");
      Assert (Condition => PDPT = Table_Range'Last,
              Message   => "PDPT index mismatch (2)");
      Assert (Condition => PD = Table_Range'Last,
              Message   => "PD index mismatch (2)");
      Assert (Condition => PT = Table_Range'Last,
              Message   => "PT index mismatch (2)");

      Get_Indexes (Address    => 16#fffc80200f000#,
                   PML4_Index => PML4,
                   PDPT_Index => PDPT,
                   PD_Index   => PD,
                   PT_Index   => PT);
      Assert (Condition => PML4 = 511,
              Message   => "PML4 index mismatch (3)");
      Assert (Condition => PDPT = 288,
              Message   => "PDPT index mismatch (3)");
      Assert (Condition => PD = 16,
              Message   => "PD index mismatch (3)");
      Assert (Condition => PT = 15,
              Message   => "PT index mismatch (3)");
   end Index_Calculation;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Paging tests");
      T.Add_Test_Routine
        (Routine => Index_Calculation'Access,
         Name    => "Paging structure index calculation");
   end Initialize;

end Paging_Tests;
