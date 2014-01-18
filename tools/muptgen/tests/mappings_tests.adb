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

with Test_Utils;

with Pt.Mappings;
with Pt.Paging;

package body Mappings_Tests
is

   use Ahven;
   use Pt.Mappings;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Mappings tests");
      T.Add_Test_Routine
        (Routine => Write_Pagetables'Access,
         Name    => "Write pagetables");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Write_Pagetables
   is
      Mem_Layout1 : Memory_Layout_Type (PT_Type      => IA32e,
                                        PML4_Address => 16#1f0000#);
      Mem_Layout2 : Memory_Layout_Type (PT_Type      => EPT,
                                        PML4_Address => 16#290000#);
   begin
      Add_Memory_Region (Mem_Layout       => Mem_Layout1,
                         Physical_Address => 16#10000#,
                         Virtual_Address  => 16#beefcafe0000#,
                         Size             => 16#4000#,
                         Caching          => Pt.Paging.UC,
                         Writable         => True,
                         Executable       => False);
      Write_Pagetables (Mem_Layout => Mem_Layout1,
                        Filename   => "obj/ia32e_pt");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/ia32e_pt.ref",
               Filename2 => "obj/ia32e_pt"),
              Message   => "IA-32e pagetables mismatch");

      Add_Memory_Region (Mem_Layout       => Mem_Layout2,
                         Physical_Address => 16#0000#,
                         Virtual_Address  => 16#cafebeef0000#,
                         Size             => 16#10000#,
                         Caching          => Pt.Paging.WB,
                         Writable         => True,
                         Executable       => True);
      Write_Pagetables (Mem_Layout => Mem_Layout2,
                        Filename   => "obj/ept_pt");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/ept_pt.ref",
               Filename2 => "obj/ept_pt"),
              Message   => "EPT pagetables mismatch");
   end Write_Pagetables;

end Mappings_Tests;
