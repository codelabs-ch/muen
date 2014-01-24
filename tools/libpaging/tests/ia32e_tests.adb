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

with Paging.Entries;
with Paging.IA32e;

package body IA32e_Tests
is

   use Ahven;
   use Paging;
   use Paging.IA32e;
   use type Interfaces.Unsigned_64;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "IA-32e paging tests");
      T.Add_Test_Routine
        (Routine => PML4E_To_Unsigned64'Access,
         Name    => "PML4E to unsigned 64");
   end Initialize;

   -------------------------------------------------------------------------

   procedure PML4E_To_Unsigned64
   is
      Ref   : constant Interfaces.Unsigned_64  := 16#80000000001f100b#;
      PML4E : constant Entries.PML4_Entry_Type := Entries.Create
        (Dst_Offset  => 0,
         Dst_Address => 16#1f1000#,
         Readable    => False,
         Writable    => True,
         Executable  => False,
         Maps_Page   => False,
         Global      => True,
         Caching     => WC);
   begin
      Assert (Condition => To_Unsigned64 (E => PML4E) = Ref,
              Message   => "PML4 entry unsigned 64 value mismatch");
   end PML4E_To_Unsigned64;

end IA32e_Tests;
