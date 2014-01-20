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

package body Entries_Tests
is

   use Ahven;
   use Paging.Entries;

   -------------------------------------------------------------------------

   procedure Creation
   is
      use type Interfaces.Unsigned_64;
      use type Paging.Caching_Type;
      use type Paging.Table_Range;

      TEntry : Table_Entry_Type;
   begin
      TEntry := Create (Dst_Offset  => 42,
                        Dst_Address => 16#1f_f000#,
                        Readable    => True,
                        Writable    => False,
                        Executable  => True,
                        Maps_Page   => True,
                        Global      => True,
                        Caching     => Paging.WB);

      Assert (Condition => Get_Dst_Offset (E => TEntry) = 42,
              Message   => "Dst offset mismatch");
      Assert (Condition => Get_Dst_Address (E => TEntry) = 16#1f_f000#,
              Message   => "Dst address mismatch");
      Assert (Condition => Is_Readable (E => TEntry),
              Message   => "Not readable");
      Assert (Condition => not Is_Writable (E => TEntry),
              Message   => "Writable");
      Assert (Condition => Is_Executable (E => TEntry),
              Message   => "Writable");
      Assert (Condition => Maps_Page (E => TEntry),
              Message   => "Not mapping page");
      Assert (Condition => Is_Global (E => TEntry),
              Message   => "Non-global entry");
      Assert (Condition => Get_Caching (E => TEntry) = Paging.WB,
              Message   => "Caching type mismatch");
   end Creation;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Pagtable entry tests");
      T.Add_Test_Routine
        (Routine => Creation'Access,
         Name    => "Table entry creation");
   end Initialize;

end Entries_Tests;
