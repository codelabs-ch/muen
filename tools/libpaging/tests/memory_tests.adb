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
   end Initialize;

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
