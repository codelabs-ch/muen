--
--  Copyright (C) 2013, 2014  Alexander Senier <mail@senier.net>
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

with Muxml;
with Alloc.Allocator;

package body Allocation_Tests
is
   use Ahven;
   use Alloc;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Allocation tests");
      T.Add_Test_Routine
        (Routine => Overlapping_Physical_Memory'Access,
         Name    => "Overlap detection");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Overlapping_Physical_Memory
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   File => "data/overlapping.xml");

      Allocator.Write (Output_Dir => "obj",
                       Policy     => Policy);
   exception
      --  Should raise an exception.
      when Alloc.Allocator.Overlapping_Physical_Memory => null;
      when others => Fail ("Overlap undetected");
   end Overlapping_Physical_Memory;

end Allocation_Tests;
