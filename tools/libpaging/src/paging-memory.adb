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

package body Paging.Memory
is

   -------------------------------------------------------------------------

   function Get_Address
     (Mem_Layout : Memory_Layout_Type)
      return Interfaces.Unsigned_64
   is
   begin
      return Tables.PML4.Get_Physical_Address (Table => Mem_Layout.PML4);
   end Get_Address;

   -------------------------------------------------------------------------

   procedure Set_Address
     (Mem_Layout : in out Memory_Layout_Type;
      Address    :        Interfaces.Unsigned_64)
   is
   begin
      Tables.PML4.Set_Physical_Address
        (Table   => Mem_Layout.PML4,
         Address => Address);
   end Set_Address;

end Paging.Memory;
