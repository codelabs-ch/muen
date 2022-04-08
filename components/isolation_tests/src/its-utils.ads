--
--  Copyright (C) 2021  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2021  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package ITS.Utils
is

   --  Addressable memory which explicitly exludes 0 since access to
   --  Null_Address is undefined behavior.
   subtype Memory_Address is
     Interfaces.Unsigned_64 range 1 .. Interfaces.Unsigned_64'Last;

   --  Read byte value from given memory address.
   procedure Read_Byte
     (Address :     Memory_Address;
      Value   : out Interfaces.Unsigned_8);

   --  Write byte value to given memory address.
   procedure Write_Byte
     (Address : Memory_Address;
      Value   : Interfaces.Unsigned_8);

end ITS.Utils;
