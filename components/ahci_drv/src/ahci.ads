--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Ahci
is

   type Unsigned_4 is mod 2 ** 4;
   for Unsigned_4'Size use 4;

   type Unsigned_5 is mod 2 ** 5;
   for Unsigned_5'Size use 5;

   type Unsigned_10 is mod 2 ** 10;
   for Unsigned_10'Size use 10;

   type Unsigned_22 is mod 2 ** 22;
   for Unsigned_22'Size use 22;

   type Unsigned_25 is mod 2 ** 25;
   for Unsigned_25'Size use 25;

   type Unsigned_31 is mod 2 ** 31;
   for Unsigned_31'Size use 31;

   type Bit_Array is array (Natural range <>) of Boolean
   with
      Pack;

   type Byte_Array is array (Natural range <>) of Interfaces.Unsigned_8
   with
      Pack;

end Ahci;
