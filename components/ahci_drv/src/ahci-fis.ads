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

package Ahci.FIS
is

   --  Serial ATA AHCI 1.3.1 Specification, section 4.2.3.1.
   type Command_FIS_Type is record
      FIS_Type     : Interfaces.Unsigned_8;
      PM_Port      : Unsigned_4;
      Reserved_1   : Bit_Array (12 .. 14);
      C            : Boolean;
      Command      : Interfaces.Unsigned_8;
      Features0_7  : Interfaces.Unsigned_8;
      LBA0_23      : Interfaces.Unsigned_24;
      Device       : Interfaces.Unsigned_8;
      LBA_24_47    : Interfaces.Unsigned_24;
      Features8_15 : Interfaces.Unsigned_8;
      Count        : Interfaces.Unsigned_16;
      ICC          : Bit_Array (16 .. 23);
      Control      : Interfaces.Unsigned_8;
      Reserved_2   : Bit_Array (0 .. 31);
   end record
   with
      Size => 20 * 8;

   for Command_FIS_Type use record
      FIS_Type     at  0 range  0 ..  7;
      PM_Port      at  0 range  8 .. 11;
      Reserved_1   at  0 range 12 .. 14;
      C            at  0 range 15 .. 15;
      Command      at  0 range 16 .. 23;
      Features0_7  at  0 range 24 .. 31;
      LBA0_23      at  4 range  0 .. 23;
      Device       at  4 range 24 .. 31;
      LBA_24_47    at  8 range  0 .. 23;
      Features8_15 at  8 range 24 .. 31;
      Count        at 12 range  0 .. 15;
      ICC          at 12 range 16 .. 23;
      Control      at 12 range 24 .. 31;
      Reserved_2   at 16 range  0 .. 31;
   end record;

end Ahci.FIS;
