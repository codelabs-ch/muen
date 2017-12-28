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

package Ahci.Commands
is

   --  Serial ATA AHCI 1.3.1 Specification, section 4.2.2.

   type Command_Header_Type is record
      CFL        : Unsigned_5;
      A          : Boolean;
      W          : Boolean;
      P          : Boolean;
      R          : Boolean;
      B          : Boolean;
      C          : Boolean;
      Reserved_1 : Boolean;
      PMP        : Unsigned_4;
      PRDTL      : Interfaces.Unsigned_16;
      PRDBC      : Interfaces.Unsigned_32;
      Reserved_2 : Bit_Array (0 .. 6);
      CTBA       : Unsigned_25;
      CTBAU      : Interfaces.Unsigned_32;
      Reserved_3 : Bit_Array (1 .. 128);
   end record
   with
      Size => 32 * 8;

   for Command_Header_Type use record
      CFL        at  0 range  0 ..   4;
      A          at  0 range  5 ..   5;
      W          at  0 range  6 ..   6;
      P          at  0 range  7 ..   7;
      R          at  0 range  8 ..   8;
      B          at  0 range  9 ..   9;
      C          at  0 range 10 ..  10;
      Reserved_1 at  0 range 11 ..  11;
      PMP        at  0 range 12 ..  15;
      PRDTL      at  0 range 16 ..  31;
      PRDBC      at  4 range  0 ..  31;
      Reserved_2 at  8 range  0 ..   6;
      CTBA       at  8 range  7 ..  31;
      CTBAU      at 12 range  0 ..  31;
      Reserved_3 at 16 range  0 .. 127;
   end record;

end Ahci.Commands;
