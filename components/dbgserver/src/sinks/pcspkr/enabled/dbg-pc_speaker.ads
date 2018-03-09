--
--  Copyright (C) 2018  secunet Security Networks AG
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

with Dbg.Byte_Arrays;

package Dbg.PC_Speaker
is

   subtype Word32 is Interfaces.Unsigned_32;
   subtype Word16 is Interfaces.Unsigned_16;
   subtype Byte   is Interfaces.Unsigned_8;

   procedure Play_Sound
     (Frequency   : Word32;
      Duration_MS : Natural);

   procedure Put_Byte
     (Item        : Byte;
      Duration_MS : Natural);

   procedure Put
     (Item        : Byte_Arrays.Byte_Array;
      Duration_MS : Natural);

private

   subtype Pos   is Byte range 0 .. 7;
   subtype Phase is Byte range 0 .. 1;
   subtype Bit   is Byte range 0 .. 1;

   procedure Start_Beep (Frequency : Word32);

   procedure Stop_Beep;

   function Nth_Bit (B : Byte; N : Pos) return Bit;

end Dbg.PC_Speaker;
