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

with System;

package body ITS.Utils
is

   The_Memory : array (Memory_Address'Range) of Interfaces.Unsigned_8
   with
      Import,
      Volatile,
      Address => System'To_Address (1);

   -------------------------------------------------------------------------

   procedure Read_Byte
     (Address :     Memory_Address;
      Value   : out Interfaces.Unsigned_8)
   is
   begin
      Value := The_Memory (Address);
   end Read_Byte;

   -------------------------------------------------------------------------

   procedure Write_Byte
     (Address : Memory_Address;
      Value   : Interfaces.Unsigned_8)
   is
   begin
      The_Memory (Address) := Value;
   end Write_Byte;

end ITS.Utils;
