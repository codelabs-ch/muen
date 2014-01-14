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

with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Mutools.Utils
is

   package U64_IO is new Ada.Text_IO.Modular_IO
     (Num => Interfaces.Unsigned_64);

   -------------------------------------------------------------------------

   function To_Hex
     (Number : Interfaces.Unsigned_64;
      Prefix : Boolean := True)
      return String
   is
      Num_Str : String (1 .. 20);
   begin
      U64_IO.Put (To   => Num_Str,
                  Item => Number,
                  Base => 16);

      declare
         Trimmed : constant String := Ada.Strings.Fixed.Trim
           (Source => Num_Str,
            Side   => Ada.Strings.Left);
      begin
         if Prefix then
            return Trimmed;
         else
            return Trimmed (Trimmed'First + 3 .. Trimmed'Last - 1);
         end if;
      end;
   end To_Hex;

end Mutools.Utils;
