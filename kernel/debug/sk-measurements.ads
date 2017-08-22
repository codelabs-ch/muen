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

package SK.Measurements
with
   SPARK_Mode => Off
is

   type Measurements_Array is array (Natural range <>) of Word64;

   subtype Measurements_Range is Natural range 1 .. 256;

   Save_State : Measurements_Array (Measurements_Range) := (others => 0);

   Current : Measurements_Range := Measurements_Range'First;

   function Calc_Average (Values : Measurements_Array) return Word64;

   procedure Start
     with Inline_Always;

   procedure Stop
     with Inline_Always;

end SK.Measurements;
