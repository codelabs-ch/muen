--
--  Copyright (C) 2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp.Hardware;

with SK.Console_VGA;

package SK.Legacy_VGA
is

   type Width_Type  is range 1 .. 80;
   type Height_Type is range 1 .. 25;

   VGA_Buffer_Offset : constant SK.Word64 := 16#1_8000#;

   package VGA is new Console_VGA
     (Width_Type    => Width_Type,
      Height_Type   => Height_Type,
      Base_Address  => Skp.Hardware.Debugconsole_Memory + VGA_Buffer_Offset,
      Cursor_Offset => 0);

end SK.Legacy_VGA;
