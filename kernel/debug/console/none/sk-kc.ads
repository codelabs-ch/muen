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

package SK.KC
is

   --  Initialize console.
   procedure Init is null;

   --  Output a new line.
   procedure New_Line is null;

   --  Output given character.
   procedure Put_Char (Item : Character) is null;

   --  Output given string.
   procedure Put_String (Item : String) is null;

   --  Output given string and append a new line.
   procedure Put_Line (Item : String) is null;

end SK.KC;
