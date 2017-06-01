--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

generic

   --  Implementation of the console initialization operation.
   with procedure Initialize;

   --  Implementation of the new line output operation.
   with procedure Output_New_Line;

   --  Implementation of the character output operation.
   with procedure Output_Char (Item : Character);

package SK.Console
is

   --  Initialize console.
   procedure Init;

   --  Output a new line.
   procedure New_Line;

   --  Output given character.
   procedure Put_Char (Item : Character);

   --  Output given string.
   procedure Put_String (Item : String);

   --  Output given string and append a new line.
   procedure Put_Line (Item : String);

end SK.Console;
