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

with System;

generic

   --  Console width range.
   type Width_Type is range <>;

   --  Console heigth range.
   type Height_Type is range <>;

   --  Base address of video framebuffer.
   Base_Address : System.Address;

package SK.Console_VGA
is

   --  Clear screen and set initial cursor position.
   procedure Init;

   --  Start new line and scroll screen if necessary.
   procedure New_Line;

   --  Print character at current cursor position.
   procedure Put_Char (Item : Character);

   --  Move cursor to the left on the current line.
   procedure Line_Move_Left;

   --  Delete character at current position.
   procedure Delete_Char;

   --  Set cursor position.
   procedure Set_Position
     (X : Width_Type;
      Y : Height_Type);

   --  Enable position cursor.
   procedure Enable_Cursor;

   --  Disable position cursor.
   procedure Disable_Cursor;

end SK.Console_VGA;
