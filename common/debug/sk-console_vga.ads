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

   --  VGA colors.
   type VGA_Color_Type is
     (Black,
      Blue,
      Green,
      Cyan,
      Red,
      Magenta,
      Brown,
      Light_Grey,
      Dark_Grey,
      Light_Blue,
      Light_Green,
      Light_Cyan,
      Light_Red,
      Light_Magenta,
      Yellow,
      White);

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

private

   for VGA_Color_Type use
     (Black         => 16#0#,
      Blue          => 16#1#,
      Green         => 16#2#,
      Cyan          => 16#3#,
      Red           => 16#4#,
      Magenta       => 16#5#,
      Brown         => 16#6#,
      Light_Grey    => 16#7#,
      Dark_Grey     => 16#8#,
      Light_Blue    => 16#9#,
      Light_Green   => 16#a#,
      Light_Cyan    => 16#b#,
      Light_Red     => 16#c#,
      Light_Magenta => 16#d#,
      Yellow        => 16#e#,
      White         => 16#f#);
   for VGA_Color_Type'Size use 4;
end SK.Console_VGA;
