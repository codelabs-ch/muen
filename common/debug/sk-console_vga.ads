--
--  Copyright (C) 2013-2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

   --  Console width range.
   type Width_Type is range <>;

   --  Console height range.
   type Height_Type is range <>;

   --  Base address of video framebuffer.
   Base_Address : SK.Word64;

   --  Hardware cursor offset (relative to given base address).
   Cursor_Offset : Natural;

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

   --  Insert horizontal tab (4 spaces).
   procedure Put_Tab;

   --  Moves the cursor N cells to the left on the current line. If the cursor
   --  is already at the edge of the screen, this has no effect.
   procedure Cursor_Back (N : Width_Type := Width_Type'First);

   --  Moves the cursor N cells to the right. If the cursor is already at the
   --  edge of the screen, this has no effect.
   procedure Cursor_Forward (N : Width_Type := Width_Type'First);

   --  Moves the cursor N cells up. If the cursor is already at the edge of the
   --  screen, this has no effect.
   procedure Cursor_Up (N : Height_Type := Height_Type'First);

   --  Moves the cursor one cell down. If the cursor is already at the edge of
   --  the screen, this has no effect.
   procedure Cursor_Down (N : Height_Type := Height_Type'First);

   --  Delete screen from cursor to end of screen.
   procedure Delete_Screen_From_Cursor;

   --  Delete from cursor to the end of the line.
   procedure Delete_Line_From_Cursor;

   --  Set cursor position.
   procedure Set_Position
     (X : Width_Type;
      Y : Height_Type);

   --  Set current X position.
   procedure Set_Position (X : Width_Type);

   --  Show cursor.
   procedure Show_Cursor;

   --  Hide cursor.
   procedure Hide_Cursor;

   --  Update cursor position.
   procedure Update_Cursor;

   --  Enable cursor updates.
   procedure Enable_Cursor_Update;

   --  Disable cursor updates.
   procedure Disable_Cursor_Update;

   --  Set text color.
   procedure Set_Text_Color (Color : VGA_Color_Type);

   --  Set background color.
   procedure Set_Bkg_Color (Color : VGA_Color_Type);

   --  Swap text color with background color.
   procedure Swap_Text_With_Bkg_Color;

   --  Set the top and bottom margins to define the scrolling region.
   procedure Set_Scrolling_Margins
     (Top    : Height_Type;
      Bottom : Height_Type);

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
