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

with SK.IO;

package body SK.Console_VGA
is

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

   type Screen_Cell_Type is record
      Char     : Character;
      FG_Color : VGA_Color_Type;
      BG_Color : VGA_Color_Type;
   end record;

   for Screen_Cell_Type use record
      Char     at 0 range 0 .. 7;
      FG_Color at 1 range 0 .. 3;
      BG_Color at 1 range 4 .. 7;
   end record;
   for Screen_Cell_Type'Size use 16;

   --  VGA screen row.
   type Screen_Row_Type is array (Width_Type'Range) of Screen_Cell_Type;

   --  VGA screen.
   type Screen_Type is array (Height_Type) of Screen_Row_Type;

   Cur_X  : Width_Type;
   Cur_Y  : Height_Type;
   Screen : Screen_Type;
   pragma Import (Ada, Screen);
   for Screen'Address use Base_Address;

   --  Scroll screen if current Y position is equal to the last row.
   procedure Scroll;

   -------------------------------------------------------------------------

   procedure Delete_Char
   is
   begin
      Screen (Cur_Y) (Cur_X) := Screen_Cell_Type'
        (Char     => ' ',
         FG_Color => Light_Grey,
         BG_Color => Black);
   end Delete_Char;

   -------------------------------------------------------------------------

   procedure Disable_Cursor
   is
   begin
      IO.Outb (Port  => 16#3d4#,
               Value => 10);
      IO.Outb (Port  => 16#3d5#,
               Value => 16);
   end Disable_Cursor;

   -------------------------------------------------------------------------

   procedure Enable_Cursor
   is
   begin
      IO.Outb (Port  => 16#3d4#,
               Value => 10);
      IO.Outb (Port  => 16#3d5#,
               Value => 13);
   end Enable_Cursor;

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      Screen := Screen_Type'
        (others => Screen_Row_Type'
           (others => Screen_Cell_Type'
              (Char     => ' ',
               FG_Color => Light_Grey,
               BG_Color => Black)));

      Cur_X := Width_Type'First;
      Cur_Y := Height_Type'First;
   end Init;

   -------------------------------------------------------------------------

   procedure Line_Move_Left
   is
   begin
      if Cur_X > Width_Type'First then
         Cur_X := Cur_X - 1;
      end if;
   end Line_Move_Left;

   -------------------------------------------------------------------------

   procedure New_Line
   is
   begin
      Cur_X := Width_Type'First;
      if Cur_Y = Height_Type'Last then
         Scroll;
      else
         Cur_Y := Cur_Y + 1;
      end if;
   end New_Line;

   -------------------------------------------------------------------------

   procedure Put_Char (Item : Character)
   is
   begin
      Screen (Cur_Y) (Cur_X) := Screen_Cell_Type'
        (Char     => Item,
         FG_Color => Light_Grey,
         BG_Color => Black);

      if Cur_X = Width_Type'Last then
         New_Line;
      else
         Cur_X := Cur_X + 1;
      end if;
   end Put_Char;

   -------------------------------------------------------------------------

   procedure Scroll
   is
      subtype Console_To_Last_Row is Height_Type range
        Height_Type'First .. Height_Type'Last - 1;
   begin
      for Y in Console_To_Last_Row
      loop
         Screen (Y) := Screen (Y + 1);
      end loop;

      Screen (Height_Type'Last) := Screen_Row_Type'
        (others => Screen_Cell_Type'
           (Char     => ' ',
            FG_Color => White,
            BG_Color => Black));
   end Scroll;

   -------------------------------------------------------------------------

   procedure Set_Position
     (X : Width_Type;
      Y : Height_Type)
   is
   begin
      Cur_X := X;
      Cur_Y := Y;
   end Set_Position;

end SK.Console_VGA;
