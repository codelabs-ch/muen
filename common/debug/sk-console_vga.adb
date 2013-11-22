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

   Cur_X         : Width_Type;
   Cur_Y         : Height_Type;
   Cur_Txt_Color : VGA_Color_Type;

   Screen : Screen_Type;
   pragma Import (Ada, Screen);
   for Screen'Address use Base_Address;

   Update_Cursor_Position : Boolean := True;

   --  Scroll screen if current Y position is equal to the last row.
   procedure Scroll;

   -------------------------------------------------------------------------

   procedure Delete_Screen_From_Cursor
   is
   begin

      --  Delete current line from cursor position.

      for X in Cur_X .. Width_Type'Last loop
         Screen (Cur_Y) (X)
           := Screen_Cell_Type'
             (Char     => ' ',
              FG_Color => Light_Grey,
              BG_Color => Black);
      end loop;

      --  Clear remaining screen.

      for Y in Cur_Y + 1 .. Height_Type'Last loop
         for X in Width_Type'Range loop
            Screen (Y) (X)
              := Screen_Cell_Type'
                (Char     => ' ',
                 FG_Color => Light_Grey,
                 BG_Color => Black);
         end loop;
      end loop;
      Update_Cursor;
   end Delete_Screen_From_Cursor;

   -------------------------------------------------------------------------

   procedure Disable_Cursor_Update
   is
   begin
      Update_Cursor_Position := False;
   end Disable_Cursor_Update;

   -------------------------------------------------------------------------

   procedure Enable_Cursor_Update
   is
   begin
      Update_Cursor_Position := True;
   end Enable_Cursor_Update;

   -------------------------------------------------------------------------

   procedure Hide_Cursor
   is
   begin
      IO.Outb (Port  => 16#3d4#,
               Value => 10);
      IO.Outb (Port  => 16#3d5#,
               Value => 16);
   end Hide_Cursor;

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

      Cur_X         := Width_Type'First;
      Cur_Y         := Height_Type'First;
      Cur_Txt_Color := Light_Grey;
      Update_Cursor;
   end Init;

   -------------------------------------------------------------------------

   procedure Put_Tab
   is
      Spaces : constant Width_Type := 4 - (Cur_X mod 4);
   begin
      for S in 1 .. Spaces loop
         Put_Char (Item => ' ');
      end loop;
   end Put_Tab;

   -------------------------------------------------------------------------

   procedure Line_Move_Left
   is
   begin
      if Cur_X > Width_Type'First then
         Cur_X := Cur_X - 1;
      end if;
      Update_Cursor;
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
      Update_Cursor;
   end New_Line;

   -------------------------------------------------------------------------

   procedure Put_Char (Item : Character)
   is
   begin
      Screen (Cur_Y) (Cur_X) := Screen_Cell_Type'
        (Char     => Item,
         FG_Color => Cur_Txt_Color,
         BG_Color => Black);

      if Cur_X = Width_Type'Last then
         New_Line;
      else
         Cur_X := Cur_X + 1;
      end if;
      Update_Cursor;
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
      Update_Cursor;
   end Scroll;

   -------------------------------------------------------------------------

   procedure Set_Position
     (X : Width_Type;
      Y : Height_Type)
   is
   begin
      Set_Position (X => X);
      Cur_Y := Y;
      Update_Cursor;
   end Set_Position;

   -------------------------------------------------------------------------

   procedure Set_Position (X : Width_Type)
   is
   begin
      Cur_X := X;
      Update_Cursor;
   end Set_Position;

   -------------------------------------------------------------------------

   procedure Set_Text_Color (Color : VGA_Color_Type)
   is
   begin
      Cur_Txt_Color := Color;
   end Set_Text_Color;

   -------------------------------------------------------------------------

   procedure Show_Cursor
   is
   begin
      IO.Outb (Port  => 16#3d4#,
               Value => 10);
      IO.Outb (Port  => 16#3d5#,
               Value => 13);
   end Show_Cursor;

   -------------------------------------------------------------------------

   procedure Update_Cursor
   is
      Pos : Positive;
   begin
      if not Update_Cursor_Position then
         return;
      end if;

      Pos := Cursor_Offset + (Natural (Cur_Y - 1) * Natural (Width_Type'Last)
                              + Natural (Cur_X) - 1);

      --  Set high cursor byte

      IO.Outb (Port  => 16#3d4#,
               Value => 16#0e#);
      IO.Outb (Port  => 16#3d5#,
               Value => Byte (Pos / 2 ** 8));

      --  Set low cursor byte

      IO.Outb (Port  => 16#3d4#,
               Value => 16#0f#);
      IO.Outb (Port  => 16#3d5#,
               Value => Byte (Pos));
   end Update_Cursor;

end SK.Console_VGA;
