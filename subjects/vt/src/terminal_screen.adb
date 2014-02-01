--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Console_VGA;
with SK.Console;

with Log;

package body Terminal_Screen
is

   Semicolon : constant := 16#3b#;

   subtype Width_Type  is Natural range 1 .. 80;
   subtype Height_Type is Natural range 1 .. 25;

   package VGA is new SK.Console_VGA
     (Width_Type    => Width_Type,
      Height_Type   => Height_Type,
      Base_Address  => Base_Address,
      Cursor_Offset => Cursor_Offset);

   package Text_IO is new SK.Console
     (Initialize      => VGA.Init,
      Output_New_Line => VGA.New_Line,
      Output_Char     => VGA.Put_Char);

   Color_Table : constant array (SK.Byte range 0 .. 7) of
     VGA.VGA_Color_Type
       := (0 => VGA.Light_Grey,
           1 => VGA.Red,
           2 => VGA.Green,
           3 => VGA.Yellow,
           4 => VGA.Blue,
           5 => VGA.Magenta,
           6 => VGA.Cyan,
           7 => VGA.Black);

   --  Print 'unknown character' message for given FSM state.
   procedure Print_Unknown
     (State : String;
      Char  : SK.Byte);

   --  Execute CSI control function.
   procedure CSI_Dispatch (Char : SK.Byte);

   --  CSI Select Graphic Rendition.
   procedure CSI_Select_SGR;

   --  Add CSI parameter to terminal state.
   procedure CSI_Add_Param (Char : SK.Byte);

   -------------------------------------------------------------------------

   procedure CSI_Add_Param (Char : SK.Byte)
   is
      use type SK.Byte;
   begin
      if Char = Semicolon then
         if Fsm.CSI_Param_Idx = CSI_Param_Range'Last then
            Log.Text_IO.Put_Line (Item => "!! Ignoring extra CSI parameter");
         else
            Fsm.CSI_Param_Idx := Fsm.CSI_Param_Idx + 1;
         end if;
         return;
      end if;

      declare
         Value : Natural := Natural (Char and 16#f#);
      begin
         if Fsm.CSI_Param_Idx = CSI_Empty_Params then
            Fsm.CSI_Param_Idx := Fsm.CSI_Param_Idx + 1;
            Fsm.CSI_Params (Fsm.CSI_Param_Idx) := CSI_Param_Value_Type (Value);
         else
            Value := Natural (Fsm.CSI_Params (Fsm.CSI_Param_Idx)) * 10 + Value;
            if Value <= Natural (CSI_Param_Value_Type'Last) then
               Fsm.CSI_Params (Fsm.CSI_Param_Idx) := CSI_Param_Value_Type
                 (Value);
            else
               Log.Text_IO.Put_Line (Item => "!! Overflow in CSI_Add_Param");
            end if;
         end if;
      end;
   end CSI_Add_Param;

   -------------------------------------------------------------------------

   procedure CSI_Dispatch (Char : SK.Byte)
   is
   begin
      case Char
      is
         when 16#48# =>

            --  CSI H: Cursor position

            VGA.Set_Position (X => Width_Type'First,
                              Y => Height_Type'First);
         when 16#4a# =>

            --  CSI J: ED - Erase Display

            VGA.Delete_Screen_From_Cursor;
         when 16#4b# =>

            --  CSI K: EL - Erase in Line

            VGA.Delete_Line_From_Cursor;
         when 16#68# =>

            --  CSI h: Set mode - ignore

            null;
         when 16#6d# =>

            --  CSI n m: SGR - Select Graphic Rendition

            CSI_Select_SGR;
         when others =>
            Print_Unknown
              (State => "CSI_Dispatch",
               Char  => Char);
      end case;
   end CSI_Dispatch;

   -------------------------------------------------------------------------

   procedure CSI_Select_SGR
   is
      use type SK.Byte;
   begin
      if Fsm.CSI_Param_Idx = CSI_Empty_Params then
         Log.Text_IO.Put_Line (Item => "!! Empty CSI params");
         return;
      end if;

      for Idx in CSI_Param_Range'First .. Fsm.CSI_Param_Idx loop
         declare
            Param : constant CSI_Param_Value_Type := Fsm.CSI_Params (Idx);
         begin
            case Param
            is
               when 0 =>
                  VGA.Set_Text_Color
                    (Color => Color_Table (Color_Table'First));
               when 1 => null;
               when 31 .. 37 =>
                  VGA.Set_Text_Color
                    (Color => Color_Table (SK. Byte (Param) - 30));
               when others    =>
                  Print_Unknown
                    (State => "CSI_Select_SGR",
                     Char  => SK.Byte (Param));
            end case;
         end;
      end loop;
   end CSI_Select_SGR;

   -------------------------------------------------------------------------

   procedure Disable_Cursor_Update
   is
   begin
      VGA.Disable_Cursor_Update;
   end Disable_Cursor_Update;

   -------------------------------------------------------------------------

   procedure Enable_Cursor_Update
   is
   begin
      VGA.Enable_Cursor_Update;
   end Enable_Cursor_Update;

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      Text_IO.Init;
   end Init;

   ----------------------------------------------------------------------

   procedure Print_Unknown
     (State : String;
      Char  : SK.Byte)
   is
   begin
      Log.Text_IO.Put_String (Item => State);
      Log.Text_IO.Put_String (Item => ": Unknown character ");
      Log.Text_IO.Put_Byte   (Item => Char);
      Log.Text_IO.New_Line;
   end Print_Unknown;

   -------------------------------------------------------------------------

   procedure Update (Char : Character)
   is
      use type SK.Byte;

      Pos : constant SK.Byte := SK.Byte (Character'Pos (Char));
   begin

      --  Video Terminal Parser, see http://vt100.net/emu/vt500_parser.png

      case Fsm.State
      is
         when State_Ground =>

            --  GROUND

            case Pos
            is
               when 16#20# .. 16#7f# =>

                  --  Printable

                  Text_IO.Put_Char (Item => Char);
               when 16#07# =>

                  --  BEL, ignore

                  null;
               when 16#08# =>

                  --  BS

                  VGA.Line_Move_Left;
               when 16#09# =>

                  --  HT (Horizontal Tab)

                  VGA.Put_Tab;
               when 16#0a# =>

                  --  LF

                  Text_IO.New_Line;
               when 16#0d# =>

                  --  CR

                  VGA.Set_Position (X => Width_Type'First);
               when 16#1b# =>
                  Fsm.State := State_Escape;
               when others =>
                  Print_Unknown
                    (State => "Ground",
                     Char  => Pos);
            end case;
         when State_Escape =>

            --  ESCAPE

            Fsm := Null_State;

            case Pos
            is
               when 16#5b# =>
                  Fsm.State := State_CSI_Entry;
               when others =>
                  Print_Unknown
                    (State => "Escape",
                     Char  => Pos);
            end case;
         when State_CSI_Entry =>

            --  CSI entry

            Fsm := Null_State;

            case Pos
            is
               when 16#30# .. 16#39# | Semicolon =>
                  Fsm.State := State_CSI_Param;
                  CSI_Add_Param (Char => Pos);
               when 16#3c# .. 16#3f# =>
                  Fsm.State := State_CSI_Param;
                  Fsm.CSI_Collect := Pos;
               when 16#40# .. 16#7e# =>
                  CSI_Dispatch (Char => Pos);
                  Fsm := Null_State;
               when others =>
                  Print_Unknown
                    (State => "CSI entry",
                     Char  => Pos);
            end case;
         when State_CSI_Param =>

            --  CSI param

            case Pos
            is
               when 16#30# .. 16#39# | Semicolon =>
                  CSI_Add_Param (Char => Pos);
               when 16#40# .. 16#7e# =>
                  CSI_Dispatch (Char => Pos);
                  Fsm := Null_State;
               when others =>
                  Print_Unknown
                    (State => "CSI param",
                     Char  => Pos);
            end case;
      end case;
   end Update;

   -------------------------------------------------------------------------

   procedure Update_Cursor
   is
   begin
      VGA.Update_Cursor;
   end Update_Cursor;

end Terminal_Screen;
