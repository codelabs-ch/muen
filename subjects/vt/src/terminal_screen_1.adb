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

with SK.Console_VGA;
with SK.Console;

with Log;

package body Terminal_Screen_1
is

   subtype Width_Type  is Natural range 1 .. 80;
   subtype Height_Type is Natural range 1 .. 25;

   package VGA is new SK.Console_VGA
     (Width_Type   => Width_Type,
      Height_Type  => Height_Type,
      Base_Address => System'To_Address (16#10000#));

   package Text_IO is new SK.Console
     (Initialize      => VGA.Init,
      Output_New_Line => VGA.New_Line,
      Output_Char     => VGA.Put_Char);

   type State_Type is
     (State_Ground,
      State_Escape,
      State_CSI_Entry);

   Current_State : State_Type := State_Ground;

   --  Print 'unknown character' message.
   procedure Print_Unknown (Char : SK.Byte);

   --  Execute CSI control function.
   procedure CSI_Dispatch (Char : SK.Byte);

   -------------------------------------------------------------------------

   procedure CSI_Dispatch (Char : SK.Byte)
   is
   begin
      Log.Text_IO.Put_String (Item => "* CSI dispatch ");
      Log.Text_IO.Put_Byte (Item => Char);
      Log.Text_IO.New_Line;

      case Char
      is
         when 16#48# =>

            --  CSI H: Cursor position

            VGA.Set_Position (X => Width_Type'First,
                              Y => Height_Type'First);
         when 16#4a# =>

            --  ED0

            VGA.Delete_Char;
         when others =>
            Print_Unknown (Char => Char);
      end case;
   end CSI_Dispatch;

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      Text_IO.Init;
   end Init;

   ----------------------------------------------------------------------

   procedure Print_Unknown (Char : SK.Byte)
   is
   begin
      Log.Text_IO.Put_String (Item => "!! Unknown character ");
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

      case Current_State
      is
         when State_Ground =>
            case Pos
            is
               when 16#20# .. 16#7f# =>

                  --  Printable

                  Text_IO.Put_Char (Item => Char);
               when 16#08# =>

                  --  BS

                  VGA.Line_Move_Left;
               when 16#0a# =>

                  --  LF

                  Text_IO.New_Line;
               when 16#0d# =>

                  --  CR, ignore

                  return;
               when 16#1b# =>
                  Log.Text_IO.Put_Line (Item => "-> Escape");
                  Current_State := State_Escape;
               when others =>
                  Print_Unknown (Char => Pos);
            end case;
         when State_Escape =>
            case Pos
            is
               when 16#5b# =>
                  Log.Text_IO.Put_Line (Item => "--> CSI entry");
                  Current_State := State_CSI_Entry;
               when others =>
                  Print_Unknown (Char => Pos);
            end case;
         when State_CSI_Entry =>
            case Pos
            is
               when 16#40# .. 16#7e# =>
                  CSI_Dispatch (Char => Pos);
                  Log.Text_IO.Put_Line (Item => "> Ground");
                  Current_State := State_Ground;
               when others =>
                  Print_Unknown (Char => Pos);
            end case;
      end case;
   end Update;

end Terminal_Screen_1;
