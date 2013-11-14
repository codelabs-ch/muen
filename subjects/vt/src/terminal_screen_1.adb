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
      Base_Address => System'To_Address (16#000b_8000#));

   package Text_IO is new SK.Console
     (Initialize      => VGA.Init,
      Output_New_Line => VGA.New_Line,
      Output_Char     => VGA.Put_Char);

   type State_Type is
     (State_Ground,
      State_Escape,
      State_CSI_Entry,
      State_CSI_Param);

   type CSI_Param_Idx_Range is range 0 .. 16;

   CSI_Empty_Params : constant CSI_Param_Idx_Range
     := CSI_Param_Idx_Range'First;

   subtype CSI_Param_Range is CSI_Param_Idx_Range range
     1 .. CSI_Param_Idx_Range'Last;

   type CSI_Param_Array is array (CSI_Param_Range) of SK.Byte;

   type Terminal_State_Type is record
      State         : State_Type;
      CSI_Params    : CSI_Param_Array;
      CSI_Param_Idx : CSI_Param_Idx_Range;
   end record;

   Null_State : constant Terminal_State_Type
     := (State         => State_Ground,
         CSI_Params    => (others => 0),
         CSI_Param_Idx => CSI_Param_Idx_Range'First);

   --  Terminal emulation state machine.
   Fsm : Terminal_State_Type := Null_State;

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

   --  Print 'unknown character' message.
   procedure Print_Unknown (Char : SK.Byte);

   --  Execute CSI control function.
   procedure CSI_Dispatch (Char : SK.Byte);

   --  CSI Select Graphic Rendition.
   procedure CSI_Select_SGR;

   --  Add CSI parameter to state.
   procedure CSI_Add_Param (Char : SK.Byte);

   -------------------------------------------------------------------------

   procedure CSI_Add_Param (Char : SK.Byte)
   is
   begin
      if Fsm.CSI_Param_Idx < CSI_Param_Range'Last then
         Fsm.CSI_Param_Idx                  := Fsm.CSI_Param_Idx + 1;
         Fsm.CSI_Params (Fsm.CSI_Param_Idx) := Char;
      else
         Log.Text_IO.Put_Line (Item => "!! CSI params overrun");
      end if;
   end CSI_Add_Param;

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

            --  CSI J: Erase display

            VGA.Delete_Screen_From_Cursor;
         when 16#6d# =>

            --  CSI n m: SGR - Select Graphic Rendition

            CSI_Select_SGR;
         when others =>
            Print_Unknown (Char => Char);
      end case;
   end CSI_Dispatch;

   -------------------------------------------------------------------------

   procedure CSI_Select_SGR
   is
      use type SK.Byte;
   begin
      Log.Text_IO.Put_Line (Item => "** CSI_Select_SGR ");

      if Fsm.CSI_Param_Idx = CSI_Empty_Params then
         Log.Text_IO.Put_Line ("!! Empty CSI params");
         return;
      end if;

      for Idx in CSI_Param_Range'First .. Fsm.CSI_Param_Idx loop
         declare
            Param : constant SK.Byte := Fsm.CSI_Params (Idx);
         begin
            case Param
            is
               when 16#30# .. 16#37# =>
                  VGA.Set_Text_Color
                    (Color => Color_Table (Param and 16#7#));
               when 16#3b# =>

                  --  Semicolon, ignore
                  null;
               when others =>
                  Print_Unknown (Char => Param);
            end case;
         end;
      end loop;
   end CSI_Select_SGR;

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

      case Fsm.State
      is
         when State_Ground =>

            --  GROUND

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
                  Fsm.State := State_Escape;
               when others =>
                  Print_Unknown (Char => Pos);
            end case;
         when State_Escape =>

            --  ESCAPE

            Fsm := Null_State;

            case Pos
            is
               when 16#5b# =>
                  Log.Text_IO.Put_Line (Item => "--> CSI entry");
                  Fsm.State := State_CSI_Entry;
               when others =>
                  Print_Unknown (Char => Pos);
            end case;
         when State_CSI_Entry =>

            --  CSI entry

            Fsm := Null_State;

            case Pos
            is
               when 16#30# .. 16#39# | 16#3b# =>
                  Log.Text_IO.Put_Line (Item => "---> CSI param ");
                  Fsm.State := State_CSI_Param;

                  CSI_Add_Param (Char => Pos);
               when 16#40# .. 16#7e# =>
                  CSI_Dispatch (Char => Pos);
                  Log.Text_IO.Put_Line (Item => "> Ground");
                  Fsm := Null_State;
               when others =>
                  Print_Unknown (Char => Pos);
            end case;
         when State_CSI_Param =>

            --  CSI param

            case Pos
            is
               when 16#30# .. 16#39# | 16#3b# =>
                  CSI_Add_Param (Char => Pos);
               when 16#40# .. 16#7e# =>
                  CSI_Dispatch (Char => Pos);
                  Log.Text_IO.Put_Line (Item => "> Ground");
                  Fsm := Null_State;
               when others =>
                  Print_Unknown (Char => Pos);
            end case;
      end case;
   end Update;

end Terminal_Screen_1;
