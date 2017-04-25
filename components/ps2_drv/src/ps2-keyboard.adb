--
--  Copyright (C) 2013, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Input;

with Log;

with PS2.Constants;
with PS2.I8042;
with PS2.Keyboard.Scancodes;
with PS2.Output;

package body PS2.Keyboard
is

   --  Flag to track the escape state of the current scancode sequence.
   Escaped : Boolean := False;

   --  Converts a given scancode to a key event. If the scancode is part of an
   --  escaped sequence, Event is set to Null_Key_Event.
   procedure Convert_Scancode
     (Code  :     SK.Byte;
      Event : out Input.Input_Event_Type);

   -------------------------------------------------------------------------

   procedure Convert_Scancode
     (Code  :     SK.Byte;
      Event : out Input.Input_Event_Type)
   is
      use type SK.Byte;

      Idx : SK.Byte;
   begin
      if Code = 16#e0# or Code = 16#e1# then
         Escaped := True;
         Event   := Input.Null_Input_Event;
         return;
      end if;

      Idx := Code mod 16#80#;
      if Escaped then
         Event.Keycode := Scancodes.Escaped_Scancode_Map (Idx);
         Escaped       := False;
      else
         Event.Keycode := Scancodes.Scancode_Map (Idx);
      end if;

      Event.Event_Type :=
        (if Code < 16#80# then Input.EVENT_PRESS else Input.EVENT_RELEASE);
   end Convert_Scancode;

   -------------------------------------------------------------------------

   procedure Init
   is
      Timeout : Boolean;
   begin

      --  Set typematic rate and delay.

      I8042.Write_Command (Cmd => Constants.CMD_SET_SAMPLE_RATE);
      I8042.Wait_For_Ack (Timeout => Timeout);
      if Timeout then
         Log.Text_IO.Put_Line
           ("PS/2 - KBD: Unable to set typematic rate and delay, no ACK");
         return;
      end if;

      --  Repeat rate 30 Hz, delay 250 ms.

      I8042.Write_Data (Data => 0);
      I8042.Wait_For_Ack (Timeout => Timeout);
      if Timeout then
         Log.Text_IO.Put_Line
           ("PS/2 - KBD: Unable to rate to 30 Hz and delay to 250 ms");
         return;
      end if;

      Log.Text_IO.Put_Line ("PS/2 - KBD: Typematic rate and delay set");
   end Init;

   -------------------------------------------------------------------------

   procedure Process (Data : SK.Byte)
   is
      use type Input.Input_Event_Type;

      Ev : Input.Input_Event_Type;
   begin
      Convert_Scancode (Code  => Data,
                        Event => Ev);

      if Ev /= Input.Null_Input_Event then
         Output.Write (Event => Ev);
      end if;
   end Process;

end PS2.Keyboard;
