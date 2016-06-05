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
with PS2.Keyboard.Scancodes;

package body PS2.Keyboard
with
   Refined_State => (State => Escaped)
is

   --  Flag to track the escape state of the current scancode sequence.
   Escaped : Boolean := False;

   --  Converts a given scancode to a key event. If the scancode is part of an
   --  escaped sequence, Event is set to Null_Key_Event.
   procedure Convert_Scancode
     (Code  :     SK.Byte;
      Event : out Input.Input_Event_Type)
   with
      Global  => (In_Out => Escaped),
      Depends => (Escaped =>+ Code,
                  Event   => (Escaped, Code));

   -------------------------------------------------------------------------

   procedure Convert_Scancode
     (Code  :     SK.Byte;
      Event : out Input.Input_Event_Type)
   is
      use type SK.Byte;

      Idx : SK.Byte;
   begin
      Event := Input.Null_Input_Event;

      if Code = 16#e0# or Code = 16#e1# then
         Escaped := True;
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

   procedure Process (Data : SK.Byte)
   with
      Refined_Global  => (In_Out => (Escaped, Output.State, X86_64.State)),
      Refined_Depends => ((Escaped,
                           Output.State,
                           X86_64.State) =>+ (Data, Escaped))
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
