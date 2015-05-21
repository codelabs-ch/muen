--
--  Copyright (C) 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with Ada.Unchecked_Conversion;

with Log;
with Input;
with Mux.Terminals;

package body PS2.Mouse
is

   --  Mouse commands, see http://wiki.osdev.org/Mouse_Input.
   CMD_RESET            : constant := 16#ff#;
   CMD_SET_DEFAULTS     : constant := 16#f6#;
   CMD_ENABLE_STREAMING : constant := 16#f4#;
   CMD_AUX_ENABLE       : constant := 16#a8#;
   CMD_READ_CONFIG      : constant := 16#20#;
   CMD_WRITE_CONFIG     : constant := 16#60#;
   ENABLE_IRQ12         : constant := 1;
   DISABLE_MOUSE_CLOCK  : constant := 5;

   --  Range of packets from mouse.
   type Packet_Range is new Positive range 1 .. 3;

   --  Storage for mouse data packets.
   Packet_Buffer : array (Packet_Range) of SK.Byte := (others => 0);

   Current_Packet : Packet_Range := Packet_Range'First;

   type Mouse_Button_Type is (Btn_Left, Btn_Right, Btn_Middle);

   --  Current state of mouse buttons.
   Button_State : array (Mouse_Button_Type) of Boolean := (others => False);

   --  PS/2 mouse protocol packet header, see
   --  http://www.win.tue.nl/~aeb/linux/kbd/scancodes-13.html
   type Packet_Header_Type is record
      Btn_Left   : Boolean;
      Btn_Right  : Boolean;
      Btn_Middle : Boolean;
      Reserved   : Boolean;
      Sign_X     : Boolean;
      Sign_Y     : Boolean;
      Overflow_X : Boolean;
      Overflow_Y : Boolean;
   end record
     with Size => 8;

   for Packet_Header_Type use record
      Btn_Left   at 0 range 0 .. 0;
      Btn_Right  at 0 range 1 .. 1;
      Btn_Middle at 0 range 2 .. 2;
      Reserved   at 0 range 3 .. 3;
      Sign_X     at 0 range 4 .. 4;
      Sign_Y     at 0 range 5 .. 5;
      Overflow_X at 0 range 6 .. 6;
      Overflow_Y at 0 range 7 .. 7;
   end record;

   --  Process mouse packets currently in the buffer and generate corresponding
   --  input events.
   procedure Process_Packets;

   --  Process mouse motion information considering the given packet header.
   procedure Process_Motion (Header : Packet_Header_Type);

   -------------------------------------------------------------------------

   procedure Init
   is
      use type SK.Byte;

      Data    : SK.Byte;
      Timeout : Boolean;
   begin

      --  Enable auxiliary mouse device.

      Write_Command (Cmd => CMD_AUX_ENABLE);
      Log.Text_IO.Put_Line ("PS/2 - Mouse: AUX device enabled");

      --  Enable IRQ 12 and mouse clock.

      Write_Command (Cmd => CMD_READ_CONFIG);
      Read (Data => Data);
      Data := SK.Byte'Mod
        (SK.Bit_Set (Value => SK.Word64 (Data),
                     Pos   => ENABLE_IRQ12));
      Data := SK.Byte'Mod
        (SK.Bit_Clear (Value => SK.Word64 (Data),
                       Pos   => DISABLE_MOUSE_CLOCK));
      Write_Command (Cmd  => CMD_WRITE_CONFIG);
      Write_Data    (Data => Data);
      Log.Text_IO.Put_Line ("PS/2 - Mouse: Enabled IRQ 12 and mouse clock");

      --  Reset

      Write_Aux (Data => CMD_RESET);
      Wait_For_Ack (Timeout => Timeout);
      if Timeout then
         Log.Text_IO.Put_Line ("PS/2 - Mouse: Unable to reset device");
         return;
      else
         Log.Text_IO.Put_Line ("PS/2 - Mouse: Reset device");
      end if;

      --  Set defaults.

      Write_Aux (Data => CMD_SET_DEFAULTS);
      Wait_For_Ack (Timeout => Timeout);
      if Timeout then
         Log.Text_IO.Put_Line ("PS/2 - Mouse: Unable to set defaults");
         Write_Aux (Data => CMD_RESET);
         return;
      else
         Log.Text_IO.Put_Line ("PS/2 - Mouse: Defaults set");
      end if;

      --  Enable streaming.

      Write_Aux (Data => CMD_ENABLE_STREAMING);
      Wait_For_Ack (Timeout => Timeout);
      if Timeout then
         Log.Text_IO.Put_Line ("PS/2 - Mouse: Unable to enable streaming");
         Write_Aux (Data => CMD_RESET);
      else
         Log.Text_IO.Put_Line ("PS/2 - Mouse: Streaming enabled");
      end if;
   end Init;

   -------------------------------------------------------------------------

   procedure Process (Data : SK.Byte)
   is
   begin
      Packet_Buffer (Current_Packet) := Data;

      if Current_Packet = Packet_Range'Last then
         Process_Packets;
         Current_Packet := Packet_Range'First;
      else
         Current_Packet := Current_Packet + 1;
      end if;
   end Process;

   -------------------------------------------------------------------------

   procedure Process_Motion (Header : Packet_Header_Type)
   is
      use type Interfaces.Integer_32;
      use type SK.Byte;

      Ev : Input.Input_Event_Type := Input.Null_Input_Event;
   begin
      Ev.Event_Type := Input.EVENT_MOTION;
      if Header.Overflow_X then
         Ev.Relative_X := 0;
      elsif Header.Sign_X then
         Ev.Relative_X := -Interfaces.Integer_32
           (not Packet_Buffer (2) + 1);
      else
         Ev.Relative_X := Interfaces.Integer_32 (Packet_Buffer (2));
      end if;

      if Header.Overflow_Y then
         Ev.Relative_Y := 0;
      elsif Header.Sign_Y then
         Ev.Relative_Y := -Interfaces.Integer_32
           (not Packet_Buffer (3) + 1);
      else
         Ev.Relative_Y := Interfaces.Integer_32 (Packet_Buffer (3));
      end if;

      if Ev.Relative_X /= 0 or Ev.Relative_Y /= 0 then
         Mux.Terminals.Process_Input (Event => Ev);
      end if;
   end Process_Motion;

   -------------------------------------------------------------------------

   procedure Process_Packets
   is
      function To_Packet_Header is new Ada.Unchecked_Conversion
        (Source => SK.Byte,
         Target => Packet_Header_Type);

      Header : constant Packet_Header_Type
        := To_Packet_Header (Packet_Buffer (Packet_Buffer'First));
   begin
      Process_Motion (Header => Header);
      Packet_Buffer := (others => 0);
   end Process_Packets;

end PS2.Mouse;
