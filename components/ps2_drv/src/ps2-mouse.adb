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

with SK.Strings;

with Input;
with Log;

with PS2.Constants;
with PS2.Output;
with PS2.I8042;

package body PS2.Mouse
is

   --  Supported PS/2 mouse extensions.
   type Mouse_Type is (Standard_PS2, IMPS2);

   Current_Mouse : Mouse_Type := Standard_PS2;

   Max_Packet_Length : constant := 4;

   --  Range of packets from mouse.
   type Packet_Range is new Positive range 1 .. Max_Packet_Length;

   --  Storage for mouse data packets.
   Packet_Buffer : array (Packet_Range) of SK.Byte := (others => 0);

   --  Mouse data packet lengths depending on mouse type.
   Packet_Length : constant array (Mouse_Type) of Packet_Range
     := (Standard_PS2 => 3,
         IMPS2        => 4);

   Current_Packet : Packet_Range := Packet_Range'First;

   type Mouse_Button_Type is (Btn_Left, Btn_Right, Btn_Middle);

   --  Current state of mouse buttons.
   Button_State : array (Mouse_Button_Type) of Boolean := (others => False);

   --  Mapping of mouse buttons to corresponding keycodes.
   Btn_To_Keycode : constant array (Mouse_Button_Type) of Input.Keysym_Type
     := (Btn_Left   => Input.BTN_LEFT,
         Btn_Right  => Input.BTN_RIGHT,
         Btn_Middle => Input.BTN_MIDDLE);

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

   --  Process mouse button information considering the given packet header.
   procedure Process_Buttons (Header : Packet_Header_Type);

   --  Update state of specified button with new state, reporting a button
   --  input event if a state change occurred.
   procedure Update_Button_State
     (Button    : Mouse_Button_Type;
      New_State : Boolean);

   --  Detect support for Intellimouse extension.
   function Supports_Intellimouse return Boolean;

   -------------------------------------------------------------------------

   procedure Init (Success : out Boolean)
   is
      use type SK.Byte;

      Timeout : Boolean;
      Data    : SK.Byte;
   begin
      Success := False;

      --  Reset device.

      I8042.Write_Aux (Data => Constants.CMD_RESET);
      I8042.Wait_For_Ack (Timeout => Timeout);
      if Timeout then
         Log.Text_IO.Put_Line
           (Item => "PS/2 - Mouse: Unable to reset device, no ACK");
         return;
      end if;

      I8042.Read_Data (Data => Data);
      if Data /= Constants.TEST_PASSED then
         Log.Text_IO.Put_Line
           (Item => "PS/2 - Mouse: Unable to reset device, self-test failed");
         return;
      end if;

      I8042.Read_Data (Data => Data);
      if Data /= Constants.RESET_MOUSE_ID then
         Log.Text_IO.Put_Line
           (Item => "PS/2 - Mouse: Unable to reset device, invalid device ID");
         return;
      end if;

      Log.Text_IO.Put_Line (Item => "PS/2 - Mouse: Device reset");

      --  Enable streaming.

      I8042.Write_Aux (Data => Constants.CMD_ENABLE_STREAMING);
      I8042.Wait_For_Ack (Timeout => Timeout);
      if Timeout then
         Log.Text_IO.Put_Line
           (Item => "PS/2 - Mouse: Unable to enable streaming");
         I8042.Write_Aux (Data => Constants.CMD_RESET);
      else
         Log.Text_IO.Put_Line (Item => "PS/2 - Mouse: Streaming enabled");
      end if;

      --  Probe for supported extensions.

      if Supports_Intellimouse then
         Log.Text_IO.Put_Line ("PS/2 - Mouse: ImPS/2 extension supported");
         Current_Mouse := IMPS2;
      end if;

      --  Set sample rate.

      I8042.Write_Aux (Data => Constants.CMD_SET_SAMPLE_RATE);
      I8042.Wait_For_Ack (Timeout => Timeout);
      if Timeout then
         Log.Text_IO.Put_Line
           (Item => "PS/2 - Mouse: Unable to set sample rate, no ACK");
         I8042.Write_Aux (Data => Constants.CMD_RESET);
         return;
      end if;

      I8042.Write_Aux (Data => Constants.DEFAULT_SAMPLE_RATE);
      I8042.Wait_For_Ack (Timeout => Timeout);
      if Timeout then
         Log.Text_IO.Put_Line
           (Item => "PS/2 - Mouse: Unable to set sample rate to "
            & SK.Strings.Img_Dec (Constants.DEFAULT_SAMPLE_RATE));
         I8042.Write_Aux (Data => Constants.CMD_RESET);
         return;
      end if;
      Log.Text_IO.Put_Line
        (Item => "PS/2 - Mouse: Sample rate set to "
         & SK.Strings.Img_Dec (Constants.DEFAULT_SAMPLE_RATE));

      Success := True;
   end Init;

   -------------------------------------------------------------------------

   procedure Process (Data : SK.Byte)
   is
   begin
      Packet_Buffer (Current_Packet) := Data;

      if Current_Packet = Packet_Length (Current_Mouse) then
         Process_Packets;
         Current_Packet := Packet_Range'First;
      else
         Current_Packet := Current_Packet + 1;
      end if;
   end Process;

   -------------------------------------------------------------------------

   procedure Process_Buttons (Header : Packet_Header_Type)
   is
   begin
      Update_Button_State (Button    => Btn_Left,
                           New_State => Header.Btn_Left);
      Update_Button_State (Button    => Btn_Right,
                           New_State => Header.Btn_Right);
      Update_Button_State (Button    => Btn_Middle,
                           New_State => Header.Btn_Middle);
   end Process_Buttons;

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
         Output.Write (Event => Ev);
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
      Process_Motion  (Header => Header);
      Process_Buttons (Header => Header);

      Packet_Buffer := (others => 0);
   end Process_Packets;

   -------------------------------------------------------------------------

   function Supports_Intellimouse return Boolean
   is
      use type SK.Byte;

      --  Sequence of magic sample rate settings to probe extension.
      type Rate_Sequence is array (Natural range <>) of SK.Byte;

      ImPS2_ID  : constant := 3;
      ImPS2_Seq : constant Rate_Sequence := (1 => 200, 2 => 100, 3 => 80);

      ID      : SK.Byte;
      Timeout : Boolean;
   begin
      for S of ImPS2_Seq loop
         I8042.Write_Aux (Data => Constants.CMD_SET_SAMPLE_RATE);
         I8042.Wait_For_Ack (Timeout => Timeout);
         if Timeout then
            Log.Text_IO.Put_Line
              ("PS/2 - Mouse: Error detecting Intellimouse extension (1)");
            return False;
         end if;

         I8042.Write_Aux (Data => S);
         I8042.Wait_For_Ack (Timeout => Timeout);
         if Timeout then
            Log.Text_IO.Put_Line ("PS/2 - Mouse: Error detecting "
                                  & "Intellimouse extension (2)");
            return False;
         end if;
      end loop;

      I8042.Write_Aux (Data => Constants.CMD_GET_ID);
      I8042.Wait_For_Ack (Timeout => Timeout);
      if Timeout then
         Log.Text_IO.Put_Line
           ("PS/2 - Mouse: Error getting device ID");
         return False;
      end if;

      I8042.Read_Data (Data => ID);
      return ID = ImPS2_ID;
   end Supports_Intellimouse;

   -------------------------------------------------------------------------

   procedure Update_Button_State
     (Button    : Mouse_Button_Type;
      New_State : Boolean)
   is
      Ev : Input.Input_Event_Type := Input.Null_Input_Event;
   begin
      if Button_State (Button) /= New_State then
         Ev.Event_Type := (if New_State then
                              Input.EVENT_PRESS else Input.EVENT_RELEASE);
         Ev.Keycode := Btn_To_Keycode (Button);
         Output.Write (Event => Ev);
         Button_State (Button) := New_State;
      end if;
   end Update_Button_State;

end PS2.Mouse;
