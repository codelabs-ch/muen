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

with Interfaces;

with SK.CPU;
with SK.IO;
with SK.Hypercall;
with SK.Strings;

with VT_Channels;
with Vt_Component.Events;

with Log;
with Mux.Screens;
with Mux.Channels;

package body Mux.Terminals
is

   Active_Slot : Output_Channel_Range := Output_Channel_Range'First
     with Atomic;

   VGA_CRT_Register       : constant := 16#3d4#;
   VGA_CRT_Idx_Start_High : constant := 16#0c#;
   VGA_CRT_Idx_Start_Low  : constant := 16#0d#;

   type Slot_Mapping is array (Input.Keysym_Type) of Output_Channel_Range;

   --  Key to session slot mapping
   Slot_Map : constant Slot_Mapping :=
     (Input.KEY_F1 => 1,
      Input.KEY_F2 => 2,
      others       => Output_Channel_Range'Last);

   type Flags_Type is array (Input_Channel_Range) of Boolean;

   --  Control key handling.
   Control_Key         : constant Input.Keysym_Type := Input.KEY_RIGHTCTRL;
   Control_Key_Pressed : Boolean := False;

   --  Pending data flags per channel.
   Pending_Data : Flags_Type := (others => False);

   --  Read data from input channels if new data is present.
   procedure Update_In_Channels;

   --  Set VGA start address, see:
   --  www.phatcode.net/res/224/files/html/ch23/23-04.html
   procedure Set_VGA_Start (Address : SK.Word16);

   --  Returns True if one of the channels has data to consume.
   function Has_Pending_Data return Boolean;

   -------------------------------------------------------------------------

   function Get_Active_Slot return Output_Channel_Range
   is
      AS : constant Output_Channel_Range := Active_Slot;
   begin
      return AS;
   end Get_Active_Slot;

   -------------------------------------------------------------------------

   function Has_Pending_Data return Boolean
   is
   begin
      for I in Pending_Data'Range loop
         if Pending_Data (I) then
            return True;
         end if;
      end loop;

      return False;
   end Has_Pending_Data;

   -------------------------------------------------------------------------

   procedure Initialize
   is
      AS : constant Output_Channel_Range := Active_Slot;
   begin
      for I in Output_Channel_Range loop
         Mux.Screens.Init (Screen => I);
      end loop;
      Set (Slot => AS);
      Mux.Channels.Init;
   end Initialize;

   -------------------------------------------------------------------------

   procedure Process_Input (Event : Input.Input_Event_Type)
   is
      use Input;

      AS : constant Output_Channel_Range := Active_Slot;
   begin
      case Event.Event_Type is
         when Input.EVENT_RELEASE =>
            case Event.Keycode is
               when Control_Key => Control_Key_Pressed := False;
               when others      => null;
            end case;

         when Input.EVENT_PRESS =>
            case Event.Keycode is
               when Control_Key => Control_Key_Pressed := True;

               when KEY_F1 | KEY_F2 | KEY_F3 | KEY_F4 | KEY_F5 | KEY_F6 =>

                  --  Handle host key presses.

                  if AS /= Slot_Map (Event.Keycode) then
                     Set (Slot => Slot_Map (Event.Keycode));
                     Log.Text_IO.Put_Line
                       (Item => "Switching to VT "
                        & SK.Strings.Img (Interfaces.Unsigned_8
                          (Slot_Map (Event.Keycode))));
                  end if;
                  return;
               when KEY_F10 =>

                  if Control_Key_Pressed then

                     --  Initiate reset of slot 1.

                     SK.Hypercall.Trigger_Event
                       (Number => Vt_Component.Events.Reset_Slot_1_ID);
                     return;
                  end if;

               when KEY_F11 =>

                  if Control_Key_Pressed then

                     --  Reboot the system.

                     SK.Hypercall.Trigger_Event
                       (Number => Vt_Component.Events.Reboot_ID);
                     return;
                  end if;

               when KEY_F12 =>

                  if Control_Key_Pressed then

                     --  Shutdown the system.

                     SK.Hypercall.Trigger_Event
                       (Number => Vt_Component.Events.Shutdown_ID);
                     return;
                  end if;

               when others => null;
            end case;
         when others => null;
      end case;

      Mux.Channels.Write
        (Channel => AS,
         Event   => Event);
   end Process_Input;

   -------------------------------------------------------------------------

   procedure Run
   is
   begin
      Log.Text_IO.Put_Line (Item => "VT subject running");

      loop
         SK.CPU.Cli;
         if not Has_Pending_Data then
            SK.CPU.Sti;
            SK.CPU.Hlt;
         else
            SK.CPU.Sti;
         end if;
         Update_In_Channels;
      end loop;
   end Run;

   -------------------------------------------------------------------------

   procedure Set (Slot : Output_Channel_Range)
   is
      use type SK.Word16;
   begin
      Active_Slot := Slot;
      Mux.Screens.Set_Active (Screen => Slot);
      Set_VGA_Start (Address => SK.Word16 (Slot - 1) * 16#800#);
   end Set;

   -------------------------------------------------------------------------

   procedure Set_Pending_Flag (Channel_Nr : Input_Channel_Range)
   is
   begin
      Pending_Data (Channel_Nr) := True;
   end Set_Pending_Flag;

   -------------------------------------------------------------------------

   procedure Set_VGA_Start (Address : SK.Word16)
   is
      use type SK.Word16;
   begin
      SK.IO.Outw
        (Port  => VGA_CRT_Register,
         Value => VGA_CRT_Idx_Start_High + (Address and 16#ff00#));
      SK.IO.Outw
        (Port  => VGA_CRT_Register,
         Value => VGA_CRT_Idx_Start_Low + ((Address * 2 ** 8) and 16#ff00#));
   end Set_VGA_Start;

   -------------------------------------------------------------------------

   procedure Update_In_Channels
   is
      use VT_Channels;

      use type VT_Channels.VT_Channel_Rdr.Result_Type;

      Data : Character;
      Res  : VT_Channel_Rdr.Result_Type;
      C    : Input_Channel_Range := Input_Channel_Range'Last;
   begin
      while Has_Pending_Data loop
         if Pending_Data (C) then
            for I in Positive range 1 .. 80 loop
               Channels.Read (Channel => C,
                              Char    => Data,
                              Result  => Res);

               case Res is
                  when VT_Channel_Rdr.Incompatible_Interface =>
                     Log.Text_IO.Put_Line
                       (Item => "Channel "
                        & SK.Strings.Img
                          (Interfaces.Unsigned_8 (C)));
                     Log.Text_IO.Put_Line
                       (Item => ": Incompatible interface detected");
                  when VT_Channel_Rdr.Epoch_Changed =>
                     Log.Text_IO.Put_Line
                       (Item => "Channel "
                        & SK.Strings.Img (Interfaces.Unsigned_8 (C))
                        & ": Epoch changed");
                  when VT_Channel_Rdr.No_Data =>
                     SK.CPU.Cli;
                     Pending_Data (C) := Channels.Has_Pending_Data
                       (Channel => C);
                     SK.CPU.Sti;
                  when VT_Channel_Rdr.Overrun_Detected =>
                     null;
                  when VT_Channel_Rdr.Inactive =>
                     Log.Text_IO.Put_Line
                       (Item => "Channel "
                        & SK.Strings.Img (Interfaces.Unsigned_8 (C))
                        & ": Inactive");
                     SK.CPU.Cli;
                     Pending_Data (C) := False;
                     Screens.Init (Screen => Output_Channel_Range (C));
                     SK.CPU.Sti;
                  when VT_Channel_Rdr.Success =>
                     SK.CPU.Cli;
                     Screens.Update
                       (Screen => Output_Channel_Range (C),
                        Char   => Data);
                     SK.CPU.Sti;
               end case;

               exit when Res /= VT_Channel_Rdr.Success;
            end loop;
         end if;

         if C = Input_Channel_Range'First then
            C := Input_Channel_Range'Last;
         else
            C := C - 1;
         end if;
      end loop;
   end Update_In_Channels;

end Mux.Terminals;
