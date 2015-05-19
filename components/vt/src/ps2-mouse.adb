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

with Log;

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

end PS2.Mouse;
