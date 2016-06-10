--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Bitops;
with SK.IO;

with PS2.Constants;

with Log;

package body PS2.I8042
is

   --  Wait until input buffer is ready for sending data to the PS/2
   --  controller.
   procedure Wait_Input_Ready;

   --  Wait until output buffer is ready for receiving data from the PS/2
   --  controller.
   procedure Wait_Output_Ready;

   --  Returns true if the input buffer is ready for sending data to the PS/2
   --  controller.
   procedure Send_State (Ready : out Boolean);

   --  Returns true if the output buffer is ready for receiving data from the
   --  PS/2 controller.
   procedure Receive_State (Ready : out Boolean);

   -------------------------------------------------------------------------

   procedure Init
   is
      Data    : SK.Byte;
      Timeout : Boolean;
   begin

      --  Enable auxiliary mouse device.

      I8042.Write_Command (Cmd => Constants.CMD_ENABLE_AUX);
      Log.Text_IO.Put_Line ("PS/2 - Mouse: AUX device enabled");

      --  Enable IRQ 12 and mouse clock.

      I8042.Write_Command (Cmd => Constants.CMD_READ_CONFIG);
      I8042.Read_Data (Data => Data);
      Data := SK.Byte'Mod
        (SK.Bitops.Bit_Set (Value => SK.Word64 (Data),
                            Pos   => Constants.IRQ_AUX));
      Data := SK.Byte'Mod
        (SK.Bitops.Bit_Clear (Value => SK.Word64 (Data),
                              Pos   => Constants.DISABLE_CLOCK_AUX));
      I8042.Write_Command (Cmd  => Constants.CMD_WRITE_CONFIG);
      I8042.Write_Data    (Data => Data);
      Log.Text_IO.Put_Line ("PS/2 - Mouse: Enabled IRQ 12 and mouse clock");

      --  Reset

      I8042.Write_Aux (Data => Constants.CMD_RESET);
      I8042.Wait_For_Ack (Timeout => Timeout);
      if Timeout then
         Log.Text_IO.Put_Line ("PS/2 - Mouse: Unable to reset device");
         return;
      else
         Log.Text_IO.Put_Line ("PS/2 - Mouse: Reset device");
      end if;

      --  Set defaults.

      I8042.Write_Aux (Data => Constants.CMD_SET_DEFAULTS);
      I8042.Wait_For_Ack (Timeout => Timeout);
      if Timeout then
         Log.Text_IO.Put_Line ("PS/2 - Mouse: Unable to set defaults");
         I8042.Write_Aux (Data => Constants.CMD_RESET);
         return;
      else
         Log.Text_IO.Put_Line ("PS/2 - Mouse: Defaults set");
      end if;

      --  Enable streaming.

      I8042.Write_Aux (Data => Constants.CMD_ENABLE_STREAMING);
      I8042.Wait_For_Ack (Timeout => Timeout);
      if Timeout then
         Log.Text_IO.Put_Line ("PS/2 - Mouse: Unable to enable streaming");
         I8042.Write_Aux (Data => Constants.CMD_RESET);
      else
         Log.Text_IO.Put_Line ("PS/2 - Mouse: Streaming enabled");
      end if;
   end Init;

   -------------------------------------------------------------------------

   procedure Read_Data (Data : out SK.Byte)
   is
   begin
      Wait_Output_Ready;
      SK.IO.Inb (Port  => Constants.DATA_REGISTER,
                 Value => Data);
   end Read_Data;

   -------------------------------------------------------------------------

   procedure Read_Status (Status : out SK.Byte)
   is
   begin
      SK.IO.Inb (Port  => Constants.STATUS_REGISTER,
                 Value => Status);
   end Read_Status;

   -------------------------------------------------------------------------

   procedure Receive_State (Ready : out Boolean)
   is
      Status : SK.Byte;
   begin
      SK.IO.Inb (Port  => Constants.STATUS_REGISTER,
                 Value => Status);
      Ready := SK.Bitops.Bit_Test
        (Value => SK.Word64 (Status),
         Pos   => Constants.OUTPUT_BUFFER_STATUS);
   end Receive_State;

   -------------------------------------------------------------------------

   procedure Send_State (Ready : out Boolean)
   is
      Status : SK.Byte;
   begin
      SK.IO.Inb (Port  => Constants.STATUS_REGISTER,
                 Value => Status);
      Ready := not SK.Bitops.Bit_Test
        (Value => SK.Word64 (Status),
         Pos   => Constants.INPUT_BUFFER_STATUS);
   end Send_State;

   -------------------------------------------------------------------------

   procedure Wait_For_Ack
     (Loops    :     Natural := 1000;
      Timeout  : out Boolean)
   is
      use type SK.Byte;

      Data  : SK.Byte;
      Ready : Boolean;
   begin
      for I in 1 .. Loops loop
         Send_State (Ready => Ready);
         if Ready then
            SK.IO.Inb (Port  => Constants.DATA_REGISTER,
                       Value => Data);
            if Data = Constants.ACKNOWLEDGE then
               Timeout := False;
               return;
            end if;
         end if;
      end loop;

      Timeout := True;
   end Wait_For_Ack;

   -------------------------------------------------------------------------

   procedure Wait_Input_Ready
   is
      Ready : Boolean;
   begin
      loop
         Send_State (Ready => Ready);
         exit when Ready;
      end loop;
   end Wait_Input_Ready;

   -------------------------------------------------------------------------

   procedure Wait_Output_Ready
   is
      Ready : Boolean;
   begin
      loop
         Receive_State (Ready => Ready);
         exit when Ready;
      end loop;
   end Wait_Output_Ready;

   -------------------------------------------------------------------------

   procedure Write_Aux (Data : SK.Byte)
   is
   begin
      Write_Command (Cmd  => Constants.CMD_WRITE_AUX);
      Write_Data    (Data => Data);
   end Write_Aux;

   -------------------------------------------------------------------------

   procedure Write_Command (Cmd : SK.Byte)
   is
   begin
      Wait_Input_Ready;
      SK.IO.Outb (Port  => Constants.COMMAND_REGISTER,
                  Value => Cmd);
   end Write_Command;

   -------------------------------------------------------------------------

   procedure Write_Data (Data : SK.Byte)
   is
   begin
      Wait_Input_Ready;
      SK.IO.Outb (Port  => Constants.DATA_REGISTER,
                  Value => Data);
   end Write_Data;

end PS2.I8042;
