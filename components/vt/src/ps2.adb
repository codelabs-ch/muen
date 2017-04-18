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

with SK.IO;
with SK.Bitops;

with PS2.Keyboard;
with PS2.Mouse;

package body PS2
is

   --  PS/2 constants.

   DATA_REGISTER    : constant := 16#60#;
   STATUS_REGISTER  : constant := 16#64#;
   COMMAND_REGISTER : constant := 16#64#;
   ACKNOWLEDGE      : constant := 16#fa#;
   WRITE_TO_AUX     : constant := 16#d4#;

   OUTPUT_BUFFER_STATUS : constant := 0;
   INPUT_BUFFER_STATUS  : constant := 1;
   AUX_DATA             : constant := 5;

   --  Wait until input buffer is ready for sending data to the PS/2
   --  controller.
   procedure Wait_Input_Ready;

   --  Wait until output buffer is ready for receiving data from the PS/2
   --  controller.
   procedure Wait_Output_Ready;

   --  Returns true if the input buffer is ready for sending data to the PS/2
   --  controller.
   function Is_Input_Ready return Boolean;

   --  Returns true if the output buffer is ready for receiving data from the
   --  PS/2 controller.
   function Is_Output_Ready return Boolean;

   -------------------------------------------------------------------------

   procedure Handle_Interrupt
   is
      Status, Data : SK.Byte;
   begin
      loop
         SK.IO.Inb (Port  => STATUS_REGISTER,
                    Value => Status);
         exit when not SK.Bitops.Bit_Test
           (Value => SK.Word64 (Status),
            Pos   => OUTPUT_BUFFER_STATUS);

         SK.IO.Inb (Port  => DATA_REGISTER,
                    Value => Data);

         if SK.Bitops.Bit_Test
           (Value => SK.Word64 (Status),
            Pos   => AUX_DATA)
         then
            Mouse.Process (Data => Data);
         else
            Keyboard.Process (Data => Data);
         end if;
      end loop;
   end Handle_Interrupt;

   -------------------------------------------------------------------------

   function Is_Input_Ready return Boolean
   is
      Status : SK.Byte;
   begin
      SK.IO.Inb (Port  => STATUS_REGISTER,
                 Value => Status);
      return not SK.Bitops.Bit_Test
        (Value => SK.Word64 (Status),
         Pos   => INPUT_BUFFER_STATUS);
   end Is_Input_Ready;

   -------------------------------------------------------------------------

   function Is_Output_Ready return Boolean
   is
      Status : SK.Byte;
   begin
      SK.IO.Inb (Port  => STATUS_REGISTER,
                 Value => Status);
      return SK.Bitops.Bit_Test
        (Value => SK.Word64 (Status),
         Pos   => OUTPUT_BUFFER_STATUS);
   end Is_Output_Ready;

   -------------------------------------------------------------------------

   procedure Read (Data : out SK.Byte)
   is
   begin
      Wait_Output_Ready;
      SK.IO.Inb (Port  => DATA_REGISTER,
                 Value => Data);
   end Read;

   -------------------------------------------------------------------------

   procedure Wait_For_Ack
     (Loops    :     Natural := 1000;
      Timeout  : out Boolean)
   is
      use type SK.Byte;

      Data : SK.Byte;
   begin
      for I in 1 .. Loops loop
         if Is_Input_Ready then
            SK.IO.Inb (Port  => DATA_REGISTER,
                       Value => Data);
            if Data = ACKNOWLEDGE then
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
   begin
      loop
         exit when Is_Input_Ready;
      end loop;
   end Wait_Input_Ready;

   -------------------------------------------------------------------------

   procedure Wait_Output_Ready
   is
   begin
      loop
         exit when Is_Output_Ready;
      end loop;
   end Wait_Output_Ready;

   -------------------------------------------------------------------------

   procedure Write_Aux (Data : SK.Byte)
   is
   begin
      Write_Command (Cmd  => WRITE_TO_AUX);
      Write_Data    (Data => Data);
   end Write_Aux;

   -------------------------------------------------------------------------

   procedure Write_Command (Cmd : SK.Byte)
   is
   begin
      Wait_Input_Ready;
      SK.IO.Outb (Port  => COMMAND_REGISTER,
                  Value => Cmd);
   end Write_Command;

   -------------------------------------------------------------------------

   procedure Write_Data (Data : SK.Byte)
   is
   begin
      Wait_Input_Ready;
      SK.IO.Outb (Port  => DATA_REGISTER,
                  Value => Data);
   end Write_Data;

end PS2;
