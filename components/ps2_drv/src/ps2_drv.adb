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

with SK.CPU;
with SK.IO;

with Interrupt_Handler;
pragma Unreferenced (Interrupt_Handler);

with Interrupts;
with Log;

with PS2.Constants;
with PS2.Keyboard;
with PS2.Mouse;
with PS2.Output;

procedure PS2_Drv
is
   --  Handle PS/2 interrupt.
   procedure Handle_Interrupt
   is
      Status, Data : SK.Byte;
   begin
      loop
         SK.IO.Inb (Port  => PS2.Constants.STATUS_REGISTER,
                    Value => Status);
         exit when not SK.Bit_Test
           (Value => SK.Word64 (Status),
            Pos   => PS2.Constants.OUTPUT_BUFFER_STATUS);

         SK.IO.Inb (Port  => PS2.Constants.DATA_REGISTER,
                    Value => Data);

         if SK.Bit_Test
           (Value => SK.Word64 (Status),
            Pos   => PS2.Constants.AUX_DATA)
         then
            PS2.Mouse.Process (Data => Data);
         else
            PS2.Keyboard.Process (Data => Data);
         end if;
      end loop;
   end Handle_Interrupt;
begin
   PS2.Output.Init;
   Interrupts.Initialize;
   PS2.Mouse.Init;

   Log.Text_IO.Put_Line (Item => "PS/2 driver initialized");

   loop
      SK.CPU.Sti;
      SK.CPU.Hlt;
      Handle_Interrupt;
   end loop;
end PS2_Drv;
