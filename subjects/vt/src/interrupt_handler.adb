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

with SK.IO;
with SK.Hypercall;

with Log;
with Terminals;

package body Interrupt_Handler
is

   type Kbd_Driver_Type is record
      Scancode : SK.Byte;
   end record;

   --  Xv6 driver page, currently used to forward keyboard scancodes.
   Kbd_Driver : Kbd_Driver_Type;
   for Kbd_Driver'Address use System'To_Address (16#40000#);
   pragma Volatile (Kbd_Driver);

   --  PS/2 constants.

   Data_Port       : constant := 16#60#;
   Status_Register : constant := 16#64#;

   OUTPUT_BUFFER_STATUS : constant := 0;

   -------------------------------------------------------------------------

   procedure Handle_Interrupt (Vector : SK.Byte)
   is
      use type SK.Byte;
      use type Terminals.Slot_Range;

      Status, Data : SK.Byte;
   begin
      if Vector /= 49 then
         Log.Text_IO.Put_String (Item => "Ignoring spurious interrupt ");
         Log.Text_IO.Put_Byte   (Item => Vector);
         Log.Text_IO.New_Line;
         return;
      end if;

      loop
         SK.IO.Inb (Port  => Status_Register,
                    Value => Status);
         exit when not SK.Bit_Test
           (Value => SK.Word64 (Status),
            Pos   => OUTPUT_BUFFER_STATUS);

         SK.IO.Inb (Port  => Data_Port,
                    Value => Data);

         case Data is
            when 1  =>
               Log.Text_IO.Init;
            when 59 =>
               Log.VGA.Enable_Cursor;
               Terminals.Set (Slot => 1);
               Log.Text_IO.Put_Line ("Switching to VT 1");
            when 60 =>
               Log.VGA.Disable_Cursor;
               Terminals.Set (Slot => 2);
               Log.Text_IO.Put_Line ("Switching to VT 2");
            when 63 =>
               Log.VGA.Disable_Cursor;
               Terminals.Set (Slot => 5);
               Log.Text_IO.Put_Line ("Switching to VT 5");
            when 64 =>
               Log.VGA.Disable_Cursor;
               Terminals.Set (Slot => 6);
               Log.Text_IO.Put_Line ("Switching to VT 6");
            when others =>
               if Terminals.Get_Active_Slot = 1 then
                  Kbd_Driver.Scancode := Data;
                  SK.Hypercall.Trigger_Event (Number => 1);
               end if;
         end case;
      end loop;
   end Handle_Interrupt;

end Interrupt_Handler;
