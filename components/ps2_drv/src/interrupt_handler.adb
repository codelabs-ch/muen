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

with SK.Strings;

with Log;
with PS2;

with Ps2_Drv_Component.Devices;

package body Interrupt_Handler
is

   -------------------------------------------------------------------------

   procedure Handle_Interrupt (Context : SK.Exceptions.Isr_Context_Type)
   is
      use type SK.Byte;

      Vector : constant SK.Byte := SK.Byte'Mod (Context.Vector);
   begin
      if Vector = Ps2_Drv_Component.Devices.Ps2_Kbd_Irq
        or else Vector = Ps2_Drv_Component.Devices.Ps2_Mouse_Irq
      then
         PS2.Handle_Interrupt;
      else
         Log.Text_IO.Put_Line
           (Item => "Ignoring spurious interrupt " & SK.Strings.Img (Vector));
      end if;
   end Handle_Interrupt;

end Interrupt_Handler;
