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

with Log;
with Driver_Keyboard;
with Mux.Terminals;

package body Interrupt_Handler
is

   -------------------------------------------------------------------------

   procedure Handle_Interrupt (Vector : SK.Byte)
   is
      use type SK.Byte;
   begin
      if Vector >= 34 and then Vector <= 36 then
         Mux.Terminals.Queue_Request;
      elsif Vector = 49 then
         Driver_Keyboard.Handle;
      else
         Log.Text_IO.Put_String (Item => "Ignoring spurious interrupt ");
         Log.Text_IO.Put_Byte   (Item => Vector);
         Log.Text_IO.New_Line;
      end if;
   end Handle_Interrupt;

end Interrupt_Handler;
