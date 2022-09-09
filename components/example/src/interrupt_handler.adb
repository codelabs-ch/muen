--
--  Copyright (C) 2013-2022  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2022  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Interrupt_Handler
is

   -------------------------------------------------------------------------

   procedure Handle_Interrupt (Context : SK.Exceptions.Isr_Context_Type)
   is
   begin
      Log.Put_Line (Item => "Received vector " & SK.Strings.Img
                    (Item => SK.Byte'Mod (Context.Vector)));
   end Handle_Interrupt;

end Interrupt_Handler;
