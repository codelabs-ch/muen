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

package PS2.Constants
is

   DATA_REGISTER    : constant := 16#60#;
   STATUS_REGISTER  : constant := 16#64#;
   COMMAND_REGISTER : constant := 16#64#;
   ACKNOWLEDGE      : constant := 16#fa#;
   WRITE_TO_AUX     : constant := 16#d4#;

   OUTPUT_BUFFER_STATUS : constant := 0;
   INPUT_BUFFER_STATUS  : constant := 1;
   AUX_DATA             : constant := 5;

   --  Mouse commands, see http://wiki.osdev.org/Mouse_Input.
   CMD_RESET            : constant := 16#ff#;
   CMD_SET_DEFAULTS     : constant := 16#f6#;
   CMD_ENABLE_STREAMING : constant := 16#f4#;
   CMD_AUX_ENABLE       : constant := 16#a8#;
   CMD_READ_CONFIG      : constant := 16#20#;
   CMD_WRITE_CONFIG     : constant := 16#60#;
   ENABLE_IRQ12         : constant := 1;
   DISABLE_MOUSE_CLOCK  : constant := 5;

end PS2.Constants;
