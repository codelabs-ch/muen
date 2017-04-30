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

   --  I/O register addresses
   DATA_REGISTER        : constant := 16#60#;
   STATUS_REGISTER      : constant := 16#64#;
   COMMAND_REGISTER     : constant := 16#64#;

   --  Controller return values
   TEST_OK              : constant := 16#55#;
   TEST_OK_KBD          : constant := 16#00#;
   TEST_OK_AUX          : constant := 16#00#;
   TEST_PASSED          : constant := 16#aa#;
   ACKNOWLEDGE          : constant := 16#fa#;
   RESET_MOUSE_ID       : constant := 16#00#;

   --  Status register bits
   OUTPUT_BUFFER_STATUS : constant := 0;
   INPUT_BUFFER_STATUS  : constant := 1;
   AUX_DATA             : constant := 5;

   --  Controller configuration bits
   IRQ_KBD              : constant := 0;
   IRQ_AUX              : constant := 1;
   DISABLE_CLOCK_KBD    : constant := 4;
   DISABLE_CLOCK_AUX    : constant := 5;

   --  i8042 commands
   CMD_READ_CONFIG      : constant := 16#20#;
   CMD_WRITE_CONFIG     : constant := 16#60#;
   CMD_DISABLE_AUX      : constant := 16#a7#;
   CMD_ENABLE_AUX       : constant := 16#a8#;
   CMD_TEST_AUX         : constant := 16#a9#;
   CMD_TEST             : constant := 16#aa#;
   CMD_DISABLE_KBD      : constant := 16#ad#;
   CMD_ENABLE_KBD       : constant := 16#ae#;
   CMD_TEST_KBD         : constant := 16#ab#;
   CMD_WRITE_AUX        : constant := 16#d4#;
   CMD_RESET            : constant := 16#ff#;
   CMD_SET_DEFAULTS     : constant := 16#f6#;
   CMD_ENABLE_STREAMING : constant := 16#f4#;
   CMD_SET_SAMPLE_RATE  : constant := 16#f3#;
   CMD_GET_ID           : constant := 16#f2#;

   DEFAULT_SAMPLE_RATE  : constant := 100;

end PS2.Constants;
