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

package SK.UART
is

   --  Baud rate: 115200
   Divisor : constant := 1;

   --  Divisor Latch Low Byte
   UART_DLL : constant := 0;
   --  Divisor Latch High Byte
   UART_DLH : constant := 1;
   --  Interrupt Enable Register
   UART_IER : constant := 1;
   --  FIFO Control Register
   UART_FCR : constant := 2;
   --  Line Control Register
   UART_LCR : constant := 3;
   --  Modem Control Register
   UART_MCR : constant := 4;
   --  Line Status Register
   UART_LSR : constant := 5;

end SK.UART;
