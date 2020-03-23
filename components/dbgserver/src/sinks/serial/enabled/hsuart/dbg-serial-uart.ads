--
--  Copyright (C) 2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.UART;
with SK.UART_HS;

with Dbgserver_Component.Devices;

private package Dbg.Serial.UART is new SK.UART
  (Base_Address  => Dbgserver_Component.Devices.Debugconsole_Mem_Address,
   Register_Type => SK.Word32,
   Address_Type  => SK.Word64,
   UART_DLL      => 0,
   UART_DLH      => 4,
   UART_IER      => 4,
   UART_FCR      => 8,
   UART_LCR      => 16#0c#,
   UART_MCR      => 16#10#,
   UART_LSR      => 16#14#,
   FIFO_Size     => FIFO_Size,
   Read          => SK.UART_HS.Read,
   Write         => SK.UART_HS.Write);
