--
--  Copyright (C) 2013-2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp.Hardware;

with SK.IO;

package body SK.Console_Serial
is

   --  Serial output base address.
   Base_Address : constant := Skp.Hardware.Debugconsole_Port;
   --  Baud rate: 115200
   Divisor      : constant := 1;

   UART_IER : constant := 1;
   UART_IIR : constant := 2;
   UART_LCR : constant := 3;
   UART_MCR : constant := 4;
   UART_LSR : constant := 5;

   --  Return True if the send buffer is empty.
   function Empty_Send_Buffer return Boolean;

   -------------------------------------------------------------------------

   function Empty_Send_Buffer return Boolean
   is
      Data : Byte;
   begin
      IO.Inb (Port  => Base_Address + UART_LSR,
              Value => Data);
      return (Data and 16#20#) /= 0;
   end Empty_Send_Buffer;

   -------------------------------------------------------------------------

   procedure Init
   is
   begin

      --  Disable interrupts.

      IO.Outb (Port  => Base_Address + UART_IER,
               Value => 0);

      --  Enable DLAB.

      IO.Outb (Port  => Base_Address + UART_LCR,
               Value => 16#80#);

      --  Set divisor (least/most significant byte).

      IO.Outb (Port  => Base_Address,
               Value => Divisor);
      IO.Outb (Port  => Base_Address + UART_IER,
               Value => 0);

      --  Clear DLAB and set 8 bits, no parity, one stop bit (8N1).

      IO.Outb (Port  => Base_Address + UART_LCR,
               Value => 3);

      --  Enable FIFO.

      IO.Outb (Port  => Base_Address + UART_IIR,
               Value => 16#c7#);

      --  IRQS enabled, RTS/DSR set.

      IO.Outb (Port  => Base_Address + UART_MCR,
               Value => 16#0b#);
   end Init;

   -------------------------------------------------------------------------

   procedure New_Line
   is
   begin
      Put_Char (Item => ASCII.CR);
      Put_Char (Item => ASCII.LF);
   end New_Line;

   -------------------------------------------------------------------------

   procedure Put_Char (Item : Character)
   is
   begin
      while not Empty_Send_Buffer loop
         null;
      end loop;

      IO.Outb (Port  => Base_Address,
               Value => Character'Pos (Item));
   end Put_Char;

end SK.Console_Serial;
