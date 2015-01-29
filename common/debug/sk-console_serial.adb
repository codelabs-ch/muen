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

   --  Serial output address.
   Port    : constant := Skp.Hardware.Debugconsole_Port;
   --  Baud rate: 115200
   Divisor : constant := 1;

   --  Return True if the send buffer is empty.
   function Empty_Send_Buffer return Boolean;

   -------------------------------------------------------------------------

   function Empty_Send_Buffer return Boolean
   is
      Data : Byte;
   begin
      IO.Inb (Port  => Port + 5,
              Value => Data);
      return (Data and 16#20#) /= 0;
   end Empty_Send_Buffer;

   -------------------------------------------------------------------------

   procedure Init
   is
   begin

      --  Disable interrupts.

      IO.Outb (Port  => Port + 1,
               Value => 0);

      --  Enable DLAB.

      IO.Outb (Port  => Port + 3,
               Value => 16#80#);

      --  Set divisor (least/most significant byte).

      IO.Outb (Port  => Port,
               Value => Divisor);
      IO.Outb (Port  => Port + 1,
               Value => 0);

      --  Clear DLAB and set 8 bits, no parity, one stop bit (8N1).

      IO.Outb (Port  => Port + 3,
               Value => 3);

      --  Enable FIFO.

      IO.Outb (Port  => Port + 2,
               Value => 16#c7#);

      --  IRQS enabled, RTS/DSR set.

      IO.Outb (Port  => Port + 4,
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

      IO.Outb (Port  => Port,
               Value => Character'Pos (Item));
   end Put_Char;

end SK.Console_Serial;
