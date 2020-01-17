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

generic

   --  UART base address.
   Base_Address : SK.Word16;

package SK.UART_8250
is

   FIFO_Size : constant := 16;

   --  Initialize serial port.
   procedure Init;

   --  Write new line and linefeed.
   procedure New_Line;

   --  Write character. Blocks until the send buffer is ready to accept new
   --  data.
   procedure Put_Char (Item : Character);

   --  Read character. Use the Is_Data_Available getter function to check
   --  whether actual data is available, otherwise you might receive garbage.
   function Read_Char return Character;

   --  Return True if the send buffer (incl. FIFO) is empty.
   function Is_Send_Buffer_Empty return Boolean;

   --  Return True if data is available to be read.
   function Is_Data_Available return Boolean;

end SK.UART_8250;
