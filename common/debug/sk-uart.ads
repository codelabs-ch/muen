--
--  Copyright (C) 2015-2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015-2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

   --  Defines UART register width.
   type Register_Type is mod <>;

   --  Defines the UART address type.
   type Address_Type is mod <>;

   --  UART base address.
   Base_Address : Address_Type;

   --  Baud rate (default 115200)
   Divisor  : Register_Type := 1;
   --  Divisor Latch Low Byte
   UART_DLL : Address_Type := 0;
   --  Divisor Latch High Byte
   UART_DLH : Address_Type := 1;

   --  Interrupt Enable Register
   UART_IER : Address_Type := 1;
   --  FIFO Control Register
   UART_FCR : Address_Type := 2;
   --  Line Control Register
   UART_LCR : Address_Type := 3;
   --  Modem Control Register
   UART_MCR : Address_Type := 4;
   --  Line Status Register
   UART_LSR : Address_Type := 5;

   --  Size of FIFO.
   FIFO_Size : Byte := 16;

   --  Read register at given address.
   with procedure Read
     (Address :     Address_Type;
      Value   : out Register_Type);

   --  Write Value to register at given address.
   with procedure Write
     (Address : Address_Type;
      Value   : Register_Type);

package SK.UART
is

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

end SK.UART;
