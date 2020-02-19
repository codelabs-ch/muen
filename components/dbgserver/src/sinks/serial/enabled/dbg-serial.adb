--
--  Copyright (C) 2014  secunet Security Networks AG
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

with SK.UART_8250;

with Dbg.Byte_Arrays;
with Dbg.Non_Interfering_Output;

with Dbgserver_Component.Devices;

package body Dbg.Serial
is

   --  Receive data up until Length bytes from UART into given buffer. The
   --  actual number of bytes received is return by Length.
   procedure Receive_Buffer
     (Buffer : in out Byte_Arrays.Byte_Array;
      Length : in out Byte_Arrays.Byte_Array_Range);

   --  Send data in buffer up until Length bytes to UART. The actual number of
   --  bytes sent is returned by Length.
   procedure Send_Buffer
     (Buffer  :        Byte_Arrays.Byte_Array;
      Length  : in out Byte_Arrays.Byte_Array_Range;
      Success :    out Boolean);

   package UART is new SK.UART_8250
     (Base_Address => Dbgserver_Component.Devices.Debugconsole_Port_Start);

   package NIO is new Non_Interfering_Output
     (Buffer_Size => UART.FIFO_Size,
      Receive     => Receive_Buffer,
      Send        => Send_Buffer);

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      UART.Init;
   end Init;

   -------------------------------------------------------------------------

   procedure Receive_Buffer
     (Buffer : in out Byte_Arrays.Byte_Array;
      Length : in out Byte_Arrays.Byte_Array_Range)
   is
   begin
      for I in Buffer'First .. Length loop
         if not UART.Is_Data_Available then
            Length := I - 1;
            exit;
         end if;
         Buffer (I) := Character'Pos (UART.Read_Char);
      end loop;
   end Receive_Buffer;

   -------------------------------------------------------------------------

   procedure Send_Buffer
     (Buffer  :        Byte_Arrays.Byte_Array;
      Length  : in out Byte_Arrays.Byte_Array_Range;
      Success :    out Boolean)
   is
   begin
      for I in Buffer'First .. Length loop
         UART.Put_Char (Item => Character'Val (Buffer (I)));
      end loop;
      Success := True;
   end Send_Buffer;

   ---------------------------------------------------------------------------

   procedure Run
     (Console      : in out Consoles.Console_Type;
      Input_Queue  : in out Byte_Queue.Queue_Type;
      Output_Queue : in out Byte_Queue.Queue_Type)
      renames NIO.Run;

end Dbg.Serial;
