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

with Dbgserver_Component.Devices;

package body Dbg.Serial
is

   package UART is new SK.UART_8250
     (Base_Address => Dbgserver_Component.Devices.Debugconsole_Port_Start);

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      UART.Init;
   end Init;

   -------------------------------------------------------------------------

   procedure Run
      (Input_Queue  : in out Byte_Queue.Queue_Type;
       Output_Queue : in out Byte_Queue.Queue_Type)
   is
      subtype Data_Range is Positive range 1 .. UART.FIFO_Size;
      subtype Data_Array is Byte_Arrays.Byte_Array (Data_Range);

      Data   : Data_Array := (others => 0);
      Length : Natural;
   begin
      while Byte_Queue.Bytes_Free (Queue => Input_Queue) > 0 loop
         exit when not UART.Is_Data_Available;

         Data (1) := Character'Pos (UART.Read_Char);
         Byte_Queue.Append
           (Queue  => Input_Queue,
            Buffer => Data,
            Length => 1);
      end loop;

      while Byte_Queue.Bytes_Used (Queue => Output_Queue) > 0 loop
         exit when not UART.Is_Send_Buffer_Empty;

         Byte_Queue.Peek (Queue  => Output_Queue,
                          Buffer => Data,
                          Length => Length);

         if Length > 0 then
            for I in Data_Range'First .. Length loop
               UART.Put_Char (Item => Character'Val (Data (I)));
            end loop;
            Byte_Queue.Drop_Bytes (Queue  => Output_Queue,
                                   Length => Length);
         end if;
      end loop;
   end Run;

end Dbg.Serial;
