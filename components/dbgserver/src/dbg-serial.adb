--
--  Copyright (C) 2014  secunet Security Networks AG
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

with SK.IO;

with Skp.Hardware;

with Dbg.Byte_Arrays;

package body Dbg.Serial
is

   use type SK.Word16;

   Serial_Port : constant SK.Word16 := Skp.Hardware.Debugconsole_Port;
   FCR_Offset  : constant SK.Word16 := 16#2#;
   LSR_Offset  : constant SK.Word16 := 16#5#;

   UART_Data_Ready              : constant SK.Byte := 16#01#;
   UART_THR_Empty_And_Line_Idle : constant SK.Byte := 16#20#;
   UART_FIFO_Enable             : constant SK.Byte := 16#01#;

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      SK.IO.Outb (Port  => Serial_Port + FCR_Offset,
                  Value => UART_FIFO_Enable);
   end Init;

   -------------------------------------------------------------------------

   procedure Run
      (Input_Queue  : in out Byte_Queue.Queue_Type;
       Output_Queue : in out Byte_Queue.Queue_Type)
   is
      use type SK.Byte;

      LSR         : SK.Byte;
      Length      : Natural;
      Byte_Buffer : Byte_Arrays.Single_Byte_Array := (1 => 0);
   begin
      while Byte_Queue.Bytes_Free (Queue => Input_Queue) > 0 loop
         SK.IO.Inb (Port  => Serial_Port + LSR_Offset,
                    Value => LSR);

         exit when (LSR and UART_Data_Ready) /= UART_Data_Ready;

         SK.IO.Inb (Port  => Serial_Port,
                    Value => SK.Byte (Byte_Buffer (1)));

         Byte_Queue.Append (Queue  => Input_Queue,
                            Buffer => Byte_Buffer,
                            Length => 1);
      end loop;

      while Byte_Queue.Bytes_Used (Queue => Output_Queue) > 0 loop
         SK.IO.Inb (Port  => Serial_Port + LSR_Offset,
                    Value => LSR);

         exit when (LSR and UART_THR_Empty_And_Line_Idle) /=
           UART_THR_Empty_And_Line_Idle;

         Byte_Queue.Peek (Queue  => Output_Queue,
                          Buffer => Byte_Buffer,
                          Length => Length);

         if Length = 1 then
            SK.IO.Outb (Port  => Serial_Port,
                        Value => SK.Byte (Byte_Buffer (1)));
            Byte_Queue.Drop_Bytes (Queue  => Output_Queue,
                                   Length => Length);
         end if;
      end loop;
   end Run;

end Dbg.Serial;
