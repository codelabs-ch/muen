--
--  Copyright (C) 2020  secunet Security Networks AG
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

package body Dbg.Non_Interfering_Output
is

   package BA renames Dbg.Byte_Arrays;

   subtype Buffer_Range is BA.Byte_Array_Range range 1 .. Buffer_Size;
   subtype Buffer_Array is BA.Byte_Array (Buffer_Range);

   --  Read available input and append it to the given queue.
   procedure Process_Input (Queue : in out Byte_Queue.Queue_Type);

   -------------------------------------------------------------------------

   procedure Process_Input (Queue : in out Byte_Queue.Queue_Type)
   is
      Buffer : Buffer_Array;
      Length : Buffer_Range;
   begin
      while Byte_Queue.Bytes_Free (Queue => Queue) > 0 loop
         Length := BA.Byte_Array_Range'Min
           (Byte_Queue.Bytes_Free (Queue => Queue), Buffer'Length);
         Receive (Buffer => Buffer,
                  Length => Length);

         exit when Length = 0;
         Byte_Queue.Append
           (Queue  => Queue,
            Buffer => Buffer,
            Length => Length);
      end loop;
   end Process_Input;

   -------------------------------------------------------------------------

   procedure Run_Queue
     (Queue  : in out Byte_Queue.Queue_Type;
      Result :    out Run_Queue_Result_Type)
   is
      Buffer  : Buffer_Array;
      Length  : Buffer_Range;
      Success : Boolean;
   begin
      Result := Null_Run_Queue_Result;

      while Byte_Queue.Bytes_Used (Queue => Queue) > 0 loop
         Byte_Queue.Peek (Queue  => Queue,
                          Buffer => Buffer,
                          Length => Length);

         if Length > 0 then
            Send (Buffer  => Buffer,
                  Length  => Length,
                  Success => Success);
            if not Success then
               Result.Exit_Reason := Send_Buffer_Full;
               exit;
            end if;

            Result.Data_Sent := True;
            Byte_Queue.Drop_Bytes (Queue  => Queue,
                                   Length => Length);
         end if;
      end loop;
   end Run_Queue;

   -------------------------------------------------------------------------

   procedure Run
     (Console      : in out Consoles.Console_Type;
      Input_Queue  : in out Byte_Queue.Queue_Type;
      Output_Queue : in out Byte_Queue.Queue_Type)
   is
      Run_Queue_Result : Run_Queue_Result_Type;
   begin
      Process_Input (Queue => Input_Queue);

      if Run_State = Sending_Output_Queue then
         Run_Queue (Queue  => Output_Queue,
                    Result => Run_Queue_Result);
         if Run_Queue_Result.Exit_Reason = Queue_Empty then

            --  All output consumed, switch to console queue.

            Run_State := Sending_Command_Queue;
         end if;
      end if;

      if Run_State = Sending_Command_Queue then
         Run_Queue (Queue  => Console.Output_Queue,
                    Result => Run_Queue_Result);
         if Run_Queue_Result.Exit_Reason = Queue_Empty then

            --  All console output consumed, switch to output queue.

            Run_State := Sending_Output_Queue;
         end if;
      end if;
   end Run;

end Dbg.Non_Interfering_Output;
