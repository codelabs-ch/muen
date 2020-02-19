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

with Dbg.Byte_Arrays;
with Dbg.Byte_Queue;
with Dbg.Consoles;

generic

   Buffer_Size : Byte_Arrays.Byte_Array_Range;

   with procedure Receive
     (Buffer : in out Byte_Arrays.Byte_Array;
      Length : in out Byte_Arrays.Byte_Array_Range);

   with procedure Send
     (Buffer  :        Byte_Arrays.Byte_Array;
      Length  : in out Byte_Arrays.Byte_Array_Range;
      Success :    out Boolean);

package Dbg.Non_Interfering_Output
is

   --  Run given queues and console. Available input is received and appended
   --  to the input queue while available output from the output queue and the
   --  console are sent out in a non-interfering way.
   procedure Run
     (Console      : in out Consoles.Console_Type;
      Input_Queue  : in out Byte_Queue.Queue_Type;
      Output_Queue : in out Byte_Queue.Queue_Type);

private

   type Run_State_Type is (Sending_Output_Queue, Sending_Command_Queue);

   Run_State : Run_State_Type := Sending_Output_Queue;

   type Exit_Reason_Type is (Send_Buffer_Full, Queue_Empty);

   type Run_Queue_Result_Type is record
      Exit_Reason : Exit_Reason_Type;
      Data_Sent   : Boolean;
   end record;

   Null_Run_Queue_Result : constant Run_Queue_Result_Type
     := (Exit_Reason => Queue_Empty,
         Data_Sent   => False);

   procedure Run_Queue
     (Queue  : in out Byte_Queue.Queue_Type;
      Result :    out Run_Queue_Result_Type);

end Dbg.Non_Interfering_Output;
