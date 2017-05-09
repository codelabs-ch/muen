--
--  Copyright (C) 2016  secunet Security Networks AG
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

with HW.DbC;

with Dbg.Byte_Arrays;

package body Dbg.Xhci_Dbg
is

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      HW.DbC.Init;
   end Init;

   -------------------------------------------------------------------------

   procedure Run
     (Input_Queue  : in out Byte_Queue.Queue_Type;
      Output_Queue : in out Byte_Queue.Queue_Type)
   is
      subtype Data_Range is Positive range 1 .. 4096;
      subtype Data_Array is Byte_Arrays.Byte_Array (Data_Range);

      Data    : Data_Array;
      Length  : Natural;
      Success : Boolean;
   begin
      while Byte_Queue.Bytes_Free (Queue => Input_Queue) > 0 loop
         Length := Natural'Min
           (Byte_Queue.Bytes_Free (Queue => Input_Queue), Data'Length);
         HW.DbC.Receive (HW.Buffer (Data), Length);
         exit when Length = 0;
         Byte_Queue.Append
           (Queue  => Input_Queue,
            Buffer => Data,
            Length => Length);
      end loop;

      Success := True;
      while Byte_Queue.Bytes_Used (Queue => Output_Queue) > 0 loop
         Byte_Queue.Peek (Queue  => Output_Queue,
                          Buffer => Data,
                          Length => Length);

         if Length > 0 then
            HW.DbC.Send (HW.Buffer (Data), Length, Success);
            Byte_Queue.Drop_Bytes (Queue  => Output_Queue,
                                   Length => Length);
         end if;
         exit when not Success;
      end loop;
      HW.DbC.Poll;
   end Run;

end Dbg.Xhci_Dbg;
