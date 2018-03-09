--
--  Copyright (C) 2018  secunet Security Networks AG
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
with Dbg.PC_Speaker;

package body Dbg.PC_Speaker_Dbg
is

   -------------------------------------------------------------------------

   procedure Init is null;

   -------------------------------------------------------------------------

   procedure Run (Output_Queue : in out Byte_Queue.Queue_Type)
   is
      Data   : Byte_Arrays.Single_Byte_Array := (1 => 0);
      Length : Natural;
   begin
      if Byte_Queue.Bytes_Used (Queue => Output_Queue) > 0 then
         Byte_Queue.Peek (Queue  => Output_Queue,
                          Buffer => Data,
                          Length => Length);

         if Length = 1 then
            PC_Speaker.Put (Item => Data, Duration_MS => 10);

            Byte_Queue.Drop_Bytes (Queue => Output_Queue, Length => Length);
         end if;
      end if;
   end Run;

end Dbg.PC_Speaker_Dbg;
