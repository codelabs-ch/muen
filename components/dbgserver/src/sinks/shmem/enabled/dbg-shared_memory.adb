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

with System;

with Dbg.Shared_Memory.Types;
with Dbg.Shared_Memory.Stream.Writer_Instance;

with Dbgserver_Component.Channels;

package body Dbg.Shared_Memory
is

   package Cspecs renames Dbgserver_Component.Channels;

   Channel : Stream.Channel_Type
   with
      Address => System'To_Address (Cspecs.Debug_Shm_Sink_Memory_Address),
      Size    => Cspecs.Debug_Shm_Sink_Memory_Size * 8,
      Async_Readers;

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      Stream.Writer_Instance.Initialize (Channel => Channel,
                                         Epoch   => 1);
   end Init;

   -------------------------------------------------------------------------

   procedure Run (Output_Queue : in out Byte_Queue.Queue_Type)
   is
      Byte_Data : Types.Data_Type;
      Length    : Natural;
   begin
      while Byte_Queue.Bytes_Used (Queue => Output_Queue) > 0 loop
         Byte_Queue.Peek (Queue  => Output_Queue,
                          Buffer => Byte_Data,
                          Length => Length);

         if Length > 0 then
            Stream.Writer_Instance.Write (Channel => Channel,
                                          Element => Byte_Data);
            Byte_Queue.Drop_Bytes (Queue  => Output_Queue,
                                   Length => Length);
         end if;
      end loop;
   end Run;

end Dbg.Shared_Memory;
