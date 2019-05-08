--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Dbg.Buffers;
with Dbg.Byte_Queue;

private package Dbg.Channels
with SPARK_Mode => Off
is

   type Channel_Type is record
      Buffer : Buffers.Buffer_Type;
      Input  : Byte_Queue.Queue_Type;
      Output : Byte_Queue.Queue_Type;
   end record;

   type Debug_Interfaces_Type
      is (INTERFACE_XHCDBG,
          INTERFACE_SERIAL,
          INTERFACE_PCSPKR,
          INTERFACE_SHMEM);

   type Channels_Type is array (Debug_Interfaces_Type) of Channel_Type;

   Instance : Channels_Type;

end Dbg.Channels;
