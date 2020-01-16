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

with Interfaces;

with Dbg.Byte_Arrays;

package Dbg.Byte_Queue
is

   --  Byte queue.
   type Queue_Type is limited private;

   --  Initialize byte queue.
   procedure Initialize (Queue : out Queue_Type);

   --  Return used bytes in given queue.
   function Bytes_Used (Queue : Queue_Type) return Natural;

   --  Return free bytes in given queue.
   function Bytes_Free (Queue : Queue_Type) return Natural;

   --  Append bytes stored in byte array to given queue.
   procedure Append
      (Queue  : in out Queue_Type;
       Buffer :        Byte_Arrays.Byte_Array;
       Length :        Positive);

   --  Read bytes from given queue. The Length parameter designates the actual
   --  bytes copied. The bytes read remain in the queue until they are
   --  discarded using the Drop_Bytes procedure.
   procedure Peek
      (Queue  :     Queue_Type;
       Buffer : out Byte_Arrays.Byte_Array;
       Length : out Positive);

   --  Drop Length bytes from queue.
   procedure Drop_Bytes
      (Queue  : in out Queue_Type;
       Length :        Positive);

private

   type Buffer_Range is mod 2 ** 12;
   type Buffer_Type is array (Buffer_Range) of Interfaces.Unsigned_8;

   type Queue_Type is record
      Buffer : Buffer_Type;
      WP     : Buffer_Range;
      RP     : Buffer_Range;
   end record;

end Dbg.Byte_Queue;
