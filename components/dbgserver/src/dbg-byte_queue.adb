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

package body Dbg.Byte_Queue
is

   -------------------------------------------------------------------------

   procedure Append
     (Queue  : in out Queue_Type;
      Buffer :        Byte_Arrays.Byte_Array;
      Length :        Positive)
   is
   begin
      for I in Natural range Buffer'First .. Buffer'First + (Length - 1) loop
         Queue.Buffer (Queue.WP) := Buffer (I);
         Queue.WP := Queue.WP + 1;
      end loop;
   end Append;

   -------------------------------------------------------------------------

   function Bytes_Free (Queue : Queue_Type) return Natural
   is
   begin
      return (Queue.Buffer'Length - Bytes_Used (Queue)) - 1;
   end Bytes_Free;

   -------------------------------------------------------------------------

   function Bytes_Used (Queue : Queue_Type) return Natural
   is
   begin
      return Natural (Queue.WP - Queue.RP);
   end Bytes_Used;

   -------------------------------------------------------------------------

   procedure Drop_Bytes
     (Queue  : in out Queue_Type;
      Length :        Positive)
   is
   begin
      for I in Positive range 1 .. Length loop
         Queue.Buffer (Queue.RP) := 0;
         Queue.RP := Queue.RP + 1;
      end loop;
   end Drop_Bytes;

   -------------------------------------------------------------------------

   procedure Initialize (Queue : out Queue_Type)
   is
   begin
      Queue := Queue_Type'
        (Buffer => Buffer_Type'(Buffer_Range => 0),
         WP     => 0,
         RP     => 0);
   end Initialize;

   -------------------------------------------------------------------------

   procedure Peek
     (Queue  :     Queue_Type;
      Buffer : out Byte_Arrays.Byte_Array;
      Length : out Positive)
   is
      RP           : Buffer_Range;
      Bytes_Copied : Natural;
   begin
      RP           := Queue.RP;
      Bytes_Copied := 0;

      for I in Natural range Buffer'Range loop
         Buffer (I)   := Queue.Buffer (RP);
         Bytes_Copied := Bytes_Copied + 1;
         RP := RP + 1;

         if RP = Queue.WP then
            for J in Natural range I + 1 .. Buffer'Last loop
               Buffer (J) := 0;
            end loop;
            exit;
         end if;
      end loop;
      Length := Bytes_Copied;
   end Peek;

end Dbg.Byte_Queue;
