--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Unchecked_Conversion;

package body Msrstore.Tables
is

   -------------------------------------------------------------------------

   procedure Append_Entry
     (Store : in out MSR_Store_Type;
      Index :        Interfaces.Unsigned_32;
      Data  :        Interfaces.Unsigned_64)
   is
      Cur_Idx : constant MSR_Store_Size := MSR_Store_Size (Store.Next_Idx);
   begin
      Store.Data (Cur_Idx).Index := Index;
      Store.Data (Cur_Idx).Data  := Data;
      Store.Next_Idx             := Store.Next_Idx + 1;
   end Append_Entry;

   -------------------------------------------------------------------------

   function To_Stream
     (Store : MSR_Store_Type)
      return Ada.Streams.Stream_Element_Array
   is
      use Ada.Streams;

      Last_Idx    : constant MSR_Store_Size
        := MSR_Store_Size (Store.Next_Idx - 1);
      Stream_Size : constant Stream_Element_Offset
        := (Store_Entry_Type'Size / 8) * Stream_Element_Offset (Last_Idx);

      subtype Table_Type    is MSR_Table_Type       (1 .. Last_Idx);
      subtype Result_Stream is Stream_Element_Array (1 .. Stream_Size);

      function Convert is new Ada.Unchecked_Conversion
        (Source => Table_Type,
         Target => Result_Stream);
   begin
      return Convert (S => Store.Data (1 .. Last_Idx));
   end To_Stream;

end Msrstore.Tables;
