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

package body Skp.Writers.ACPI is

   use type SK.Byte;

   function Checksum (Table : Table_T) return SK.Byte
   is
      function FL_Checksum is
         new Fixed_Length_Checksum (Table_T'Size / 8, Table_T);
   begin
      return FL_Checksum (Table);
   end Checksum;

   function Fixed_Length_Checksum (Table : Table_T) return SK.Byte
   is
      type Table_Bytes_T is array (1 .. Table_T'Size / 8) of SK.Byte;
      function Table_Bytes is
         new Ada.Unchecked_Conversion (Table_T, Table_Bytes_T);

      Sum : SK.Byte := 16#00#;
      BA  : constant Table_Bytes_T := Table_Bytes (Table);
   begin
      for I in 1 .. Length loop
         Sum := Sum + BA (I);
      end loop;
      return 16#00# - Sum;
   end Fixed_Length_Checksum;

   -------------------------------------------------------------------------

   generic
      type Idx is range <>;
      type ID_T is array (Idx) of SK.Byte;
   function To_ID_x (Str : String) return ID_T;

   function To_ID_4 (Str : String) return ID_4 is
      function To_ID is new To_ID_x (Range_4, ID_4);
   begin
      return To_ID (Str);
   end To_ID_4;

   function To_ID_6 (Str : String) return ID_6 is
      function To_ID is new To_ID_x (Range_6, ID_6);
   begin
      return To_ID (Str);
   end To_ID_6;

   function To_ID_8 (Str : String) return ID_8 is
      function To_ID is new To_ID_x (Range_8, ID_8);
   begin
      return To_ID (Str);
   end To_ID_8;

   function To_ID_x (Str : String) return ID_T is
      ID : ID_T;
      Copy_Last : constant Idx := Idx
        (Integer'Min (Integer (ID'Last),
                      Integer (ID'First) + Str'Length - 1));
   begin
      for I in Idx range ID'First .. Copy_Last loop
         ID (I) := Character'Pos
           (Str (Integer (I) - Integer (ID'First) + Str'First));
      end loop;
      if Copy_Last < ID'Last then
         for I in Idx range Copy_Last + 1 .. ID'Last loop
            ID (I) := Character'Pos (' ');
         end loop;
      end if;
      return ID;
   end To_ID_x;

end Skp.Writers.ACPI;
