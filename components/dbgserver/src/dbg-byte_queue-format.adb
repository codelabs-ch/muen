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

with Debuglog.Utils;

with Dbg.Byte_Arrays;

package body Dbg.Byte_Queue.Format
is

   -------------------------------------------------------------------------

   procedure Append_Character
     (Queue : in out Queue_Type;
      Item  :        Character)
   is
      Single_Byte : Byte_Arrays.Single_Byte_Array;
   begin
      Single_Byte := Byte_Arrays.Single_Byte_Array'
        (1 => Interfaces.Unsigned_8 (Character'Pos (Item)));
      Append (Queue  => Queue,
              Buffer => Single_Byte,
              Length => Single_Byte'Length);
   end Append_Character;

   -------------------------------------------------------------------------

   procedure Append_New_Line (Queue : in out Queue_Type)
   is
   begin
      Append_Character (Queue => Queue,
                        Item  => ASCII.CR);
      Append_Character (Queue => Queue,
                        Item  => ASCII.LF);
   end Append_New_Line;

   -------------------------------------------------------------------------

   procedure Append_Number
     (Queue : in out Queue_Type;
      Item  :        Interfaces.Unsigned_64;
      Len   :        Positive)
   is
      use type Interfaces.Unsigned_64;

      subtype Image_Range is Natural range 1 .. 16;
      subtype Image_Type is String (Image_Range);

      Image : Image_Type := Image_Type'(Image_Range => '_');
      Temp  : Interfaces.Unsigned_64;
   begin
      Temp := Item;
      for I in reverse Image_Range range
        Image_Range'First .. (Image_Range'First + Len) - 1
      loop
         Image (I) := Debuglog.Utils.Num_To_Char
           (Value => Debuglog.Utils.Nibble_Type (Temp mod 16));
         Temp := Temp / 16;
      end loop;

      Append_String (Queue  => Queue,
                     Buffer => Image,
                     Length => Len);
   end Append_Number;

   -------------------------------------------------------------------------

   procedure Append_String
     (Queue : in out Queue_Type;
      Item  :        String)
   is
   begin
      Append_String (Queue  => Queue,
                     Buffer => Item,
                     Length => Item'Length);
   end Append_String;

end Dbg.Byte_Queue.Format;
