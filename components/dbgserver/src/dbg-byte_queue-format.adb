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

package body Dbg.Byte_Queue.Format
is

   -------------------------------------------------------------------------

   procedure Append_Bool
     (Queue : in out Queue_Type;
      Item  :        Boolean)
   is
   begin
      if Item then
         Append_String
           (Queue => Queue,
            Item  => "True");
      else
         Append_String
           (Queue => Queue,
            Item  => "False");
      end if;
   end Append_Bool;

   -------------------------------------------------------------------------

   procedure Append_Character
     (Queue : in out Queue_Type;
      Item  :        Character)
   is
   begin
      Append (Queue => Queue,
              Byte  => Character'Pos (Item));
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

   procedure Append_String
     (Queue : in out Queue_Type;
      Item  :        String)
   is
   begin
      for C of Item loop
         Append_Character (Queue => Queue,
                           Item  => C);
      end loop;
   end Append_String;

end Dbg.Byte_Queue.Format;
