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

with Dbg.String_Utils;

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

   procedure Append_Bool_Short
     (Queue : in out Queue_Type;
      Item  :        Boolean)
   is
      Char : constant Character := (if Item then 'T' else 'F');
   begin
      Append_Character
        (Queue => Queue,
         Item  => Char);
   end Append_Bool_Short;

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

   procedure Append_Natural
     (Queue      : in out Queue_Type;
      Item       :        Natural;
      Left_Align :        Boolean := True)
   is
      Buffer : String (1 .. 10) := (others => ' ');
   begin
      String_Utils.Unsigned_Integer_To_String
        (Unsigned_Number => Item,
         Buffer          => Buffer,
         Left_Align      => Left_Align);
      Append_String (Queue => Queue,
                     Item  => Buffer);
   end Append_Natural;

   -------------------------------------------------------------------------

   procedure Append_Line
     (Queue : in out Queue_Type;
      Item  :        String)
   is
   begin
      Append_String (Queue => Queue,
                     Item  => Item);
      Append_New_Line (Queue => Queue);
   end Append_Line;

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

   procedure Append_State
     (Queue : in out Queue_Type;
      Item  :        Queue_Type)
   is
   begin
      Append_String (Queue => Queue,
                     Item  => " free: ");
      Append_Natural (Queue => Queue,
                      Item  => Bytes_Free (Queue => Item));
      Append_New_Line (Queue => Queue);

      Append_String (Queue => Queue,
                     Item  => " used: ");
      Append_Natural (Queue => Queue,
                      Item  => Bytes_Used (Queue => Item));
      Append_New_Line (Queue => Queue);
   end Append_State;

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
