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

package Dbg.Byte_Queue.Format
is

   --  Append Character to queue.
   procedure Append_Character
     (Queue : in out Queue_Type;
      Item  :        Character);

   --  Append string to queue.
   procedure Append_String
     (Queue : in out Queue_Type;
      Item  :        String);

   --  Append string and new line to queue.
   procedure Append_Line
     (Queue : in out Queue_Type;
      Item  :        String);

   --  Append boolean to queue.
   procedure Append_Bool
     (Queue : in out Queue_Type;
      Item  :        Boolean);

   --  Append boolean to queue (T (F, resp.) instead of True (False, resp.)).
   procedure Append_Bool_Short
     (Queue : in out Queue_Type;
      Item  :        Boolean);

   --  Appends natural number to queue. If Left_Align is False, then the number
   --  is right aligned.
   procedure Append_Natural
     (Queue      : in out Queue_Type;
      Item       :        Natural;
      Left_Align :        Boolean := True);

   --  Append new line to queue.
   procedure Append_New_Line (Queue : in out Queue_Type);

   --  Append current state of byte queue specified by Item to given queue.
   procedure Append_State
     (Queue : in out Queue_Type;
      Item  :        Queue_Type);

end Dbg.Byte_Queue.Format;
