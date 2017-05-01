--
--  Copyright (C) 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Strings;

with Log;

with PS2.Keyboard;
with PS2.Mouse;
with PS2.I8042;

package body PS2
is

   -------------------------------------------------------------------------

   procedure Handle_Interrupt
   is
      Status, Data : SK.Byte;
   begin
      I8042.Read_Status (Status => Status);
      if I8042.Has_Pending_Data (Status => Status) then
         I8042.Read_Data (Data => Data);
         if I8042.Is_Keyboard_Data (Status => Status) then
            Log.Text_IO.Put_Line (Item => "KBD data: "
                                  & SK.Strings.Img (Data));
            Keyboard.Process (Data => Data);
         else
            Log.Text_IO.Put_Line (Item => "AUX data: "
                                  & SK.Strings.Img (Data));
            Mouse.Process (Data => Data);
         end if;
      end if;
   end Handle_Interrupt;

end PS2;
