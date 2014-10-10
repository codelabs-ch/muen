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

with System;

with SK.IO;

with Subject.Console;
with Subject.Text_IO;

procedure Time
is
   --  Time page
   Counter : SK.Word64
     with
       Volatile,
       Address => System'To_Address (16#20000#);

   use type SK.Word64;
begin
   Subject.Console.Enable_Notification;
   Subject.Text_IO.Init;
   Subject.Text_IO.Put_Line (Item => "Time subject running");

   loop
      for J in 1 .. 998 loop
         SK.IO.Outb (Port  => 16#80#,
                     Value => 16#00#);
      end loop;
      Counter := Counter + 1;
   end loop;
end Time;
