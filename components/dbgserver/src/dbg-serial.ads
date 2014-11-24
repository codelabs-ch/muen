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

with Dbg.Byte_Queue;

private package Dbg.Serial
is

   --  Init serial line.
   procedure Init;

   --  Read bytes from the serial line into the input queue and output bytes in
   --  the output queue to the serial line.
   procedure Run
      (Input_Queue  : in out Byte_Queue.Queue_Type;
       Output_Queue : in out Byte_Queue.Queue_Type);

end Dbg.Serial;
