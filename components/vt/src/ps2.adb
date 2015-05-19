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

with SK.IO;

with PS2.Keyboard;

package body PS2
is

   --  PS/2 constants.

   Data_Port       : constant := 16#60#;
   Status_Register : constant := 16#64#;

   OUTPUT_BUFFER_STATUS : constant := 0;

   -------------------------------------------------------------------------

   procedure Handle_Interrupt
   is
      Status, Data : SK.Byte;
   begin
      loop
         SK.IO.Inb (Port  => Status_Register,
                    Value => Status);
         exit when not SK.Bit_Test
           (Value => SK.Word64 (Status),
            Pos   => OUTPUT_BUFFER_STATUS);

         SK.IO.Inb (Port  => Data_Port,
                    Value => Data);

         Keyboard.Process (Data => Data);
      end loop;
   end Handle_Interrupt;

end PS2;
