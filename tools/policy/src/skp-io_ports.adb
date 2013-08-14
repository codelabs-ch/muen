--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Skp.IO_Ports
is

   -------------------------------------------------------------------------

   procedure Allow_Ports
     (B          : in out IO_Bitmap_Type;
      Start_Port :        SK.Word16;
      End_Port   :        SK.Word16)
   is
   begin
      for Port in SK.Word16 range Start_Port .. End_Port loop
         B (Port) := Allowed;
      end loop;
   end Allow_Ports;

   -------------------------------------------------------------------------

   function To_Stream
     (B : IO_Bitmap_Type)
      return IO_Bitmap_Stream
   is
      function Convert is new Ada.Unchecked_Conversion
        (Source => IO_Bitmap_Type,
         Target => IO_Bitmap_Stream);
   begin
      return Convert (S => B);
   end To_Stream;

end Skp.IO_Ports;
