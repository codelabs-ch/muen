--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Types
is

   -------------------------------------------------------------------------

   function To_EPTV_Info (Qualification : SK.Word64) return EPTV_Info_Type
   is
      function To_EPTV_Information is new Ada.Unchecked_Conversion
        (Source => SK.Word64,
         Target => EPTV_Info_Type);
   begin
      return To_EPTV_Information (Qualification);
   end To_EPTV_Info;

   -------------------------------------------------------------------------

   function To_IO_Info (Qualification : SK.Word64) return IO_Info_Type
   is
      function To_IO_Information is new Ada.Unchecked_Conversion
        (Source => SK.Word64,
         Target => IO_Info_Type);
   begin
      return To_IO_Information (Qualification);
   end To_IO_Info;

end Types;
