--
--  Copyright (C) 2018  secunet Security Networks AG
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

with Dbg.Byte_Arrays;

package Dbg.Shared_Memory.Types
is

   subtype Data_Index is Natural range 1 .. 64;

   subtype Data_Type is Byte_Arrays.Byte_Array (Data_Index);

   Null_Data : constant Data_Type := (others => 0);

end Dbg.Shared_Memory.Types;
