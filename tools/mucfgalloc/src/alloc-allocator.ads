--
--  Copyright (C) 2014  Alexander Senier <mail@senier.net>
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

with Muxml;

package Alloc.Allocator
is
   --  Write fully allocated configuration to given output directory.
   procedure Write
     (Output_File : String;
      Policy      : Muxml.XML_Data_Type);

   --  Physical memory regions in the config overlapped each other.
   Overlapping_Physical_Memory : exception;

   --  Internal error (e.g. malformed XML input)
   Internal_Error : exception;

end Alloc.Allocator;
