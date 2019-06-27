--
--  Copyright (C) 2019  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2019  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Cmd_Stream.XML_Utils;

package Cmd_Stream.Memblocks
is

   --  Generate command stream to create physical memory blocks of given system
   --  policy.
   procedure Create_Memory_Blocks
     (Policy     : in out Muxml.XML_Data_Type;
      Stream_Doc : in out XML_Utils.Stream_Document_Type);

end Cmd_Stream.Memblocks;
