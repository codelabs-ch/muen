--
--  Copyright (C) 2013-2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp;

package SK.CPU_Info
is

   use type Skp.CPU_Range;

   --  ID of the local CPU.
   CPU_ID : constant Skp.CPU_Range
   with
      Import,
      Size       => 8,
      Convention => C,
      Link_Name  => "cpu_id";

   --  Returns True if the local CPU is the bootstrap processor.
   function Is_BSP return Boolean
   with
      Post => Is_BSP'Result = (CPU_ID = Skp.CPU_Range'First);

end SK.CPU_Info;
