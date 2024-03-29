--
--  Copyright (C) 2023  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2023  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Ucode
is

   --  Start processing microcode updates by reading the CPU signature from
   --  the system policy and inspect Ucode updates in given microcode
   --  directory. Write matching MCU to given output directory and add
   --  physical memory region to policy.
   procedure Run
     (Policy     : String;
      Ucode_Dir  : String;
      Output_Dir : String);

end Ucode;
