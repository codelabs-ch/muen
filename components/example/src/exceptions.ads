--
--  Copyright (C) 2022  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2022  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

package Exceptions
is

   --  Trigger breakpoint exception (vector number 3).
   procedure Trigger_Breakpoint;

   --  Print backtrace of System V ABI 64-bit call stack with given RBP value.
   procedure Print_Backtrace
     (RIP : Interfaces.Unsigned_64;
      RBP : Interfaces.Unsigned_64);

   BP_Triggered : Boolean
   with Volatile;

end Exceptions;
