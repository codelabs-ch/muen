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

package Backtraces
is

   type Call_Trace_Type is array (Natural range <>) of Interfaces.Unsigned_64;

   --  Create backtrace of System V ABI 64-bit call stack with given RBP value.
   procedure Analyse_Stack
     (RIP        :     Interfaces.Unsigned_64;
      RBP        :     Interfaces.Unsigned_64;
      Traces     : out Call_Trace_Type;
      Last_Index : out Natural)
   with
      Pre  => Traces'Length > 0,
      Post => Last_Index in Traces'Range;

end Backtraces;
