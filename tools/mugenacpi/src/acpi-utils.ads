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

with Ada.Strings.Unbounded;

with Interfaces;

with Mutools.Utils;

package Acpi.Utils
is

   function Indent
     (N         : Positive := 1;
      Unit_Size : Positive := 4)
      return String renames Mutools.Utils.Indent;

   --  Append device IRQ resource with given parameters to specified
   --  string buffer.
   procedure Add_Dev_IRQ_Resource
     (Buffer  : in out Ada.Strings.Unbounded.Unbounded_String;
      Bus_Nr  :        Interfaces.Unsigned_64;
      Dev_Nr  :        Interfaces.Unsigned_64;
      Irq_Nr  :        Interfaces.Unsigned_64;
      Int_Pin :        Natural);

end Acpi.Utils;
