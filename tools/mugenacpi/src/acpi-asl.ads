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

package Acpi.Asl
is

   use type Interfaces.Unsigned_16;

   --  Returns a DWord Memory Resource Descriptor string with the given
   --  parameters, see ACPI Specification 5.0, Errata A, section 19.5.34.
   function DWordMemory
     (Base_Address : Interfaces.Unsigned_32;
      Size         : Interfaces.Unsigned_32;
      Cacheable    : Boolean)
      return String;

   --  Returns an IO Resource Descriptor string with the given parameters, see
   --  ACPI Specification 5.0, Errata A, section 19.5.62.
   function IO
     (Start_Port : Interfaces.Unsigned_16;
      Port_Range : Interfaces.Unsigned_16)
      return String;

end Acpi.Asl;
