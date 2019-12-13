--
--  Copyright (C) 2014, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

   --  Returns a DWord Memory Resource Descriptor string with the given
   --  parameters, see ACPI Specification 6.1, section 19.6.34.
   function DWordMemory
     (Base_Address : Interfaces.Unsigned_32;
      Size         : Interfaces.Unsigned_32;
      Cacheable    : Boolean)
      return String;

   --  Returns a QWord Memory Resource Descriptor string with the given
   --  parameters, see ACPI Specification 6.1, section 19.6.104.
   function QWordMemory
     (Base_Address : Interfaces.Unsigned_64;
      Size         : Interfaces.Unsigned_64;
      Cacheable    : Boolean)
      return String;

   --  Returns an IO Resource Descriptor string with the given parameters, see
   --  ACPI Specification 6.1, section 19.6.63.
   function IO
     (Start_Port : Interfaces.Unsigned_16;
      Port_Range : Interfaces.Unsigned_16)
      return String;

   --  Returns an IRQ Resource Descriptor string for an active-high,
   --  edge-triggered IRQ with the given number, see ACPI Specification 6.1,
   --  section 19.6.65.
   function IRQNoFlags (Number : Interfaces.Unsigned_8) return String;

end Acpi.Asl;
