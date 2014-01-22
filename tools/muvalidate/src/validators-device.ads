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

with Muxml;

package Validators.Device
is

   --  Validate that devices referenced by logical devices exists.
   procedure Physical_Device_References (XML_Data : Muxml.XML_Data_Type);

   --  Validate that all physical IRQs are unique.
   procedure Physical_IRQ_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --  Validate that physical device IRQs referenced by logical IRQs exists.
   procedure Physical_IRQ_References (XML_Data : Muxml.XML_Data_Type);

   --  Validate that logical and physical device IRQ numbers are equal.
   procedure IRQ_Number_Equality (XML_Data : Muxml.XML_Data_Type);

   --  Validate that all IO start ports are smaller than end ports.
   procedure IO_Port_Start_Smaller_End (XML_Data : Muxml.XML_Data_Type);

   --  Validate that physical I/O ports referenced by logical I/O ports exists.
   procedure IO_Port_References (XML_Data : Muxml.XML_Data_Type);

   --  Validate that logical and physical I/O port ranges are equal.
   procedure IO_Port_Range_Equality (XML_Data : Muxml.XML_Data_Type);

end Validators.Device;
