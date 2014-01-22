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

with Ahven.Framework;

package Device_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Validate physical device references.
   procedure Validate_Physdev_Refs;

   --  Validate uniqueness of physical device IRQs.
   procedure Validate_Physirq_Uniqueness;

   --  Validate physical device IRQ references.
   procedure Validate_Physirq_Refs;

   --  Validate physical and logical IRQ equality.
   procedure Validate_IRQ_Number_Eq;

   --  Validate that IO start ports are smaller than end ports.
   procedure Validate_IO_Port_Start_Smaller_End;

   --  Validate physical device I/O port references.
   procedure Validate_IO_Port_Refs;

   --  Validate physical and logical I/O port range equality.
   procedure Validate_IO_Port_Range_Eq;

end Device_Tests;
