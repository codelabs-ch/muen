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

package Mucfgcheck.Scheduling
is

   --  Validate that each major frame specifies the same number of CPUs.
   procedure CPU_Element_Count (XML_Data : Muxml.XML_Data_Type);

   --  Validate subject references.
   procedure Subject_References (XML_Data : Muxml.XML_Data_Type);

   --  Validate that subjects are scheduled on the correct logical CPU.
   procedure Subject_CPU_Affinity (XML_Data : Muxml.XML_Data_Type);

   --  Validate tick counts in major frame.
   procedure Major_Frame_Ticks (XML_Data : Muxml.XML_Data_Type);

   --  Validate that barrier sizes do not exceed the number of logical CPUs.
   procedure Barrier_Size (XML_Data : Muxml.XML_Data_Type);

   --  Validate that the barrier sizes and count of a major frame corresponds
   --  to the minor frame synchronization points.
   procedure Barrier_Count (XML_Data : Muxml.XML_Data_Type);

end Mucfgcheck.Scheduling;
