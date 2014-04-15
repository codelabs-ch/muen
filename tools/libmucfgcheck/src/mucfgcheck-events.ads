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

package Mucfgcheck.Events
is

   --  Check that each global event has associated sources and one target.
   procedure Source_Targets (XML_Data : Muxml.XML_Data_Type);

   --  Check subject event references.
   procedure Subject_Event_References (XML_Data : Muxml.XML_Data_Type);

   --  Validate subject references in event table notification entries.
   procedure Subject_References (XML_Data : Muxml.XML_Data_Type);

   --  Validate that there are no self-references in subject's event
   --  notification entries.
   procedure Self_References (XML_Data : Muxml.XML_Data_Type);

   --  Validate that notification entries switch to a subject running on the
   --  same core.
   procedure Switch_Same_Core (XML_Data : Muxml.XML_Data_Type);

   --  Validate that target subjects of IPI notification entries run on
   --  different logical CPUs.
   procedure IPI_Different_Core (XML_Data : Muxml.XML_Data_Type);

end Mucfgcheck.Events;
