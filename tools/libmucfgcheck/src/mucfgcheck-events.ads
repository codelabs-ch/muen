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
with Mutools.Types;

package Mucfgcheck.Events
is

   --  Check that all physical event names are unique.
   procedure Physical_Event_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --  Check that each global event has associated sources and one target.
   procedure Source_Targets (XML_Data : Muxml.XML_Data_Type);

   --  Check subject event references.
   procedure Subject_Event_References (XML_Data : Muxml.XML_Data_Type);

   --  Validate that there are no self-references in subject's event
   --  notification entries.
   procedure Self_References (XML_Data : Muxml.XML_Data_Type);

   --  Validate that notification entries switch to a subject running on the
   --  same core.
   procedure Switch_Same_Core (XML_Data : Muxml.XML_Data_Type);

   --  Validate that target subjects of IPI notification entries run on
   --  different logical CPUs.
   procedure IPI_Different_Core (XML_Data : Muxml.XML_Data_Type);

   --  Validate that source event IDs are unique per group.
   procedure Source_Group_Event_ID_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --  Check source event ID validity.
   procedure Source_Group_Event_ID_Validity (XML_Data : Muxml.XML_Data_Type);

   --  Check that self events provide a target action.
   procedure Self_Event_Action (XML_Data : Muxml.XML_Data_Type);

   --  Check that kernel-mode events have an action specified.
   procedure Kernel_Mode_Event_Actions (XML_Data : Muxml.XML_Data_Type);

   --  Check that system-related actions are only used with kernel-mode events.
   procedure Kernel_Mode_System_Actions (XML_Data : Muxml.XML_Data_Type);

   --  Returns the maximum valid ID for a given event group.
   function Get_Max_ID (Group : Mutools.Types.Event_Group_Type) return Natural;

   --  Returns True if the specified ID is valid in the context of the given
   --  event group.
   function Is_Valid_Event_ID
     (Group : Mutools.Types.Event_Group_Type;
      ID    : Natural)
      return Boolean;

end Mucfgcheck.Events;
