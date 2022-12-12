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
   --D @Section Id => validation-evts, Label => Events, Parent => validation
   --D @Text Section => validation-evts
   --D The following checks are performed to guarantee that events are
   --D correctly configured in the system policy.
   --D @UL Id => validators_evts, Section => validation-evts

   --D @Item List => validators_evts
   --D Check that all physical event names are unique.
   procedure Physical_Event_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_evts
   --D Check that each global event has associated sources and one target.
   procedure Source_Targets (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_evts
   --D Check subject event references.
   procedure Subject_Event_References (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_evts
   --D Validate that there are no self-references in subject's event
   --D notification entries.
   procedure Self_References (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_evts
   --D Validate that notification entries switch to a subject running on the
   --D same core and in the same scheduling group.
   procedure Switch_Same_Core (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_evts
   --D Validate that target subjects of IPI notification entries run on
   --D different logical CPUs.
   procedure IPI_Different_Core (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_evts
   --D Validate that target event IDs as well as logical names are unique.
   procedure Target_Event_ID_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_evts
   --D Validate that source event IDs as well as logical names are unique per
   --D group.
   procedure Source_Group_Event_ID_Name_Uniqueness
     (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_evts
   --D Check source event ID validity.
   procedure Source_Group_Event_ID_Validity (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_evts
   --D Check that source event IDs of the VMX Exit group are all given or a
   --D default is specified.
   procedure Source_VMX_Exit_Event_Completeness
     (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_evts
   --D Check that self events provide a target action.
   procedure Self_Event_Action (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_evts
   --D Check that kernel-mode events have an action specified.
   procedure Kernel_Mode_Event_Actions (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_evts
   --D Check that system-related actions are only used with kernel-mode events.
   procedure Kernel_Mode_System_Actions (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_evts
   --D Check that level-triggered IRQs have a corresponding unmask IRQ event.
   procedure Level_Triggered_Unmask_IRQ_Action
     (XML_Data : Muxml.XML_Data_Type);

end Mucfgcheck.Events;
