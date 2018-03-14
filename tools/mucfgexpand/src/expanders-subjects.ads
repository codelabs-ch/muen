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

package Expanders.Subjects
is

   --  Add subject profile-specific XML elements. Removes the profile attribute
   --  after processing.
   procedure Handle_Profile (Data : in out Muxml.XML_Data_Type);

   --  Add tau0 subject.
   procedure Add_Tau0 (Data : in out Muxml.XML_Data_Type);

   --  Add simple subject mappings to subject monitors.
   procedure Handle_Monitors (Data : in out Muxml.XML_Data_Type);

   --  Add subject memory mappings to subject loaders.
   procedure Handle_Loaders (Data : in out Muxml.XML_Data_Type);

   --  Add global subject IDs.
   procedure Add_Global_IDs (Data : in out Muxml.XML_Data_Type);

   --  Add local subject IDs.
   procedure Add_Local_IDs (Data : in out Muxml.XML_Data_Type);

   --  Add missing subject XML elements.
   procedure Add_Missing_Elements (Data : in out Muxml.XML_Data_Type);

   --  Add memory mappings for channels.
   procedure Add_Channel_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Add channel events to vmcall groups.
   procedure Add_Channel_Events (Data : in out Muxml.XML_Data_Type);

   --  Remove subject channel elements.
   procedure Remove_Channel_Elements (Data : in out Muxml.XML_Data_Type);

   --  Add default events. Removes the default element after processing.
   procedure Add_Default_Events (Data : in out Muxml.XML_Data_Type);

   --  Add CPU IDs.
   procedure Add_CPU_IDs (Data : in out Muxml.XML_Data_Type);

   --  Add identity-mappings for device memory (if not explicitly specified by
   --  the user).
   procedure Add_Device_Memory_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Add BDF definitions for PCI devices (if not explicitly specified by the
   --  user).
   procedure Add_Device_BDFs (Data : in out Muxml.XML_Data_Type);

   --  Add physical device resources if no explicit resource references are
   --  specified.
   procedure Add_Device_Resources (Data : in out Muxml.XML_Data_Type);

   --  Add logical device IRQ for each MSI.
   procedure Add_Device_MSIs (Data : in out Muxml.XML_Data_Type);

   --  Allocate vectors of logical IRQs (if not explicitly specified by the
   --  user).
   procedure Add_Device_Vectors (Data : in out Muxml.XML_Data_Type);

   --  Add sinfo region to all subjects.
   procedure Add_Sinfo_Regions (Data : in out Muxml.XML_Data_Type);

   --  Add scheduling group info mappings.
   procedure Add_Sched_Group_Info_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Add subject timed event mappings.
   procedure Add_Timed_Event_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Allocate target event IDs.
   procedure Add_Target_Event_IDs  (Data : in out Muxml.XML_Data_Type);

   --  Expand mugenschedcfg auto idle subjects (if found).
   procedure Add_Mugensched_Idle_Subjects (Data : in out Muxml.XML_Data_Type);

   --  Remove subject monitor elements.
   procedure Remove_Monitors (Data : in out Muxml.XML_Data_Type);

   --  Remove subject device irq msi elements.
   procedure Remove_Device_MSIs (Data : in out Muxml.XML_Data_Type);

end Expanders.Subjects;
