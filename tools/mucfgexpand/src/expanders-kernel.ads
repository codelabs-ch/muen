--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Expanders.Kernel
is

   --  Add kernel section skeleton.
   procedure Add_Section_Skeleton (Data : in out Muxml.XML_Data_Type);

   --  Add kernel memory mappings (binary, stack and store).
   procedure Add_Binary_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Add subject VMCS memory mappings.
   procedure Add_Subj_VMCS_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Add subject state memory mappings.
   procedure Add_Subj_State_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Add subject timed event page mappings.
   procedure Add_Subj_Timed_Event_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Add subject interrupts page mappings.
   procedure Add_Subj_Interrupts_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Add subject MSR-store mappings.
   procedure Add_Subj_MSR_Store_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Add subject FPU state region mappings.
   procedure Add_Subj_FPU_State_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Add scheduling group info page mappings.
   procedure Add_Sched_Group_Info_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Map tau0 interface memory region on BSP.
   procedure Map_Tau0_Interface (Data : in out Muxml.XML_Data_Type);

   --  Add devices to kernel section.
   procedure Add_Devices (Data : in out Muxml.XML_Data_Type);

   --  Add crash audit region mappings.
   procedure Add_Crash_Audit_Mappings (Data : in out Muxml.XML_Data_Type);

end Expanders.Kernel;
