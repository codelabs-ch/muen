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

package Expanders.Memory
is

   --  Add kernel shared memory elements for text, ro and globaldata sections.
   procedure Add_Kernel_Shared_Memory (Data : in out Muxml.XML_Data_Type);

   --  Add kernel CPU-local memory elements for data and bss sections.
   procedure Add_Kernel_CPU_Local_Memory (Data : in out Muxml.XML_Data_Type);

   --  Add kernel pagetable memory elements.
   procedure Add_Kernel_PTs (Data : in out Muxml.XML_Data_Type);

   --  Add subject pagetable memory elements.
   procedure Add_Subject_PTs (Data : in out Muxml.XML_Data_Type);

   --  Add kernel stack memory elements.
   procedure Add_Kernel_Stack (Data : in out Muxml.XML_Data_Type);

   --  Add subject state memory elements.
   procedure Add_Subject_States (Data : in out Muxml.XML_Data_Type);

   --  Add subject timed event page memory elements.
   procedure Add_Subject_Timed_Event_Pages (Data : in out Muxml.XML_Data_Type);

   --  Add subject interrupts page memory elements.
   procedure Add_Subject_Interrupts_Pages (Data : in out Muxml.XML_Data_Type);

   --  Add subject FPU state memory regions.
   procedure Add_Subject_FPU_State_Regions (Data : in out Muxml.XML_Data_Type);

   --  Add tau0 interface memory element.
   procedure Add_Tau0_Interface (Data : in out Muxml.XML_Data_Type);

   --  Add memory region for AP trampoline.
   procedure Add_AP_Trampoline (Data : in out Muxml.XML_Data_Type);

   --  Add VMXON regions.
   procedure Add_VMXON_Regions (Data : in out Muxml.XML_Data_Type);

   --  Add VMCS regions.
   procedure Add_VMCS_Regions (Data : in out Muxml.XML_Data_Type);

   --  Add alignment/type attribute with default value where missing.
   procedure Add_Missing_Attributes (Data : in out Muxml.XML_Data_Type);

   --  Add subject I/O and MSR bitmap memory regions.
   procedure Add_Subject_Bitmaps (Data : in out Muxml.XML_Data_Type);

   --  Add subject MSR store memory regions.
   procedure Add_Subject_MSR_Store (Data : in out Muxml.XML_Data_Type);

   --  Add physical memory region for each reserved memory region.
   procedure Add_Reserved_Memory_Regions (Data : in out Muxml.XML_Data_Type);

   --  Add physical scheduling info memory region for each scheduling partition.
   procedure Add_Scheduling_Info_Regions (Data : in out Muxml.XML_Data_Type);

end Expanders.Memory;
