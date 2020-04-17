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

package Mucfgcheck.Memory
is

   --  Validate that a VMXON region exists for every specified kernel.
   procedure VMXON_Region_Presence (XML_Data : Muxml.XML_Data_Type);

   --  Validate size of VMXON regions.
   procedure VMXON_Region_Size (XML_Data : Muxml.XML_Data_Type);

   --  Validate that VMXON regions are in low-mem.
   procedure VMXON_In_Lowmem (XML_Data : Muxml.XML_Data_Type);

   --  Validate that all VMXON regions are consecutive.
   procedure VMXON_Consecutiveness (XML_Data : Muxml.XML_Data_Type);

   --  Validate that a VMCS region exists for each declared subject.
   procedure VMCS_Region_Presence (XML_Data : Muxml.XML_Data_Type);

   --  Validate size of VMCS regions.
   procedure VMCS_Region_Size (XML_Data : Muxml.XML_Data_Type);

   --  Validate that physical memory region names are unique.
   procedure Physical_Memory_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --  Validate that physical memory referenced by logical memory exists.
   procedure Physical_Memory_References (XML_Data : Muxml.XML_Data_Type);

   --  Validate that all physical memory addresses are page aligned.
   procedure Physical_Address_Alignment (XML_Data : Muxml.XML_Data_Type);

   --  Validate that all virtual memory addresses are page aligned.
   procedure Virtual_Address_Alignment (XML_Data : Muxml.XML_Data_Type);

   --  Validate that all memory region sizes are multiples of page size.
   procedure Region_Size (XML_Data : Muxml.XML_Data_Type);

   --  Validate kernel or subject entities encoded in physical memory names
   --  (e.g. 'linux|zp' or 'kernel_0|vmxon').
   procedure Entity_Name_Encoding (XML_Data : Muxml.XML_Data_Type);

   --  Validate that no physical memory regions overlap.
   procedure Physical_Memory_Overlap (XML_Data : Muxml.XML_Data_Type);

   --  Validate that an uncached crash audit region is present.
   procedure Uncached_Crash_Audit_Presence (XML_Data : Muxml.XML_Data_Type);

   --  Validate that crash audit region is located after system image.
   procedure Crash_Audit_After_Image (XML_Data : Muxml.XML_Data_Type);

   --  Validate that a kernel data region exists for every CPU.
   procedure Kernel_Data_Region_Presence (XML_Data : Muxml.XML_Data_Type);

   --  Validate that a kernel BSS region exists for every CPU.
   procedure Kernel_BSS_Region_Presence (XML_Data : Muxml.XML_Data_Type);

   --  Validate that a kernel stack region exists for every CPU.
   procedure Kernel_Stack_Region_Presence (XML_Data : Muxml.XML_Data_Type);

   --  Validate that a kernel interrupt stack region exists for every CPU.
   procedure Kernel_Intr_Stack_Region_Presence
     (XML_Data : Muxml.XML_Data_Type);

   --  Validate that a kernel PT region exists for every CPU.
   procedure Kernel_PT_Region_Presence (XML_Data : Muxml.XML_Data_Type);

   --  Validate that kernel PT regions are in the first 4G.
   procedure Kernel_PT_Below_4G (XML_Data : Muxml.XML_Data_Type);

   --  Validate that scheduling group info regions are mapped by the kernel
   --  running subjects of that scheduling group. Also verify that the kernel
   --  mapping is at the expected virtual location.
   procedure Kernel_Sched_Group_Info_Mappings (XML_Data : Muxml.XML_Data_Type);

   --  Validate that a subject state memory region with the expected size
   --  exists for every subject.
   procedure Subject_State_Region_Presence (XML_Data : Muxml.XML_Data_Type);

   --  Validate that a subject interrupts memory region with the expected size
   --  exists for every subject.
   procedure Subject_Interrupts_Region_Presence
     (XML_Data : Muxml.XML_Data_Type);

   --  Validate kernel memory mappings.
   procedure Kernel_Memory_Mappings (XML_Data : Muxml.XML_Data_Type);

   --  Validate system memory mappings.
   procedure System_Memory_Mappings (XML_Data : Muxml.XML_Data_Type);

   --  Validate memory mappings of type 'device' (e.g. device_rmrr).
   procedure Device_Memory_Mappings (XML_Data : Muxml.XML_Data_Type);

   --  Validate that subject state memory regions are mapped by the kernel
   --  running that subject. Also verify that the kernel mapping is at the
   --  expected virtual location.
   procedure Subject_State_Mappings (XML_Data : Muxml.XML_Data_Type);

   --  Validate that subject interrupts memory regions are mapped by the
   --  kernel running that subject. Also verify that the kernel mapping is at
   --  the expected virtual location.
   procedure Subject_Interrupts_Mappings (XML_Data : Muxml.XML_Data_Type);

   --  Validate that subject MSR store memory regions are mapped by the kernel
   --  running that subject. Also verify that the kernel mapping is at the
   --  expected virtual location.
   procedure Subject_MSR_Store_Mappings (XML_Data : Muxml.XML_Data_Type);

   --  Validate that subject timed event memory regions are mapped by the
   --  kernel running that subject. Also verify that the kernel mapping is at
   --  the expected virtual location.
   procedure Subject_Timed_Event_Mappings (XML_Data : Muxml.XML_Data_Type);

   --  Validate that subject VMCS regions are mapped by the kernel running that
   --  subject. Also verify that the kernel mapping is at the expected virtual
   --  location.
   procedure Subject_VMCS_Mappings (XML_Data : Muxml.XML_Data_Type);

   --  Validate that subject FPU state regions are mapped by the kernel running
   --  that subject. Also verify that the kernel mapping is at the expected
   --  virtual location.
   procedure Subject_FPU_State_Mappings (XML_Data : Muxml.XML_Data_Type);

   --  Validate that a subject FPU state memory region with the expected size
   --  exists for every subject.
   procedure Subject_FPU_State_Region_Presence
     (XML_Data : Muxml.XML_Data_Type);

   --  Validate that a subject timed event memory region with the expected size
   --  exists for every subject.
   procedure Subject_Timed_Event_Region_Presence
     (XML_Data : Muxml.XML_Data_Type);

   --  Validate that a subject I/O Bitmap region with the expected size exists
   --  for every subject.
   procedure Subject_IOBM_Region_Presence (XML_Data : Muxml.XML_Data_Type);

   --  Validate that a subject MSR Bitmap region  with the expected size exists
   --  for every subject.
   procedure Subject_MSRBM_Region_Presence (XML_Data : Muxml.XML_Data_Type);

   --  Validate that a subject MSR store memory region exists for each subject
   --  that accesses MSR registers not managed by VMCS.
   procedure Subject_MSR_Store_Region_Presence
     (XML_Data : Muxml.XML_Data_Type);

   --  Validate that a scheduling group info memory region exists for each
   --  scheduling group.
   procedure Scheduling_Group_Info_Region_Presence
     (XML_Data : Muxml.XML_Data_Type);

   --  Validate that subjects map the scheduling group info region of the
   --  scheduling group they belong to.
   procedure Subject_Sched_Group_Info_Mappings
     (XML_Data : Muxml.XML_Data_Type);

   --  Validate size of VT-d root table region.
   procedure VTd_Root_Region_Size (XML_Data : Muxml.XML_Data_Type);

   --  Validate size of VT-d context table region.
   procedure VTd_Context_Region_Size (XML_Data : Muxml.XML_Data_Type);

   --  Validate that a VT-d root table region exists if domains are present.
   procedure VTd_Root_Region_Presence (XML_Data : Muxml.XML_Data_Type);

   --  Validate that a VT-d interrupt remapping table region exists.
   procedure VTd_IRT_Region_Presence (XML_Data : Muxml.XML_Data_Type);

end Mucfgcheck.Memory;
