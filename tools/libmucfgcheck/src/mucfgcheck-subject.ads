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

package Mucfgcheck.Subject
is

   --D @Section Id => validation-subj, Label => Subjects, Parent => validation
   --D @Text Section => validation-subj
   --D The following checks are performed to verify the correctness of the
   --D subject configuration in the system policy.
   --D @UL Id => validators_subj, Section => validation-subj

   --D @Item List => validators_subj
   --D Validate subject name uniqueness.
   procedure Name_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate subject CPU ID.
   procedure CPU_ID (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate uniqueness of global subject IDs.
   procedure Global_ID_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate per-CPU uniqueness of local subject IDs.
   procedure Local_ID_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate memory types of memory mappings (ie. allow access by subjects).
   procedure Memory_Types (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that no subject references an IOMMU device.
   procedure No_IOMMU_Device_References (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that all subjects are runnable, i.e. directly referenced in
   --D the scheduling plan or target of a switch event of a subject that is
   --D itself scheduled.
   procedure Runnability (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that subject scheduling group IDs match values as determined by
   --D the scheduling plan and handover events.
   procedure Scheduling_Group_IDs (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that logical names of subject devices are unique.
   procedure Logical_Device_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that IRQ vector numbers of PCI device references with MSI
   --D enabled are consecutive.
   procedure Logical_IRQ_MSI_Consecutiveness (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that logical names of subject unmask IRQ events conform to the
   --D naming scheme (\verb"unmask_irq_$IRQNR") and that the unmask number matches
   --D the physical IRQ.
   procedure Logical_Unmask_Event (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that no virtual memory regions of a subject overlap.
   procedure Virtual_Memory_Overlap (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that multiple initramfs regions are consecutive.
   procedure Initramfs_Consecutiveness (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that no subject has write access to the crash audit region.
   procedure Crash_Audit_Write_Access (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that subject device mmconf mappings are correct.
   procedure Device_Mmconf_Mappings (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that shared PCI devices specify the same PCI element.
   procedure Shared_Device_Same_PCI_Element (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that the VMX controls conform to the checks specified in Intel
   --D SDM Vol. 3C, "26.2.1 Checks on VMX Controls".
   procedure VMX_Controls_Entry_Checks (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that the Pin-Based VM-Execution controls meet the requirements
   --D for the execution of Muen.
   procedure VMX_Controls_Pin_Requirements (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that the Processor-Based VM-Execution Controls meet the
   --D requirements for the execution of Muen.
   procedure VMX_Controls_Proc_Requirements (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that the secondary Processor-Based VM-Execution Controls meet
   --D the requirements for the execution of Muen.
   procedure VMX_Controls_Proc2_Requirements (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that the VM-Exit Controls meet the requirements for the
   --D execution of Muen.
   procedure VM_Exit_Controls_Requirements (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that the VM-Entry Controls meet the requirements for the
   --D execution of Muen.
   procedure VM_Entry_Controls_Requirements (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that the VMX CR0 guest/host masks meet the requirements for the
   --D execution of Muen.
   procedure VMX_CR0_Mask_Requirements (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that the VMX CR4 guest/host masks meet the requirements for the
   --D execution of Muen.
   procedure VMX_CR4_Mask_Requirements (XML_Data : Muxml.XML_Data_Type);

   --D @Item List => validators_subj
   --D Validate that the VMX Exception bitmap meet the requirements for the
   --D execution of Muen.
   procedure VMX_Exception_Bitmap_Requirements
     (XML_Data : Muxml.XML_Data_Type);

end Mucfgcheck.Subject;
