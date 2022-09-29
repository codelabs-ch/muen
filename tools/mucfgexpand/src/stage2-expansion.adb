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

with Expanders.Events;
with Expanders.Memory;
with Expanders.Kernel;
with Expanders.Subjects;
with Expanders.Channels;
with Expanders.Hardware;
with Expanders.Scheduling;
with Expanders.Device_Domains;

package body Stage2.Expansion
is

   -------------------------------------------------------------------------

   procedure Clear
   is
   begin
      Procs.Clear;
   end Clear;

   -------------------------------------------------------------------------

   function Get_Count return Natural renames Procs.Get_Count;

   -------------------------------------------------------------------------

   procedure Register_All
   is
      use Expanders;
   begin
      Procs.Register (Process => Hardware.Add_PCI_Device_MSI_IRQs'Access);

      --  Remove channel elements after stage 2 checks passed.

      Procs.Register (Process => Subjects.Remove_Channel_Elements'Access);
      Procs.Register (Process => Channels.Remove_Global_Channels'Access);

      Procs.Register (Process => Scheduling.Add_Partition_IDs'Access);
      Procs.Register (Process => Scheduling.Add_Group_IDs'Access);

      --  Add tau0 and potential Mugenschedcfg idle subjects prior to
      --  subject-related memory expanders (state, VMCS and bitmaps).

      Procs.Register (Process => Subjects.Add_Tau0'Access);
      Procs.Register (Process => Subjects.Add_Mugensched_Idle_Subjects'Access);

      --  Set subject ids and cpu prior to kernel subject state mapping
      --  expander.

      Procs.Register (Process => Subjects.Add_Global_IDs'Access);
      Procs.Register (Process => Subjects.Add_CPU_IDs'Access);

      --  Set local subject IDs after CPU ID expander.

      Procs.Register (Process => Subjects.Add_Local_IDs'Access);

      Procs.Register (Process => Memory.Add_Missing_Attributes'Access);
      Procs.Register (Process => Memory.Add_Kernel_Shared_Memory'Access);
      Procs.Register (Process => Memory.Add_Kernel_CPU_Local_Memory'Access);
      Procs.Register (Process => Memory.Add_Kernel_Stack'Access);
      Procs.Register (Process => Memory.Add_Subject_States'Access);
      Procs.Register (Process => Memory.Add_Subject_Timed_Event_Pages'Access);
      Procs.Register (Process => Memory.Add_Subject_Interrupts_Pages'Access);
      Procs.Register (Process => Memory.Add_Subject_FPU_State_Regions'Access);
      Procs.Register (Process => Memory.Add_Tau0_Interface'Access);
      Procs.Register (Process => Memory.Add_AP_Trampoline'Access);
      Procs.Register (Process => Memory.Add_VMXON_Regions'Access);
      Procs.Register (Process => Memory.Add_VMCS_Regions'Access);
      Procs.Register (Process => Memory.Add_Subject_Bitmaps'Access);
      Procs.Register
        (Process => Memory.Add_Scheduling_Group_Info_Regions'Access);
      Procs.Register (Process => Kernel.Add_Section_Skeleton'Access);
      Procs.Register (Process => Kernel.Add_Binary_Mappings'Access);
      Procs.Register (Process => Kernel.Add_Subj_State_Mappings'Access);
      Procs.Register (Process => Kernel.Add_Subj_Timed_Event_Mappings'Access);
      Procs.Register (Process => Kernel.Add_Subj_Interrupts_Mappings'Access);
      Procs.Register (Process => Kernel.Add_Subj_VMCS_Mappings'Access);
      Procs.Register (Process => Kernel.Add_Subj_FPU_State_Mappings'Access);
      Procs.Register (Process => Kernel.Add_Sched_Group_Info_Mappings'Access);
      Procs.Register (Process => Kernel.Add_Crash_Audit_Mappings'Access);
      Procs.Register (Process => Kernel.Map_Tau0_Interface'Access);
      Procs.Register (Process => Kernel.Add_Devices'Access);

      Procs.Register (Process => Subjects.Add_Sinfo_Regions'Access);

      Procs.Register (Process => Subjects.Handle_Monitors'Access);
      Procs.Register
        (Process => Subjects.Add_Sched_Group_Info_Mappings'Access);
      Procs.Register (Process => Subjects.Add_Timed_Event_Mappings'Access);
      Procs.Register (Process => Subjects.Add_Default_Events'Access);
      Procs.Register (Process => Subjects.Add_Device_BDFs'Access);

      --  Logical PCI element must exist before adding other resources because
      --  of virtual mmconf mapping derived from it.

      Procs.Register (Process => Subjects.Add_Device_Resources'Access);
      Procs.Register (Process => Subjects.Add_Device_Memory_Mappings'Access);
      Procs.Register (Process => Subjects.Add_Device_MSIs'Access);

      --  IRQ resources must be expanded to add vectors to IRQs.

      Procs.Register (Process => Subjects.Add_Device_Vectors'Access);

      --  Logical vectors must be expanded to add Unmask IRQ events.

      Procs.Register (Process => Subjects.Add_Unmask_IRQ_Events'Access);

      --  Handle profile removes profile info.

      Procs.Register (Process => Subjects.Handle_Profile'Access);

      --  SMP Linux profile handler adds target events.

      Procs.Register (Process => Subjects.Add_Target_Event_IDs'Access);

      --  Sibling memory processing must occur after all origin subject memory
      --  is present.

      Procs.Register (Process => Subjects.Add_Sibling_Memory'Access);

      --  Handle 'asap' events after channel and sibling expansion.

      Procs.Register (Process => Events.Handle_Asap_Events'Access);

      --  Handle loader adjusts the vcpu section which is added by
      --  Handle_Profile.

      Procs.Register (Process => Subjects.Handle_Loaders'Access);
      Procs.Register (Process => Subjects.Remove_Monitors'Access);
      Procs.Register (Process => Subjects.Remove_Device_MSIs'Access);

      Procs.Register (Process => Scheduling.Add_Barrier_Configs'Access);

      --  Subject profiles must be expanded since they may add MSR registers.

      Procs.Register (Process => Memory.Add_Subject_MSR_Store'Access);
      Procs.Register (Process => Kernel.Add_Subj_MSR_Store_Mappings'Access);

      --  All kernel/subject memory regions and mappings must exist and specify
      --  an alignment to add PTs.

      Procs.Register (Process => Memory.Add_Kernel_PTs'Access);
      Procs.Register (Process => Memory.Add_Subject_PTs'Access);

      --  Format A and B mandate a device domain section (even if IOMMU is not
      --  active).

      Procs.Register (Process => Device_Domains.Add_Section_Skeleton'Access);

      Procs.Register (Process => Hardware.Add_IOMMU_Default_Caps'Access);
      Procs.Register
        (Process => Device_Domains.Add_Reserved_Memory_Region_Mappings'Access);

      --  RMRR mappings must be added before the VT-d tables.

      Procs.Register (Process => Device_Domains.Add_Tables'Access);

      --  Device domains are allowed in a configuration where the IOMMU is
      --  disabled. This can be useful to quickly perform tests without
      --  IOMMU interference.

      Procs.Register (Process => Device_Domains.Add_Domain_IDs'Access);
      Procs.Register
        (Process => Device_Domains.Remove_Map_Reserved_Mem_Attribute'Access);

      Procs.Register
        (Process => Hardware.Remove_Reserved_Mem_References'Access);
   end Register_All;

   -------------------------------------------------------------------------

   procedure Run (Data : in out Muxml.XML_Data_Type) renames Procs.Run;

end Stage2.Expansion;
