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
with Mulog;
with Mucfgcheck.Config;
with Mucfgcheck.Memory;
with Mucfgcheck.MSR;
with Mucfgcheck.Device;
with Mucfgcheck.Scheduling;
with Mucfgcheck.Kernel;
with Mucfgcheck.Subject;
with Mucfgcheck.Hardware;
with Mucfgcheck.Platform;
with Mucfgcheck.Events;
with Mucfgcheck.Device_Domains;
with Mucfgcheck.Validation_Errors;

with Validate.XML_Processors;

package body Validate
is

   -------------------------------------------------------------------------

   procedure Register_All
   is
      use Mucfgcheck;
   begin

      --  Check references first, some of these are fatal if not correct.

      XML_Processors.Register
        (Process => Memory.Physical_Memory_References'Access);
      XML_Processors.Register
        (Process => Device.Physical_Device_References'Access);
      XML_Processors.Register
        (Process => Device.Legacy_Device_References'Access);
      XML_Processors.Register
        (Process => Device.PCI_Device_References'Access);
      XML_Processors.Register
        (Process => Device.Physical_IRQ_References'Access);
      XML_Processors.Register
        (Process => Device.IO_Port_References'Access);
      XML_Processors.Register
        (Process => Device.Device_Memory_References'Access);
      XML_Processors.Register
        (Process => Scheduling.Subject_References'Access);
      XML_Processors.Register
        (Process => Events.Subject_Event_References'Access);
      XML_Processors.Register
        (Process => Events.Subject_Event_References'Access);

      XML_Processors.Register
        (Process => Config.Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Memory.Physical_Memory_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Memory.VMXON_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.VMXON_Region_Size'Access);
      XML_Processors.Register
        (Process => Memory.VMXON_In_Lowmem'Access);
      XML_Processors.Register
        (Process => Memory.VMXON_Consecutiveness'Access);
      XML_Processors.Register
        (Process => Memory.VMCS_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.VMCS_Region_Size'Access);
      XML_Processors.Register
        (Process => Memory.Physical_Address_Alignment'Access);
      XML_Processors.Register
        (Process => Memory.Virtual_Address_Alignment'Access);
      XML_Processors.Register
        (Process => Memory.Region_Size'Access);
      XML_Processors.Register
        (Process => Memory.Entity_Name_Encoding'Access);
      XML_Processors.Register
        (Process => Memory.Physical_Memory_Overlap'Access);
      XML_Processors.Register
        (Process => Memory.Uncached_Crash_Audit_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Crash_Audit_After_Image'Access);
      XML_Processors.Register
        (Process => Kernel.Virtual_Memory_Overlap'Access);
      XML_Processors.Register
        (Process => Subject.Virtual_Memory_Overlap'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_Data_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_BSS_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_Stack_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_Intr_Stack_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_PT_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_PT_Below_4G'Access);
      XML_Processors.Register
        (Process => Memory.Subject_State_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Subject_Timed_Event_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Subject_Interrupts_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Subject_IOBM_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Subject_MSRBM_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Subject_MSR_Store_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Subject_FPU_State_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Subject_PT_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Scheduling_Group_Info_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_Memory_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.System_Memory_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Device_Memory_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Subject_State_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Subject_Interrupts_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Subject_Timed_Event_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Subject_VMCS_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Subject_MSR_Store_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Subject_FPU_State_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Subject_Sched_Group_Info_Mappings'Access);
      XML_Processors.Register
        (Process => Memory.Kernel_Sched_Group_Info_Mappings'Access);
      XML_Processors.Register
        (Process => MSR.Start_Smaller_End'Access);
      XML_Processors.Register
        (Process => MSR.Check_Whitelist'Access);
      XML_Processors.Register
        (Process => Device.Physical_Device_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.Physical_IRQ_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.Device_IRQ_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.Physical_IRQ_Constraints_ISA'Access);
      XML_Processors.Register
        (Process => Device.Physical_IRQ_Constraints_PCI_LSI'Access);
      XML_Processors.Register
        (Process => Device.Physical_IRQ_Constraints_PCI_MSI'Access);
      XML_Processors.Register
        (Process => Device.Physical_IRQ_MSI_Consecutiveness'Access);
      XML_Processors.Register
        (Process => Device.IO_Port_Start_Smaller_End'Access);
      XML_Processors.Register
        (Process => Device.Device_IO_Port_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.IO_Port_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.Device_Memory_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.Device_Reference_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.PCI_Device_BDF_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.PCI_Multifunction_Device_Refs'Access);
      XML_Processors.Register
        (Process => Device.Device_Reference_BDF_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device.Device_References_PCI_Bus_Number'Access);
      XML_Processors.Register
        (Process => Scheduling.CPU_Element_Count'Access);
      XML_Processors.Register
        (Process => Scheduling.Subject_CPU_Affinity'Access);
      XML_Processors.Register
        (Process => Scheduling.Major_Frame_Ticks'Access);
      XML_Processors.Register
        (Process => Scheduling.Barrier_ID'Access);
      XML_Processors.Register
        (Process => Scheduling.Barrier_Size'Access);
      XML_Processors.Register
        (Process => Scheduling.Minor_Frame_Sync_Points'Access);
      XML_Processors.Register
        (Process => Scheduling.Minor_Frame_Barrier_Refs'Access);
      XML_Processors.Register
        (Process => Kernel.CPU_Local_Data_Address_Equality'Access);
      XML_Processors.Register
        (Process => Kernel.CPU_Local_BSS_Address_Equality'Access);
      XML_Processors.Register
        (Process => Kernel.Global_Data_Address_Equality'Access);
      XML_Processors.Register
        (Process => Kernel.Crash_Audit_Address_Equality'Access);
      XML_Processors.Register
        (Process => Kernel.Stack_Address_Equality'Access);
      XML_Processors.Register
        (Process => Kernel.Stack_Layout'Access);
      XML_Processors.Register
        (Process => Kernel.CPU_Memory_Section_Count'Access);
      XML_Processors.Register
        (Process => Kernel.System_Board_Reference'Access);
      XML_Processors.Register
        (Process => Kernel.Diagnostics_Device_Reference'Access);
      XML_Processors.Register
        (Process => Subject.Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Subject.CPU_ID'Access);
      XML_Processors.Register
        (Process => Subject.Global_ID_Uniqueness'Access);
      XML_Processors.Register
        (Process => Subject.Local_ID_Uniqueness'Access);
      XML_Processors.Register
        (Process => Subject.Memory_Types'Access);
      XML_Processors.Register
        (Process => Subject.Runnability'Access);
      XML_Processors.Register
        (Process => Subject.Scheduling_Group_IDs'Access);
      XML_Processors.Register
        (Process => Subject.Logical_Device_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Subject.Logical_IRQ_MSI_Consecutiveness'Access);
      XML_Processors.Register
        (Process => Subject.Logical_Unmask_Event'Access);
      XML_Processors.Register
        (Process => Subject.Initramfs_Consecutiveness'Access);
      XML_Processors.Register
        (Process => Subject.Crash_Audit_Write_Access'Access);
      XML_Processors.Register
        (Process => Subject.Device_Mmconf_Mappings'Access);
      XML_Processors.Register
        (Process => Subject.Shared_Device_Same_PCI_Element'Access);
      XML_Processors.Register
        (Process => Subject.VMX_Controls_Entry_Checks'Access);
      XML_Processors.Register
        (Process => Subject.VMX_Controls_Pin_Requirements'Access);
      XML_Processors.Register
        (Process => Subject.VMX_Controls_Proc_Requirements'Access);
      XML_Processors.Register
        (Process => Subject.VMX_Controls_Proc2_Requirements'Access);
      XML_Processors.Register
        (Process => Subject.VM_Exit_Controls_Requirements'Access);
      XML_Processors.Register
        (Process => Subject.VM_Entry_Controls_Requirements'Access);
      XML_Processors.Register
        (Process => Subject.VMX_CR0_Mask_Requirements'Access);
      XML_Processors.Register
        (Process => Subject.VMX_CR4_Mask_Requirements'Access);
      XML_Processors.Register
        (Process => Subject.VMX_Exception_Bitmap_Requirements'Access);
      XML_Processors.Register
        (Process => Events.Physical_Event_Name_Uniqueness'Access);
      XML_Processors.Register
        (Process => Events.Source_Targets'Access);
      XML_Processors.Register
        (Process => Events.Self_References'Access);
      XML_Processors.Register
        (Process => Events.Switch_Same_Core'Access);
      XML_Processors.Register
        (Process => Events.IPI_Different_Core'Access);
      XML_Processors.Register
        (Process => Events.Target_Event_ID_Uniqueness'Access);
      XML_Processors.Register
        (Process => Events.Source_Group_Event_ID_Validity'Access);
      XML_Processors.Register
        (Process => Events.Source_Group_Event_ID_Uniqueness'Access);
      XML_Processors.Register
        (Process => Events.Source_VMX_Exit_Event_Completeness'Access);
      XML_Processors.Register
        (Process => Events.Self_Event_Action'Access);
      XML_Processors.Register
        (Process => Events.Kernel_Mode_Event_Actions'Access);
      XML_Processors.Register
        (Process => Events.Kernel_Mode_System_Actions'Access);
      XML_Processors.Register
        (Process => Events.Level_Triggered_Unmask_IRQ_Action'Access);
      XML_Processors.Register
        (Process => Hardware.Memory_Space'Access);
      XML_Processors.Register
        (Process => Hardware.Memory_Block_Overlap'Access);
      XML_Processors.Register
        (Process => Hardware.Memory_Block_Size'Access);
      XML_Processors.Register
        (Process => Hardware.PCI_Config_Space_Address'Access);
      XML_Processors.Register
        (Process => Hardware.CPU_Count'Access);
      XML_Processors.Register
        (Process => Hardware.CPU_Sub_Elements'Access);
      XML_Processors.Register
        (Process => Hardware.System_Board_Presence'Access);
      XML_Processors.Register
        (Process => Hardware.IOAPIC_Presence'Access);
      XML_Processors.Register
        (Process => Hardware.IOAPIC_Cap_SID'Access);
      XML_Processors.Register
        (Process => Platform.Kernel_Diagnostics_Device_Reference'Access);
      XML_Processors.Register
        (Process => Platform.Kernel_Diagnostics_Type_Resources'Access);
      XML_Processors.Register
        (Process => Memory.VTd_Root_Region_Presence'Access);
      XML_Processors.Register
        (Process => Memory.VTd_Root_Region_Size'Access);
      XML_Processors.Register
        (Process => Memory.VTd_Context_Region_Size'Access);
      XML_Processors.Register
        (Process => Memory.VTd_IRT_Region_Presence'Access);
      XML_Processors.Register
        (Process => Hardware.IOMMU_Presence'Access);
      XML_Processors.Register
        (Process => Hardware.IOMMU_Cap_Agaw'Access);
      XML_Processors.Register
        (Process => Hardware.IOMMU_Cap_Register_Offsets'Access);
      XML_Processors.Register
        (Process => Device.IOMMU_Region_Size'Access);
      XML_Processors.Register
        (Process => Kernel.IOMMU_Consecutiveness'Access);
      XML_Processors.Register
        (Process => Device_Domains.Device_Reference_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device_Domains.Domain_Memory_Overlap'Access);
      XML_Processors.Register
        (Process => Device_Domains.Memory_Reference_Uniqueness'Access);
      XML_Processors.Register
        (Process => Device_Domains.Domain_Memory_Type'Access);
      XML_Processors.Register
        (Process => Device_Domains.Domain_PT_Region_Presence'Access);
      XML_Processors.Register
        (Process => Device_Domains.PCI_Bus_Context_Region_Presence'Access);
   end Register_All;

   -------------------------------------------------------------------------

   procedure Run (Policy : String)
   is
      Data : Muxml.XML_Data_Type;
   begin
      Mulog.Log (Msg => "Validating policy '" & Policy & "'");

      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => Policy);
      Register_All;

      Mulog.Log
        (Msg => "Registered validators" & XML_Processors.Get_Count'Img);

      XML_Processors.Run (Data => Data);
      Mucfgcheck.Validation_Errors.Check;

      Mulog.Log (Msg => "Successfully validated policy '" & Policy & "'");
   end Run;

end Validate;
