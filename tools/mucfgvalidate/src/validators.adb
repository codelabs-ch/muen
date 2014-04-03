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

with Validate.XML_Processors;

with Mucfgcheck.Memory;
with Mucfgcheck.MSR;
with Mucfgcheck.Device;
with Mucfgcheck.Scheduling;
with Mucfgcheck.Kernel;
with Mucfgcheck.Subject;
with Mucfgcheck.Platform;

package body Validators
is

   -------------------------------------------------------------------------

   procedure Register_All
   is
      use Mucfgcheck;
   begin
      Validate.XML_Processors.Register
        (Process => Memory.Physical_Memory_Name_Uniqueness'Access);
      Validate.XML_Processors.Register
        (Process => Memory.Physical_Memory_References'Access);
      Validate.XML_Processors.Register
        (Process => Memory.VMXON_Region_Presence'Access);
      Validate.XML_Processors.Register
        (Process => Memory.VMXON_Region_Size'Access);
      Validate.XML_Processors.Register
        (Process => Memory.VMXON_In_Lowmem'Access);
      Validate.XML_Processors.Register
        (Process => Memory.VMXON_Consecutiveness'Access);
      Validate.XML_Processors.Register
        (Process => Memory.VMCS_Region_Presence'Access);
      Validate.XML_Processors.Register
        (Process => Memory.VMCS_Region_Size'Access);
      Validate.XML_Processors.Register
        (Process => Memory.VMCS_In_Lowmem'Access);
      Validate.XML_Processors.Register
        (Process => Memory.VMCS_Consecutiveness'Access);
      Validate.XML_Processors.Register
        (Process => Memory.Physical_Address_Alignment'Access);
      Validate.XML_Processors.Register
        (Process => Memory.Virtual_Address_Alignment'Access);
      Validate.XML_Processors.Register
        (Process => Memory.Region_Size'Access);
      Validate.XML_Processors.Register
        (Process => Memory.Entity_Name_Encoding'Access);
      Validate.XML_Processors.Register
        (Process => Memory.Physical_Memory_Overlap'Access);
      Validate.XML_Processors.Register
        (Process => Memory.Virtual_Memory_Overlap'Access);
      Validate.XML_Processors.Register
        (Process => Memory.Kernel_Stack_Region_Presence'Access);
      Validate.XML_Processors.Register
        (Process => Memory.Kernel_Store_Region_Presence'Access);
      Validate.XML_Processors.Register
        (Process => Memory.Kernel_PT_Region_Presence'Access);
      Validate.XML_Processors.Register
        (Process => Memory.Kernel_PT_Consecutiveness'Access);
      Validate.XML_Processors.Register
        (Process => MSR.Start_Smaller_End'Access);
      Validate.XML_Processors.Register
        (Process => MSR.Low_Or_High'Access);
      Validate.XML_Processors.Register
        (Process => Device.Physical_Device_Name_Uniqueness'Access);
      Validate.XML_Processors.Register
        (Process => Device.Physical_Device_References'Access);
      Validate.XML_Processors.Register
        (Process => Device.Physical_IRQ_Uniqueness'Access);
      Validate.XML_Processors.Register
        (Process => Device.Device_IRQ_Name_Uniqueness'Access);
      Validate.XML_Processors.Register
        (Process => Device.Physical_IRQ_References'Access);
      Validate.XML_Processors.Register
        (Process => Device.IRQ_Number_Equality'Access);
      Validate.XML_Processors.Register
        (Process => Device.IO_Port_Start_Smaller_End'Access);
      Validate.XML_Processors.Register
        (Process => Device.Device_IO_Port_Name_Uniqueness'Access);
      Validate.XML_Processors.Register
        (Process => Device.IO_Port_References'Access);
      Validate.XML_Processors.Register
        (Process => Device.IO_Port_Range_Equality'Access);
      Validate.XML_Processors.Register
        (Process => Device.Device_Memory_Name_Uniqueness'Access);
      Validate.XML_Processors.Register
        (Process => Device.Device_Memory_References'Access);
      Validate.XML_Processors.Register
        (Process => Device.Device_Sharing'Access);
      Validate.XML_Processors.Register
        (Process => Scheduling.CPU_Element_Count'Access);
      Validate.XML_Processors.Register
        (Process => Scheduling.Subject_References'Access);
      Validate.XML_Processors.Register
        (Process => Scheduling.Subject_CPU_Affinity'Access);
      Validate.XML_Processors.Register
        (Process => Scheduling.Major_Frame_Ticks'Access);
      Validate.XML_Processors.Register
        (Process => Kernel.CPU_Store_Address_Equality'Access);
      Validate.XML_Processors.Register
        (Process => Kernel.Stack_Address_Equality'Access);
      Validate.XML_Processors.Register
        (Process => Subject.Name_Uniqueness'Access);
      Validate.XML_Processors.Register
        (Process => Subject.CPU_ID'Access);
      Validate.XML_Processors.Register
        (Process => Subject.Event_Subject_References'Access);
      Validate.XML_Processors.Register
        (Process => Subject.Event_Self_References'Access);
      Validate.XML_Processors.Register
        (Process => Subject.Event_Switch_Same_Core'Access);
      Validate.XML_Processors.Register
        (Process => Subject.Event_IPI_Different_Core'Access);
      Validate.XML_Processors.Register
        (Process => Platform.Memory_Space'Access);
      Validate.XML_Processors.Register
        (Process => Platform.Memory_Block_Overlap'Access);
   end Register_All;

end Validators;
