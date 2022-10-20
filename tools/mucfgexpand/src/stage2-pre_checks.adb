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

with Mutools.Immutable_Processors;
with Mucfgcheck.Memory;
with Mucfgcheck.Device;
with Mucfgcheck.Events;
with Mucfgcheck.Hardware;
with Mucfgcheck.Platform;
with Mucfgcheck.Scheduling;
with Mucfgcheck.Subject;
with Mucfgcheck.Validation_Errors;

with Cfgchecks;

pragma Elaborate_All (Mutools.Immutable_Processors);

package body Stage2.Pre_Checks
is

   package MC renames Mucfgcheck;

   -------------------------------------------------------------------------

   procedure Clear
   is
   begin
      Check_Procs.Clear;
   end Clear;

   -------------------------------------------------------------------------

   function Get_Count return Natural renames Check_Procs.Get_Count;

   -------------------------------------------------------------------------

   procedure Register_All
   is
      use Cfgchecks;
   begin
      Check_Procs.Register
        (Process => MC.Memory.Physical_Memory_References'Access);
      Check_Procs.Register
        (Process => MC.Memory.Uncached_Crash_Audit_Presence'Access);
      Check_Procs.Register
        (Process => MC.Device.Device_Reference_Uniqueness'Access);
      Check_Procs.Register
        (Process => MC.Device.Device_Memory_References'Access);
      Check_Procs.Register
        (Process => MC.Device.PCI_Device_BDF_Uniqueness'Access);
      Check_Procs.Register
        (Process => MC.Device.IO_Port_Uniqueness'Access);
      Check_Procs.Register
        (Process => MC.Hardware.PCI_Config_Space'Access);
      Check_Procs.Register
        (Process => MC.Hardware.CPU_Sub_Elements'Access);
      Check_Procs.Register
        (Process => MC.Hardware.System_Board_Presence'Access);
      Check_Procs.Register
        (Process => MC.Events.Physical_Event_Name_Uniqueness'Access);
      Check_Procs.Register
        (Process => MC.Events.Subject_Event_References'Access);
      Check_Procs.Register
        (Process => MC.Events.Source_Targets'Access);
      Check_Procs.Register
        (Process => MC.Events.Self_References'Access);
      Check_Procs.Register
        (Process => MC.Events.Source_VMX_Exit_Event_Completeness'Access);
      Check_Procs.Register
        (Process => MC.Scheduling.Subject_Scheduling_Group_Assignment'Access);
      Check_Procs.Register
        (Process => MC.Scheduling.Minor_Frame_Partition_References'Access);
      Check_Procs.Register
        (Process => MC.Subject.Runnability'Access);
      Check_Procs.Register
        (Process => MC.Subject.Logical_Device_Name_Uniqueness'Access);

      Check_Procs.Register (Process => Tau0_Presence_In_Scheduling'Access);
      Check_Procs.Register (Process => Subject_Monitor_References'Access);
      Check_Procs.Register
        (Process => Subject_Monitor_Loader_Addresses'Access);
      Check_Procs.Register (Process => Subject_Channel_References'Access);
      Check_Procs.Register (Process => Subject_Sibling_Bootparams'Access);
      Check_Procs.Register (Process => Subject_Sibling_Memory'Access);
      Check_Procs.Register (Process => Subject_Sibling_Device_BDFs'Access);
      Check_Procs.Register (Process => Channel_Reader_Writer'Access);
      Check_Procs.Register (Process => Channel_Writer_Has_Event_ID'Access);
      Check_Procs.Register (Process => Channel_Reader_Has_Event_Vector'Access);
      Check_Procs.Register (Process => Hardware_CPU_Count_Presence'Access);
      Check_Procs.Register
        (Process => MC.Hardware.IOAPIC_Presence'Access);
      Check_Procs.Register
        (Process => MC.Platform.Kernel_Diagnostics_Device_Reference'Access);
      Check_Procs.Register
        (Process => MC.Platform.Kernel_Diagnostics_Type_Resources'Access);

      --  Register after hardware CPU count presence check.

      Check_Procs.Register (Process => MC.Hardware.CPU_Count'Access);

      Check_Procs.Register
        (Process => MC.Hardware.IOMMU_Presence'Access);
      Check_Procs.Register
        (Process => MC.Device.IOMMU_Region_Size'Access);
   end Register_All;

   -------------------------------------------------------------------------

   procedure Run (Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Procs.Run (Data => Data);
      Mucfgcheck.Validation_Errors.Check;
   end Run;

end Stage2.Pre_Checks;
