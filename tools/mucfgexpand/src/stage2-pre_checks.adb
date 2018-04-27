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

with Mutools.System_Config;
with Mutools.Immutable_Processors;
with Mucfgcheck.Memory;
with Mucfgcheck.Device;
with Mucfgcheck.Events;
with Mucfgcheck.Hardware;
with Mucfgcheck.Subject;

with Cfgchecks;

pragma Elaborate_All (Mutools.Immutable_Processors);

package body Stage2.Pre_Checks
is

   -------------------------------------------------------------------------

   procedure Clear
   is
   begin
      Check_Procs.Clear;
   end Clear;

   -------------------------------------------------------------------------

   function Get_Count return Natural renames Check_Procs.Get_Count;

   -------------------------------------------------------------------------

   procedure Register_All (Data : Muxml.XML_Data_Type)
   is
      use Cfgchecks;
   begin
      Check_Procs.Register
        (Process => Mucfgcheck.Memory.Physical_Memory_References'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Memory.Uncached_Crash_Audit_Presence'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Device.Device_Memory_References'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Device.PCI_Device_BDF_Uniqueness'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Device.IO_Port_Uniqueness'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Hardware.PCI_Config_Space_Address'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Hardware.System_Board_Presence'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Events.Physical_Event_Name_Uniqueness'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Events.Subject_Event_References'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Events.Source_Targets'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Events.Self_References'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Subject.Runnability'Access);

      Check_Procs.Register (Process => Tau0_Presence_In_Scheduling'Access);
      Check_Procs.Register (Process => Subject_Monitor_References'Access);
      Check_Procs.Register
        (Process => Subject_Monitor_Loader_Addresses'Access);
      Check_Procs.Register (Process => Subject_Monitor_Loader_States'Access);
      Check_Procs.Register (Process => Subject_Channel_References'Access);
      Check_Procs.Register (Process => Subject_Sibling_Device_BDFs'Access);
      Check_Procs.Register
        (Process => Subject_Sibling_Device_Uniqueness'Access);
      Check_Procs.Register (Process => Channel_Reader_Writer'Access);
      Check_Procs.Register (Process => Channel_Writer_Has_Event_ID'Access);
      Check_Procs.Register (Process => Channel_Reader_Has_Event_Vector'Access);
      Check_Procs.Register (Process => Hardware_CPU_Count_Presence'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Hardware.IOAPIC_Presence'Access);
      Check_Procs.Register
        (Process => Kernel_Diagnostics_Dev_Reference'Access);

      --  Register after hardware CPU count presence check.

      Check_Procs.Register (Process => Mucfgcheck.Hardware.CPU_Count'Access);

      --  IOMMU config.

      if Mutools.System_Config.Get_Value
        (Data => Data,
         Name => "iommu_enabled")
      then
         Check_Procs.Register
           (Process => Mucfgcheck.Hardware.IOMMU_Presence'Access);
         Check_Procs.Register
           (Process => Mucfgcheck.Device.IOMMU_Region_Size'Access);
      end if;
   end Register_All;

   -------------------------------------------------------------------------

   procedure Run (Data : Muxml.XML_Data_Type) renames Check_Procs.Run;

end Stage2.Pre_Checks;
