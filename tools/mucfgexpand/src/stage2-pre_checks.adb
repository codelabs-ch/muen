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

with Mutools.Immutable_Processors;
with Mucfgcheck.Memory;
with Mucfgcheck.Device;
with Mucfgcheck.Events;
with Mucfgcheck.Platform;
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

   procedure Register_All
   is
      use Cfgchecks;
   begin
      Check_Procs.Register
        (Process => Mucfgcheck.Memory.Physical_Memory_References'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Device.Device_Memory_References'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Device.PCI_Device_BDF_Uniqueness'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Device.IOMMU_Region_Size'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Platform.PCI_Config_Space_Address'Access);
      Check_Procs.Register
        (Process => Mucfgcheck.Platform.IOMMU_Cap_Agaw'Access);
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
      Check_Procs.Register (Process => Subject_Channel_References'Access);
      Check_Procs.Register (Process => Channel_Reader_Writer'Access);
      Check_Procs.Register (Process => Channel_Writer_Has_Event_ID'Access);
      Check_Procs.Register (Process => Channel_Reader_Has_Event_Vector'Access);
      Check_Procs.Register (Process => Platform_CPU_Count_Presence'Access);
      Check_Procs.Register (Process => Platform_IOAPIC_Presence'Access);
      Check_Procs.Register (Process => Platform_IOMMU_Memory'Access);
      Check_Procs.Register
        (Process => Kernel_Diagnostics_Dev_Reference'Access);

      --  Register after platform CPU count presence check.

      Check_Procs.Register (Process => Mucfgcheck.Platform.CPU_Count'Access);
   end Register_All;

   -------------------------------------------------------------------------

   procedure Run (Data : Muxml.XML_Data_Type) renames Check_Procs.Run;

end Stage2.Pre_Checks;
