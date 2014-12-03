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

with Expanders.Memory;
with Expanders.Kernel;
with Expanders.Subjects;
with Expanders.Channels;
with Expanders.Platform;
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

      --  Add tau0 subject prior to subject-related memory expanders (state,
      --  VMCS and bitmaps).

      Procs.Register (Process => Subjects.Add_Tau0'Access);

      --  Set subject ids and cpu prior to kernel subject state mapping
      --  expander.

      Procs.Register (Process => Subjects.Add_Ids'Access);
      Procs.Register (Process => Subjects.Add_CPU_Ids'Access);

      Procs.Register (Process => Memory.Add_Missing_Attributes'Access);
      Procs.Register (Process => Memory.Add_Kernel_Binary'Access);
      Procs.Register (Process => Memory.Add_Stack_Store'Access);
      Procs.Register (Process => Memory.Add_Subject_States'Access);
      Procs.Register (Process => Memory.Add_Subject_Timer_Pages'Access);
      Procs.Register (Process => Memory.Add_Tau0_Interface'Access);
      Procs.Register (Process => Memory.Add_AP_Trampoline'Access);
      Procs.Register (Process => Memory.Add_VMXON_Regions'Access);
      Procs.Register (Process => Memory.Add_VMCS_Regions'Access);
      Procs.Register (Process => Memory.Add_Subject_Bitmaps'Access);
      Procs.Register (Process => Kernel.Add_Section_Skeleton'Access);
      Procs.Register (Process => Kernel.Add_Binary_Mappings'Access);
      Procs.Register (Process => Kernel.Add_Subj_State_Mappings'Access);
      Procs.Register (Process => Kernel.Add_Subj_Timer_Mappings'Access);
      Procs.Register (Process => Kernel.Map_Tau0_Interface'Access);
      Procs.Register (Process => Kernel.Add_Devices'Access);

      Procs.Register (Process => Subjects.Handle_Profile'Access);
      Procs.Register (Process => Subjects.Handle_Monitors'Access);
      Procs.Register (Process => Subjects.Add_Channel_Mappings'Access);
      Procs.Register (Process => Subjects.Add_Channel_Events'Access);
      Procs.Register (Process => Subjects.Remove_Channel_Elements'Access);
      Procs.Register (Process => Subjects.Add_Default_Events'Access);
      Procs.Register (Process => Subjects.Add_Device_Memory_Mappings'Access);
      Procs.Register (Process => Subjects.Add_Device_BDFs'Access);
      Procs.Register (Process => Channels.Add_Physical_Memory'Access);

      Procs.Register (Process => Scheduling.Add_Barrier_Configs'Access);

      --  BDF of device references must be expanded

      Procs.Register (Process => Platform.Add_PCI_Config_Space'Access);

      --  Subject profiles must be expanded since they may add MSR registers.

      Procs.Register (Process => Memory.Add_Subject_MSR_Store'Access);

      --  All kernel/subject memory regions and mappings must exist and specify
      --  and alignment to add PTs.

      Procs.Register (Process => Memory.Add_Kernel_PTs'Access);
      Procs.Register (Process => Memory.Add_Subject_PTs'Access);

      Procs.Register (Process => Device_Domains.Add_Section_Skeleton'Access);
      Procs.Register (Process => Device_Domains.Add_Domain_IDs'Access);
      Procs.Register (Process => Device_Domains.Add_Tables'Access);
   end Register_All;

   -------------------------------------------------------------------------

   procedure Run (Data : in out Muxml.XML_Data_Type) renames Procs.Run;

end Stage2.Expansion;
