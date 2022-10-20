--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp.IOMMU;

with X86_64;

with SK.CPU_Info;
with SK.FPU;
with SK.Interrupt_Tables;
with SK.IO_Apic;
with SK.MCE;
with SK.MP;
with SK.Scheduler;
with SK.Scheduling_Info;
with SK.Subjects;
with SK.Subjects_Events;
with SK.Subjects_Interrupts;
with SK.Subjects_MSR_Store;
with SK.Tau0_Interface;
with SK.Timed_Events;
with SK.VMX;
with SK.Crash_Audit;

--D @Interface
--D This package implements kernel initialization which is the initial entry
--D point from the early boot code into the SPARK kernel implementation. It
--D also contains the VM exit handler procedure which implements the main
--D kernel processing loop.
package SK.Kernel
is

   --  Initialize subject with given ID.
   procedure Init_Subject (ID : Skp.Global_Subject_ID_Type)
   with
      Global =>
        (Input  => (CPU_Info.APIC_ID, Interrupt_Tables.State,
                    VMX.Exit_Address),
         In_Out => (Crash_Audit.State, FPU.State, Subjects.State,
                    Subjects_Events.State, Subjects_Interrupts.State,
                    Subjects_MSR_Store.State, Timed_Events.State,
                    VMX.VMCS_State, X86_64.State));

   --  Kernel initialization.
   procedure Initialize (Subject_Registers : out CPU_Registers_Type)
   with
      Global =>
        (Input  => (CPU_Info.APIC_ID, CPU_Info.CPU_ID, CPU_Info.Is_BSP,
                    MCE.State, VMX.Exit_Address),
         In_Out => (Crash_Audit.State, FPU.State, Interrupt_Tables.State,
                    IO_Apic.State, MP.Barrier, Scheduler.State,
                    Scheduling_Info.State, Subjects.State,
                    Subjects_Events.State, Subjects_Interrupts.State,
                    Subjects_MSR_Store.State, Timed_Events.State,
                    VMX.VMCS_State, Skp.IOMMU.State, X86_64.State)),
      Post   => MCE.Valid_State,
      Export,
      Convention => C,
      Link_Name  => "sk_initialize";

   --  VMX exit handler.
   procedure Handle_Vmx_Exit (Subject_Registers : in out CPU_Registers_Type)
   with
      Global     =>
         (Input  => (CPU_Info.APIC_ID, CPU_Info.CPU_ID, CPU_Info.Is_BSP,
                     Interrupt_Tables.State, MCE.State, Tau0_Interface.State,
                     VMX.Exit_Address),
          In_Out => (Crash_Audit.State, FPU.State, IO_Apic.State, MP.Barrier,
                     Scheduler.State, Scheduler.Group_Activity_Indicator,
                     Subjects.State, Scheduling_Info.State,
                     Subjects_Events.State, Subjects_Interrupts.State,
                     Subjects_MSR_Store.State, Timed_Events.State,
                     VMX.VMCS_State, X86_64.State)),
      Pre        => MCE.Valid_State,
      Export,
      Convention => C,
      Link_Name  => "handle_vmx_exit";

end SK.Kernel;
