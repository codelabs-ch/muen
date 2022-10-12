with Skp.IOMMU;

with X86_64;

with SK.CPU_Info;
with SK.FPU;
with SK.Interrupt_Tables;
with SK.IO_Apic;
with SK.Kernel;
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

procedure Init
with
   Global =>
      (Input  => (SK.CPU_Info.APIC_ID, SK.CPU_Info.CPU_ID, SK.CPU_Info.Is_BSP,
                  SK.MCE.State, SK.Tau0_Interface.State, SK.VMX.Exit_Address),
       In_Out => (SK.Crash_Audit.State, SK.FPU.State, SK.IO_Apic.State,
                  SK.Interrupt_Tables.State, SK.MP.Barrier, SK.Scheduler.State,
                  SK.Scheduler.Group_Activity_Indicator,
                  SK.Scheduling_Info.State, SK.Subjects.State,
                  SK.Subjects_Events.State, SK.Subjects_Interrupts.State,
                  SK.Subjects_MSR_Store.State, SK.Timed_Events.State,
                  SK.VMX.VMCS_State, Skp.IOMMU.State, X86_64.State))
is
   Subject_Registers : SK.CPU_Registers_Type;
begin
   SK.Kernel.Initialize (Subject_Registers);
   loop
      SK.Kernel.Handle_Vmx_Exit (Subject_Registers);
   end loop;
end Init;
