with Skp.IOMMU;

with X86_64;

with SK.CPU_Info;
with SK.FPU;
with SK.Interrupt_Tables;
with SK.IO_Apic;
with SK.Kernel;
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
      (Input  => (SK.Tau0_Interface.State, SK.CPU_Info.CPU_ID,
                  SK.VMX.Exit_Address),
       In_Out => (SK.FPU.State, SK.IO_Apic.State, SK.MP.Barrier,
                  SK.Scheduling_Info.State, SK.Subjects.State,
                  SK.Subjects_Events.State, SK.Subjects_Interrupts.State,
                  SK.Subjects_MSR_Store.State, SK.Timed_Events.State,
                  Skp.IOMMU.State, SK.VMX.VMCS_State, SK.Crash_Audit.State,
                  X86_64.State, SK.Interrupt_Tables.State, SK.Scheduler.State))
is
   Subject_Registers : SK.CPU_Registers_Type;
begin
   SK.Kernel.Initialize (Subject_Registers);
   loop
      SK.Scheduler.Handle_Vmx_Exit (Subject_Registers);
   end loop;
end Init;
