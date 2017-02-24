with Skp.IOMMU;

with X86_64;

with SK.CPU_Global;
with SK.CPU_Registry;
with SK.FPU;
with SK.Interrupts;
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
with SK.VTd;
with SK.GDT;
with SK.VMX;

procedure Init
with
   Global =>
      (Input  => (SK.Tau0_Interface.State, SK.CPU_Global.CPU_ID,
                  SK.GDT.GDT_Pointer, SK.VMX.Exit_Address,
                  SK.Interrupt_Tables.State, SK.Interrupts.State),
       In_Out => (SK.CPU_Registry.State, SK.FPU.State, SK.IO_Apic.State,
                  SK.MP.Barrier, SK.Scheduling_Info.State, SK.Subjects.State,
                  SK.Subjects_Events.State, SK.Subjects_Interrupts.State,
                  SK.Subjects_MSR_Store.State, SK.VTd.State,
                  SK.Timed_Events.State, Skp.IOMMU.State, SK.VMX.VMCS_State,
                  X86_64.State),
       Output => SK.CPU_Global.State)
is
   Subject_Registers : SK.CPU_Registers_Type;
begin
   SK.Kernel.Initialize (Subject_Registers);
   loop
      SK.Scheduler.Handle_Vmx_Exit (Subject_Registers);
   end loop;
end Init;
