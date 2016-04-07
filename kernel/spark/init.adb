with X86_64;

with SK.CPU_Global;
with SK.CPU_Registry;
with SK.Events;
with SK.FPU;
with SK.Interrupts;
with SK.IO_Apic;
with SK.Kernel;
with SK.MP;
with SK.Scheduler;
with SK.Subjects;
with SK.Subjects_Sinfo;
with SK.Tau0_Interface;
with SK.Timers;
with SK.VTd;
with SK.GDT;
with SK.VMX;

procedure Init
with
   Global =>
      (Input  => (SK.Tau0_Interface.State, SK.CPU_Global.CPU_ID,
                  SK.GDT.GDT_Pointer, SK.VMX.State),
       In_Out => (SK.CPU_Registry.State, SK.Events.State, SK.FPU.State,
                  SK.Interrupts.State, SK.IO_Apic.State, SK.MP.Barrier,
                  SK.Subjects.State, SK.Subjects_Sinfo.State, SK.Timers.State,
                  SK.VTd.State, X86_64.State),
       Output => SK.CPU_Global.State)
is
   Subject_Registers : SK.CPU_Registers_Type;
begin
   SK.Kernel.Initialize (Subject_Registers);
   loop
      SK.Scheduler.Handle_Vmx_Exit (Subject_Registers);
   end loop;
end Init;
