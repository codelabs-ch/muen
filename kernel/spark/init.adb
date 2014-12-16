with SK.CPU_Global;
with SK.CPU_Registry;
with SK.Events;
with SK.Interrupts;
with SK.IO_Apic;
with SK.Kernel;
with SK.MP;
with SK.Scheduler;
with SK.Subjects;
with SK.Timers;
with SK.VTd;
with X86_64;

procedure Init
with
   Global =>
      (Input  => SK.Scheduler.Tau0_Kernel_Interface,
       In_Out => (SK.CPU_Registry.State, SK.Events.State, SK.Interrupts.State,
                  SK.IO_Apic.State, SK.MP.Barrier, SK.Scheduler.State,
                  SK.Subjects.State, SK.Timers.State, SK.VTd.State,
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
