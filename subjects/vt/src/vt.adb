with SK.CPU;

with Interrupts;
with Handler;
with VGA_Output;

procedure VT
is
begin
   Interrupts.Initialize;
   Handler.Initialize;

   SK.CPU.Sti;

   VGA_Output.Sync;
end VT;
