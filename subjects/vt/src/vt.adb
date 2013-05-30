with System.Machine_Code;

with Interrupts;
with Handler;
with VGA_Output;

procedure VT
is
begin
   Interrupts.Initialize;
   Handler.Initialize;

   System.Machine_Code.Asm
     (Template => "sti",
      Volatile => True);

   VGA_Output.Sync;
end VT;
