with System.Machine_Code;

with SubjC;
with VGA_Output;

procedure Subject_C
is
begin
   SubjC.Initialize;

   System.Machine_Code.Asm
     (Template => "sti",
      Volatile => True);

   VGA_Output.Sync;
end Subject_C;
