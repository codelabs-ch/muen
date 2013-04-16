with System.Machine_Code;

with SubjC;

procedure Subject_C
is
begin
   SubjC.Initialize;

   System.Machine_Code.Asm
     (Template => "sti",
      Volatile => True);
   loop

      --  All processing is done in the interrupt handler.

      System.Machine_Code.Asm
        (Template => "hlt",
         Volatile => True);
   end loop;
end Subject_C;
