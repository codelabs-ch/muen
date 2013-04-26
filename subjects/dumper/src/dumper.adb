with System.Machine_Code;

with Dump;

procedure Dumper
is
begin
   Dump.Initialize;
   System.Machine_Code.Asm
     (Template => "sti",
      Volatile => True);

   loop
      System.Machine_Code.Asm
        (Template => "hlt",
         Volatile => True);
   end loop;
end Dumper;
