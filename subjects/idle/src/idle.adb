with System.Machine_Code;

procedure Idle
is
begin
   System.Machine_Code.Asm (Template => "sti",
                            Volatile => True);
   loop
      System.Machine_Code.Asm (Template => "hlt",
                               Volatile => True);
   end loop;
end Idle;
