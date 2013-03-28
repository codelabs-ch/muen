with GNAT.OS_Lib;

package body Pack.OS
is

   Shell : constant String := "/bin/bash";

   -------------------------------------------------------------------------

   procedure Execute (Command : String)
   is
      Status : Boolean;
      Args   : GNAT.OS_Lib.Argument_List (1 .. 5);
   begin
      Args (1) := new String'(Shell);
      Args (2) := new String'("-o");
      Args (3) := new String'("pipefail");
      Args (4) := new String'("-c");
      Args (5) := new String'(Command);

      GNAT.OS_Lib.Spawn (Program_Name => Args (Args'First).all,
                         Args         => Args (Args'First + 1 .. Args'Last),
                         Success      => Status);
      for A in Args'Range loop
         GNAT.OS_Lib.Free (X => Args (A));
      end loop;

      if not Status then
         raise Command_Failed with "Command '" & Command & "' failed";
      end if;
   end Execute;

end Pack.OS;
