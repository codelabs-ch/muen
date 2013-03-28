with Pack.OS;

package body OS_Tests
is

   use Ahven;
   use Pack;

   -------------------------------------------------------------------------

   procedure Execute_Command
   is
   begin
      OS.Execute (Command => "/bin/echo");

      begin
         OS.Execute (Command => "no_such_command 2>/dev/null");

      exception
         when OS.Command_Failed => null;
      end;
   end Execute_Command;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "OS package tests");
      T.Add_Test_Routine
        (Routine => Execute_Command'Access,
         Name    => "Execute command");
   end Initialize;

end OS_Tests;
