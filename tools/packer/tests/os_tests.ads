with Ahven.Framework;

package OS_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Test OS command execution.
   procedure Execute_Command;

end OS_Tests;
