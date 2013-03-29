with Ahven.Framework;

package Writer_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Write kernel policy files.
   procedure Write_Kernel;

   --  Write subject policy files.
   procedure Write_Subjects;

   --  Write system policy files.
   procedure Write_System;

   --  Write binary spec files.
   procedure Write_Binaries;

end Writer_Tests;
