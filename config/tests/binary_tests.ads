with Ahven.Framework;

package Binary_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Read information from a binary file.
   procedure Read_Binary;

end Binary_Tests;
