with Ahven.Framework;

package MSR_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Verify MSR bitmap handling.
   procedure MSR_Bitmap_Handling;

end MSR_Tests;
