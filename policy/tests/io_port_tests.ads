with Ahven.Framework;

package IO_Port_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Verify I/O bitmap handling.
   procedure IO_Bitmap_Handling;

end IO_Port_Tests;
