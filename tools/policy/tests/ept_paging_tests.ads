with Ahven.Framework;

package EPT_Paging_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  EPT PML4 entry creation tests.
   procedure Create_PML4_Entry;

end EPT_Paging_Tests;
