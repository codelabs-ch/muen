with Ahven.Framework;

package Paging_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  PML4 entry creation tests.
   procedure Create_PML4_Entry;

   --  PDPT entry creation tests.
   procedure Create_PDPT_Entry;

end Paging_Tests;
