with Ahven.Framework;

package EPT_Paging_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  EPT PML4 entry creation tests.
   procedure Create_PML4_Entry;

   --  EPT PDPT entry creation tests.
   procedure Create_PDPT_Entry;

   --  EPT PD entry creation tests.
   procedure Create_PD_Entry;

   --  EPT PT entry creation tests.
   procedure Create_PT_Entry;

end EPT_Paging_Tests;
