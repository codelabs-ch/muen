with SK;

with Skp.Paging.EPT;

package body EPT_Paging_Tests
is

   use Ahven;
   use Skp;
   use Skp.Paging;
   use type SK.Word64;

   -------------------------------------------------------------------------

   procedure Create_PML4_Entry
   is
      E     : PML4_Entry_Type;
      Addr  : constant SK.Word64       := 16#1f1000#;
      Ref_E : constant PML4_Entry_Type := 16#1f1003#;
   begin
      E := EPT.Create_PML4_Entry (Address    => Addr,
                                  Readable   => True,
                                  Writable   => True,
                                  Executable => False);

      Assert (Condition => E = Ref_E,
              Message   => "PML4 entry mismatch");
      Assert (Condition => Get_PDPT_Address (E => E) = Addr,
              Message   => "Address mismatch");
   end Create_PML4_Entry;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "EPT Paging tests");
      T.Add_Test_Routine
        (Routine => Create_PML4_Entry'Access,
         Name    => "PML4 entry creation");
   end Initialize;

end EPT_Paging_Tests;
