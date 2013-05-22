with Ada.Text_IO;

with SK;

with Skp.Paging.EPT;

package body EPT_Paging_Tests
is

   use Ahven;
   use Skp;
   use Skp.Paging;
   use type SK.Word64;

   -------------------------------------------------------------------------

   procedure Create_PD_Entry
   is
      E     : PD_Entry_Type;
      Addr  : constant SK.Word64     := 16#8fff10000#;
      Ref_E : constant PD_Entry_Type := 16#8fff10002#;
   begin
      E := EPT.Create_PD_Entry (Address     => Addr,
                                Readable    => False,
                                Writable    => True,
                                Executable  => False,
                                Map_Page    => False,
                                Ignore_PAT  => False,
                                Memory_Type => UC);

      Assert (Condition => E = Ref_E,
              Message   => "PD entry mismatch");
      Assert (Condition => Get_PT_Address (E => E) = Addr,
              Message   => "Address mismatch");
   end Create_PD_Entry;

   -------------------------------------------------------------------------

   procedure Create_PDPT_Entry
   is
      E     : PDPT_Entry_Type;
      Addr  : constant SK.Word64       := 16#d1324b000#;
      Ref_E : constant PDPT_Entry_Type := 16#d1324b006#;
   begin
      E := EPT.Create_PDPT_Entry (Address     => Addr,
                                  Readable    => False,
                                  Writable    => True,
                                  Executable  => True,
                                  Map_Page    => False,
                                  Ignore_PAT  => False,
                                  Memory_Type => WC);

      Assert (Condition => E = Ref_E,
              Message   => "PDPT entry mismatch");
      Assert (Condition => Get_PD_Address (E => E) = Addr,
              Message   => "Address mismatch");
   end Create_PDPT_Entry;

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

   procedure Create_PT_Entry
   is
      E     : PT_Entry_Type;
      Addr  : constant SK.Word64     := 16#ab763fc00000#;
      Ref_E : constant PT_Entry_Type := 16#ab763fc000f2#;
   begin
      E := EPT.Create_PT_Entry (Address     => Addr,
                                Readable    => False,
                                Writable    => True,
                                Executable  => False,
                                Map_Page    => True,
                                Ignore_PAT  => True,
                                Memory_Type => WB);
      Ada.Text_IO.Put_Line (E'Img);

      Assert (Condition => E = Ref_E,
              Message   => "PT entry mismatch");
      Assert (Condition => Get_Address (E => E) = Addr,
              Message   => "Address mismatch");
   end Create_PT_Entry;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "EPT Paging tests");
      T.Add_Test_Routine
        (Routine => Create_PML4_Entry'Access,
         Name    => "PML4 entry creation");
      T.Add_Test_Routine
        (Routine => Create_PDPT_Entry'Access,
         Name    => "PDPT entry creation");
      T.Add_Test_Routine
        (Routine => Create_PD_Entry'Access,
         Name    => "PD entry creation");
      T.Add_Test_Routine
        (Routine => Create_PT_Entry'Access,
         Name    => "PT entry creation");
   end Initialize;

end EPT_Paging_Tests;
