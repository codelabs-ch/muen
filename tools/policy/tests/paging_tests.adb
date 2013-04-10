with SK;

with Skp.Paging;

package body Paging_Tests
is

   use Ahven;
   use Skp;

   -------------------------------------------------------------------------

   procedure Create_PD_Entry
   is
      use type SK.Word64;

      E    : Paging.PD_Entry_Type;
      Addr : constant SK.Word64 := 16#fffc800f0000#;
   begin
      E := Paging.Create_PD_Entry
        (Address       => Addr,
         Writable      => True,
         User_Access   => False,
         Writethrough  => True,
         Cache_Disable => False,
         Map_Page      => False,
         Global        => False,
         PAT           => False,
         Exec_Disable  => True);

      Assert (Condition => Paging.Get_PT_Address (E => E) = Addr,
              Message   => "Address mismatch");
      Assert (Condition => Paging.Is_Present (E => E),
              Message   => "Entry not marked present");
      Assert (Condition => Paging.Is_Writable (E => E),
              Message   => "Entry not writable");
      Assert (Condition => not Paging.Is_User_Accessible (E => E),
              Message   => "Entry user accessible");
      Assert (Condition => Paging.Is_Writethrough (E => E),
              Message   => "Entry not marked writethrough");
      Assert (Condition => not Paging.Is_Cache_Disabled (E => E),
              Message   => "Cache disabled");
      Assert (Condition => not Paging.Maps_Page (E => E),
              Message   => "Entry maps a page");
      Assert (Condition => not Paging.Has_PAT (E => E),
              Message   => "PAT enabled");
      Assert (Condition => Paging.Has_Execute_Disable (E => E),
              Message   => "Entry is executable");
   end Create_PD_Entry;

   -------------------------------------------------------------------------

   procedure Create_PDPT_Entry
   is
      use type SK.Word64;

      E    : Paging.PDPT_Entry_Type;
      Addr : constant SK.Word64 := 16#2b3c004000#;
   begin
      E := Paging.Create_PDPT_Entry
        (Address       => Addr,
         Writable      => True,
         User_Access   => False,
         Writethrough  => True,
         Cache_Disable => False,
         Map_Page      => False,
         Global        => False,
         PAT           => False,
         Exec_Disable  => True);

      Assert (Condition => Paging.Get_PD_Address (E => E) = Addr,
              Message   => "Address mismatch");
      Assert (Condition => Paging.Is_Present (E => E),
              Message   => "Entry not marked present");
      Assert (Condition => Paging.Is_Writable (E => E),
              Message   => "Entry not writable");
      Assert (Condition => not Paging.Is_User_Accessible (E => E),
              Message   => "Entry user accessible");
      Assert (Condition => Paging.Is_Writethrough (E => E),
              Message   => "Entry not marked writethrough");
      Assert (Condition => not Paging.Is_Cache_Disabled (E => E),
              Message   => "Cache disabled");
      Assert (Condition => not Paging.Maps_Page (E => E),
              Message   => "Entry maps a page");
      Assert (Condition => not Paging.Has_PAT (E => E),
              Message   => "PAT enabled");
      Assert (Condition => Paging.Has_Execute_Disable (E => E),
              Message   => "Entry is executable");
   end Create_PDPT_Entry;

   -------------------------------------------------------------------------

   procedure Create_PML4_Entry
   is
      use type SK.Word64;

      E    : Paging.PML4_Entry_Type;
      Addr : constant SK.Word64 := 16#1f1000#;
   begin
      E := Paging.Create_PML4_Entry
        (Address       => Addr,
         Writable      => True,
         User_Access   => False,
         Writethrough  => True,
         Cache_Disable => False,
         Exec_Disable  => True);

      Assert (Condition => Paging.Get_PDPT_Address (E => E) = Addr,
              Message   => "Address mismatch");
      Assert (Condition => Paging.Is_Present (E => E),
              Message   => "Entry not marked present");
      Assert (Condition => Paging.Is_Writable (E => E),
              Message   => "Entry not writable");
      Assert (Condition => not Paging.Is_User_Accessible (E => E),
              Message   => "Entry user accessible");
      Assert (Condition => Paging.Is_Writethrough (E => E),
              Message   => "Entry not marked writethrough");
      Assert (Condition => not Paging.Is_Cache_Disabled (E => E),
              Message   => "Cache disabled");
      Assert (Condition => Paging.Has_Execute_Disable (E => E),
              Message   => "Entry is executable");
   end Create_PML4_Entry;

   -------------------------------------------------------------------------

   procedure Create_PT_Entry
   is
      use type SK.Word64;

      E    : Paging.PT_Entry_Type;
      Addr : constant SK.Word64 := 16#100043f000#;
   begin
      E := Paging.Create_PT_Entry
        (Address       => Addr,
         Writable      => True,
         User_Access   => False,
         Writethrough  => True,
         Cache_Disable => False,
         Global        => True,
         PAT           => False,
         Exec_Disable  => False);

      Assert (Condition => Paging.Get_Address (E => E) = Addr,
              Message   => "Address mismatch");
      Assert (Condition => Paging.Is_Present (E => E),
              Message   => "Entry not marked present");
      Assert (Condition => Paging.Is_Writable (E => E),
              Message   => "Entry not writable");
      Assert (Condition => not Paging.Is_User_Accessible (E => E),
              Message   => "Entry user accessible");
      Assert (Condition => Paging.Is_Writethrough (E => E),
              Message   => "Entry not marked writethrough");
      Assert (Condition => not Paging.Is_Cache_Disabled (E => E),
              Message   => "Cache disabled");
      Assert (Condition => Paging.Is_Global (E => E),
              Message   => "Entry not global");
      Assert (Condition => not Paging.PT_Has_PAT (E => E),
              Message   => "PAT enabled");
      Assert (Condition => not Paging.Has_Execute_Disable (E => E),
              Message   => "Entry is non-executable");
      Assert (Condition => not Paging.Is_Dirty (E => E),
              Message   => "Entry is dirty");
   end Create_PT_Entry;

   -------------------------------------------------------------------------

   procedure Index_Calculation
   is
      use type Skp.Paging.Table_Range;

      PML4, PDPT, PD, PT : Paging.Table_Range;
   begin
      Paging.Get_Indexes (Address    => 0,
                          PML4_Index => PML4,
                          PDPT_Index => PDPT,
                          PD_Index   => PD,
                          PT_Index   => PT);
      Assert (Condition => PML4 = 1,
              Message   => "PML4 index mismatch (1)");
      Assert (Condition => PDPT = 1,
              Message   => "PDPT index mismatch (1)");
      Assert (Condition => PD = 1,
              Message   => "PD index mismatch (1)");
      Assert (Condition => PT = 1,
              Message   => "PT index mismatch (1)");

      Paging.Get_Indexes (Address    => SK.Word64'Last,
                          PML4_Index => PML4,
                          PDPT_Index => PDPT,
                          PD_Index   => PD,
                          PT_Index   => PT);
      Assert (Condition => PML4 = Paging.Table_Range'Last,
              Message   => "PML4 index mismatch (2)");
      Assert (Condition => PDPT = Paging.Table_Range'Last,
              Message   => "PDPT index mismatch (2)");
      Assert (Condition => PD = Paging.Table_Range'Last,
              Message   => "PD index mismatch (2)");
      Assert (Condition => PT = Paging.Table_Range'Last,
              Message   => "PT index mismatch (2)");

      Paging.Get_Indexes (Address    => 16#fffc80200f000#,
                          PML4_Index => PML4,
                          PDPT_Index => PDPT,
                          PD_Index   => PD,
                          PT_Index   => PT);
            Assert (Condition => PML4 = 512,
              Message   => "PML4 index mismatch (3)");
      Assert (Condition => PDPT = 289,
              Message   => "PDPT index mismatch (3)");
      Assert (Condition => PD = 17,
              Message   => "PD index mismatch (3)");
      Assert (Condition => PT = 16,
              Message   => "PT index mismatch (3)");
   end Index_Calculation;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Paging tests");
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
      T.Add_Test_Routine
        (Routine => Index_Calculation'Access,
         Name    => "Paging structure index calculation");
   end Initialize;

end Paging_Tests;
