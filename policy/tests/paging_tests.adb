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
   end Initialize;

end Paging_Tests;
