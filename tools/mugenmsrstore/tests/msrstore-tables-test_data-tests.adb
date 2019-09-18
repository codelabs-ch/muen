--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Msrstore.Tables.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Msrstore.Tables.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Is_Full (Gnattest_T : in out Test);
   procedure Test_Is_Full_aaff6f (Gnattest_T : in out Test) renames Test_Is_Full;
--  id:2.2/aaff6f63df362f53/Is_Full/1/0/
   procedure Test_Is_Full (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Store : MSR_Store_Type (Size => 2);
   begin
      Assert (Condition => Store.Size = 2,
              Message   => "Invalid MSR store size");
      Assert (Condition => Store.Next_Idx = 1,
              Message   => "Empty MSR store contains entry");

      Assert (Condition => not Is_Full (Store => Store),
              Message   => "Empty MSR store is full");

      Store.Next_Idx := 2;
      Assert (Condition => not Is_Full (Store => Store),
              Message   => "MSR store is full");

      Store.Next_Idx := 3;
      Assert (Condition => Is_Full (Store => Store),
              Message   => "MSR store not full");
--  begin read only
   end Test_Is_Full;
--  end read only


--  begin read only
   procedure Test_Is_Empty (Gnattest_T : in out Test);
   procedure Test_Is_Empty_7c576a (Gnattest_T : in out Test) renames Test_Is_Empty;
--  id:2.2/7c576a03a21b9e2d/Is_Empty/1/0/
   procedure Test_Is_Empty (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Store : MSR_Store_Type (Size => 2);
   begin
      Assert (Condition => Store.Next_Idx = 1,
              Message   => "Empty MSR store contains entry");

      Assert (Condition => Is_Empty (Store => Store),
              Message   => "MSR store not empty");

      Store.Next_Idx := 2;
      Assert (Condition => not Is_Empty (Store => Store),
              Message   => "MSR store still empty");
--  begin read only
   end Test_Is_Empty;
--  end read only


--  begin read only
   procedure Test_Append_Entry (Gnattest_T : in out Test);
   procedure Test_Append_Entry_ce511a (Gnattest_T : in out Test) renames Test_Append_Entry;
--  id:2.2/ce511a06b30ed5cd/Append_Entry/1/0/
   procedure Test_Append_Entry (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_32;
      use type Interfaces.Unsigned_64;

      Store : MSR_Store_Type (Size => 2);
   begin
      Append_Entry (Store => Store,
                    Index => 16#c000_0080#,
                    Data  => 16#cafe_beef#);
      Assert (Condition => Store.Next_Idx = 2,
              Message   => "Entry not appended (1)");
      Assert (Condition => Store.Data (1).Index = 16#c000_0080#,
              Message   => "Index mismatch (1)");
      Assert (Condition => Store.Data (1).Reserved = 0,
              Message   => "Reserved field mismatch (1)");
      Assert (Condition => Store.Data (1).Data = 16#cafe_beef#,
              Message   => "Data mismatch (1)");

      Append_Entry (Store => Store,
                    Index => 16#beef_beef#,
                    Data  => 16#0123_4567#);
      Assert (Condition => Store.Next_Idx = 3,
              Message   => "Entry not appended (2)");
      Assert (Condition => Store.Data (2).Index = 16#beef_beef#,
              Message   => "Index mismatch (2)");
      Assert (Condition => Store.Data (2).Reserved = 0,
              Message   => "Reserved field mismatch (2)");
      Assert (Condition => Store.Data (2).Data = 16#0123_4567#,
              Message   => "Data mismatch (2)");
--  begin read only
   end Test_Append_Entry;
--  end read only


--  begin read only
   procedure Test_To_Stream (Gnattest_T : in out Test);
   procedure Test_To_Stream_45df0e (Gnattest_T : in out Test) renames Test_To_Stream;
--  id:2.2/45df0e704318cdb4/To_Stream/1/0/
   procedure Test_To_Stream (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Streams.Stream_Element_Array;

      Ref_Stream : constant Ada.Streams.Stream_Element_Array
        := (16#84#, 16#00#, 16#00#, 16#c0#, 16#00#, 16#00#, 16#00#, 16#00#,
            16#fe#, 16#ca#, 16#fe#, 16#ca#, 16#00#, 16#00#, 16#00#, 16#00#,
            16#00#, 16#2a#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
            16#78#, 16#56#, 16#34#, 16#12#, 16#00#, 16#00#, 16#00#, 16#00#);

      Store : MSR_Store_Type (Size => 2);
   begin
      Store.Next_Idx := 3;
      Store.Data (1) := (Index    => 16#c000_0084#,
                         Reserved => 0,
                         Data     => 16#cafe_cafe#);
      Store.Data (2) := (Index    => 16#2a00#,
                         Reserved => 0,
                         Data     => 16#1234_5678#);
      Assert (Condition => To_Stream (Store => Store) = Ref_Stream,
              Message   => "Stream mismatch");
--  begin read only
   end Test_To_Stream;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Msrstore.Tables.Test_Data.Tests;
