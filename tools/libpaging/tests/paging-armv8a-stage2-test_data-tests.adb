--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paging.ARMv8a.Stage2.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
--  begin read only
--  end read only
package body Paging.ARMv8a.Stage2.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Deserialize_Level0_Entry (Gnattest_T : in out Test);
   procedure Test_Deserialize_Level0_Entry_92e8ac (Gnattest_T : in out Test) renames Test_Deserialize_Level0_Entry;
--  id:2.2/92e8ac65ade6b78b/Deserialize_Level0_Entry/1/0/
   procedure Test_Deserialize_Level0_Entry (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Streams.Stream_IO;

      File : File_Type;
   begin
      Ada.Streams.Stream_IO.Open
        (File => File,
         Mode => Ada.Streams.Stream_IO.In_File,
         Name => "data/armv8a_stage2_lvl0.ref");

      declare
         use type Interfaces.Unsigned_64;

         Lvl0_Entry : Entries.Table_Entry_Type;
      begin
         Deserialize_Level0_Entry (Stream      => Stream (File => File),
                                  Table_Entry => Lvl0_Entry);
         Close (File => File);

         Assert (Condition => Entries.Get_Dst_Address
                 (E => Lvl0_Entry) = 16#0009_c000#,
                 Message   => "Deserialized Level0 entry dst address mismatch");
         Assert (Condition => Entries.Is_Present (E => Lvl0_Entry),
                 Message   => "Deserialized Level0 entry not present");
         Assert (Condition => Entries.Is_Readable (E => Lvl0_Entry),
                 Message   => "Deserialized Level0 entry not readable");
         Assert (Condition => Entries.Is_Writable (E => Lvl0_Entry),
                 Message   => "Deserialized Level0 entry not writable");
         Assert (Condition => Entries.Is_Executable (E => Lvl0_Entry),
                 Message   => "Deserialized Level0 entry not executable");
         Assert (Condition => not Entries.Maps_Page (E => Lvl0_Entry),
                 Message   => "Deserialized Level0 entry maps page");

      exception
         when others =>
            if Is_Open (File => File) then
               Close (File => File);
            end if;
            raise;
      end;
--  begin read only
   end Test_Deserialize_Level0_Entry;
--  end read only


--  begin read only
   procedure Test_Deserialize_Level1_Entry (Gnattest_T : in out Test);
   procedure Test_Deserialize_Level1_Entry_f1701b (Gnattest_T : in out Test) renames Test_Deserialize_Level1_Entry;
--  id:2.2/f1701be991920039/Deserialize_Level1_Entry/1/0/
   procedure Test_Deserialize_Level1_Entry (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Streams.Stream_IO;

      File : File_Type;
   begin
      Ada.Streams.Stream_IO.Open
        (File => File,
         Mode => Ada.Streams.Stream_IO.In_File,
         Name => "data/armv8a_stage2_lvl1.ref");

      declare
         use type Interfaces.Unsigned_64;

         Lvl1_Entry : Entries.Table_Entry_Type;
      begin
         Deserialize_Level1_Entry (Stream      => Stream (File => File),
                                   Table_Entry => Lvl1_Entry);
         Close (File => File);

         Assert (Condition => Entries.Get_Dst_Address
                 (E => Lvl1_Entry) = 16#0009_e000#,
                 Message   => "Deserialized Level1 entry dst address mismatch");
         Assert (Condition => Entries.Is_Present (E => Lvl1_Entry),
                 Message   => "Deserialized Level1 entry not present");
         Assert (Condition => Entries.Is_Readable (E => Lvl1_Entry),
                 Message   => "Deserialized Level1 entry not readable");
         Assert (Condition => Entries.Is_Writable (E => Lvl1_Entry),
                 Message   => "Deserialized Level1 entry not writable");
         Assert (Condition => Entries.Is_Executable (E => Lvl1_Entry),
                 Message   => "Deserialized Level1 entry not executable");
         Assert (Condition => not Entries.Maps_Page (E => Lvl1_Entry),
                 Message   => "Deserialized Level1 entry maps page");

      exception
         when others =>
            if Is_Open (File => File) then
               Close (File => File);
            end if;
            raise;
      end;
--  begin read only
   end Test_Deserialize_Level1_Entry;
--  end read only


--  begin read only
   procedure Test_Deserialize_Level2_Entry (Gnattest_T : in out Test);
   procedure Test_Deserialize_Level2_Entry_b3523a (Gnattest_T : in out Test) renames Test_Deserialize_Level2_Entry;
--  id:2.2/b3523ab967315d20/Deserialize_Level2_Entry/1/0/
   procedure Test_Deserialize_Level2_Entry (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Streams.Stream_IO;

      File : File_Type;
   begin
      Ada.Streams.Stream_IO.Open
        (File => File,
         Mode => Ada.Streams.Stream_IO.In_File,
         Name => "data/armv8a_stage2_lvl2.ref");

      declare
         use type Interfaces.Unsigned_64;

         Lvl2_Entry : Entries.Table_Entry_Type;

         Ref_Base_Addr : constant Interfaces.Unsigned_64 := 16#1000_0000#;
      begin

         --  256 large page (2MB) mappings.

         for I in Interfaces.Unsigned_64 range 0 .. 255 loop
            Deserialize_Level2_Entry (Stream      => Stream (File => File),
                                      Table_Entry => Lvl2_Entry);
            Assert (Condition => Entries.Get_Dst_Address
                    (E => Lvl2_Entry) = Ref_Base_Addr + I * 16#0020_0000#,
                    Message   => "Deserialized Level2 entry (" & I'Img & " ) "
                    & "dst address mismatch");
            Assert (Condition => Entries.Is_Present (E => Lvl2_Entry),
                    Message   => "Deserialized Level2 entry (" & I'Img & " ) "
                    & "not present");
            Assert (Condition => Entries.Is_Readable (E => Lvl2_Entry),
                    Message   => "Deserialized Level2 entry (" & I'Img & " ) "
                    & "not readable");
            Assert (Condition => Entries.Is_Writable (E => Lvl2_Entry),
                    Message   => "Deserialized Level2 entry (" & I'Img & " ) "
                    & "not writable");
            Assert (Condition => Entries.Is_Executable (E => Lvl2_Entry),
                    Message   => "Deserialized Level2 entry (" & I'Img & " ) "
                    & " ot executable");
            Assert (Condition => Entries.Maps_Page (E => Lvl2_Entry),
                    Message   => "Deserialized Level2 entry (" & I'Img & " ) "
                    & "does not map page");
         end loop;

         --  Reference to level 3 PT.

         Deserialize_Level2_Entry (Stream      => Stream (File => File),
                                   Table_Entry => Lvl2_Entry);
         Assert (Condition => Entries.Get_Dst_Address
                 (E => Lvl2_Entry) = 16#000a_0000#,
                 Message   => "Deserialized Level2 entry ( 256 ) "
                 & "dst address mismatch");
         Assert (Condition => Entries.Is_Present (E => Lvl2_Entry),
                 Message   => "Deserialized Level2 entry ( 256 ) "
                 & "not present");
         Assert (Condition => Entries.Is_Readable (E => Lvl2_Entry),
                 Message   => "Deserialized Level2 entry ( 256 ) "
                 & "not readable");
         Assert (Condition => Entries.Is_Writable (E => Lvl2_Entry),
                 Message   => "Deserialized Level2 entry ( 256 )"
                 & "not writable");
         Assert (Condition => Entries.Is_Executable (E => Lvl2_Entry),
                 Message   => "Deserialized Level2 entry ( 256 ) "
                 & "not executable");
         Assert (Condition => not Entries.Maps_Page (E => Lvl2_Entry),
                 Message   => "Deserialized Level2 entry ( 256 ) "
                 & "maps page");

         --  7 entries are unmapped.

          for I in Interfaces.Unsigned_64 range 257 .. 263 loop
            Deserialize_Level2_Entry (Stream      => Stream (File => File),
                                      Table_Entry => Lvl2_Entry);
            Assert (Condition => Entries.Get_Dst_Address
                    (E => Lvl2_Entry) = 0,
                    Message   => "Deserialized Level2 entry (" & I'Img & " ) "
                    & "dst address mismatch");
            Assert (Condition => not Entries.Is_Present (E => Lvl2_Entry),
                    Message   => "Deserialized Level2 entry (" & I'Img & " ) "
                    & "present");
            Assert (Condition => not Entries.Is_Readable (E => Lvl2_Entry),
                    Message   => "Deserialized Level2 entry (" & I'Img & " ) "
                    & "readable");
            Assert (Condition => not Entries.Is_Writable (E => Lvl2_Entry),
                    Message   => "Deserialized Level2 entry (" & I'Img & " ) "
                    & "writable");
            Assert (Condition => not Entries.Is_Executable (E => Lvl2_Entry),
                    Message   => "Deserialized Level2 entry (" & I'Img & " ) "
                    & "executable");
            Assert (Condition => not Entries.Maps_Page (E => Lvl2_Entry),
                    Message   => "Deserialized Level2 entry (" & I'Img & " ) "
                    & "maps page");
         end loop;

         --  Second Reference to level 3 PT.

         Deserialize_Level2_Entry (Stream      => Stream (File => File),
                                   Table_Entry => Lvl2_Entry);
         Assert (Condition => Entries.Get_Dst_Address
                 (E => Lvl2_Entry) = 16#000a_2000#,
                 Message   => "Deserialized Level2 entry ( 264 ) "
                 & "dst address mismatch");
         Assert (Condition => Entries.Is_Present (E => Lvl2_Entry),
                 Message   => "Deserialized Level2 entry ( 264 ) "
                 & "not present");
         Assert (Condition => Entries.Is_Readable (E => Lvl2_Entry),
                 Message   => "Deserialized Level2 entry ( 264 ) "
                 & "not readable");
         Assert (Condition => Entries.Is_Writable (E => Lvl2_Entry),
                 Message   => "Deserialized Level2 entry ( 264 )"
                 & "not writable");
         Assert (Condition => Entries.Is_Executable (E => Lvl2_Entry),
                 Message   => "Deserialized Level2 entry ( 264 ) "
                 & "not executable");
         Assert (Condition => not Entries.Maps_Page (E => Lvl2_Entry),
                 Message   => "Deserialized Level2 entry ( 264 ) "
                 & "maps page");

         --  Remaining entries are unmapped.

         for I in Interfaces.Unsigned_64 range 265 .. 511 loop
            Deserialize_Level2_Entry (Stream      => Stream (File => File),
                                      Table_Entry => Lvl2_Entry);
            Assert (Condition => Entries.Get_Dst_Address
                    (E => Lvl2_Entry) = 0,
                    Message   => "Deserialized Level2 entry (" & I'Img & " ) "
                    & "dst address mismatch");
            Assert (Condition => not Entries.Is_Present (E => Lvl2_Entry),
                    Message   => "Deserialized Level2 entry (" & I'Img & " ) "
                    & "present");
            Assert (Condition => not Entries.Is_Readable (E => Lvl2_Entry),
                    Message   => "Deserialized Level2 entry (" & I'Img & " ) "
                    & "readable");
            Assert (Condition => not Entries.Is_Writable (E => Lvl2_Entry),
                    Message   => "Deserialized Level2 entry (" & I'Img & " ) "
                    & "writable");
            Assert (Condition => not Entries.Is_Executable (E => Lvl2_Entry),
                    Message   => "Deserialized Level2 entry (" & I'Img & " ) "
                    & "executable");
            Assert (Condition => not Entries.Maps_Page (E => Lvl2_Entry),
                    Message   => "Deserialized Level2 entry (" & I'Img & " ) "
                    & "maps page");
         end loop;

         Close (File => File);
      exception
         when others =>
            if Is_Open (File => File) then
               Close (File => File);
            end if;
            raise;
      end;
--  begin read only
   end Test_Deserialize_Level2_Entry;
--  end read only


--  begin read only
   procedure Test_Deserialize_Level3_Entry (Gnattest_T : in out Test);
   procedure Test_Deserialize_Level3_Entry_f7bbfd (Gnattest_T : in out Test) renames Test_Deserialize_Level3_Entry;
--  id:2.2/f7bbfded3025b349/Deserialize_Level3_Entry/1/0/
   procedure Test_Deserialize_Level3_Entry (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Streams.Stream_IO;

      File : File_Type;
   begin
      Ada.Streams.Stream_IO.Open
        (File => File,
         Mode => Ada.Streams.Stream_IO.In_File,
         Name => "data/armv8a_stage2_lvl3.ref");

      declare
         use type Interfaces.Unsigned_64;

         Lvl3_Entry : Entries.Table_Entry_Type;
      begin

         --  2 vGIC mappings

         Deserialize_Level3_Entry (Stream      => Stream (File => File),
                                   Table_Entry => Lvl3_Entry);
         Assert (Condition => Entries.Get_Dst_Address
                 (E => Lvl3_Entry) = 16#f906_0000#,
                 Message   => "Deserialized Level3 entry ( 0 ) "
                 & "dst address mismatch");
         Assert (Condition => Entries.Is_Present (E => Lvl3_Entry),
                 Message   => "Deserialized Level3 entry ( 0 ) "
                 & "not present");
         Assert (Condition => Entries.Is_Readable (E => Lvl3_Entry),
                 Message   => "Deserialized Level3 entry ( 0 ) "
                 & "not readable");
         Assert (Condition => Entries.Is_Writable (E => Lvl3_Entry),
                 Message   => "Deserialized Level3 entry ( 0 ) "
                 & "not writable");
         Assert (Condition => Entries.Is_Executable (E => Lvl3_Entry),
                 Message   => "Deserialized Level3 entry ( 0 ) "
                 & " ot executable");
         Assert (Condition => Entries.Maps_Page (E => Lvl3_Entry),
                 Message   => "Deserialized Level3 entry ( 0 ) "
                 & "does not map page");
         Assert (Condition => Entries.Get_Caching (E => Lvl3_Entry) = UC,
                 Message   => "Deserialized Level3 entry ( 0 ) "
                 & "caching type mismatch");

         Deserialize_Level3_Entry (Stream      => Stream (File => File),
                                   Table_Entry => Lvl3_Entry);
         Assert (Condition => Entries.Get_Dst_Address
                 (E => Lvl3_Entry) = 16#f907_0000#,
                 Message   => "Deserialized Level3 entry ( 1 ) "
                 & "dst address mismatch");
         Assert (Condition => Entries.Is_Present (E => Lvl3_Entry),
                 Message   => "Deserialized Level3 entry ( 1 ) "
                 & "not present");
         Assert (Condition => Entries.Is_Readable (E => Lvl3_Entry),
                 Message   => "Deserialized Level3 entry ( 1 ) "
                 & "not readable");
         Assert (Condition => Entries.Is_Writable (E => Lvl3_Entry),
                 Message   => "Deserialized Level3 entry ( 1 ) "
                 & "not writable");
         Assert (Condition => Entries.Is_Executable (E => Lvl3_Entry),
                 Message   => "Deserialized Level3 entry ( 1 ) "
                 & " ot executable");
         Assert (Condition => Entries.Maps_Page (E => Lvl3_Entry),
                 Message   => "Deserialized Level3 entry ( 1 ) "
                 & "does not map page");
         Assert (Condition => Entries.Get_Caching (E => Lvl3_Entry) = UC,
                 Message   => "Deserialized Level3 entry ( 1 ) "
                 & "caching type mismatch");

         --  Remaining entries are unmapped.

         for I in Interfaces.Unsigned_64 range 2 .. 511 loop
            Deserialize_Level3_Entry (Stream      => Stream (File => File),
                                      Table_Entry => Lvl3_Entry);
            Assert (Condition => Entries.Get_Dst_Address
                    (E => Lvl3_Entry) = 0,
                    Message   => "Deserialized Level3 entry (" & I'Img & " ) "
                    & "dst address mismatch");
            Assert (Condition => not Entries.Is_Present (E => Lvl3_Entry),
                    Message   => "Deserialized Level3 entry (" & I'Img & " ) "
                    & "present");
            Assert (Condition => not Entries.Is_Readable (E => Lvl3_Entry),
                    Message   => "Deserialized Level3 entry (" & I'Img & " ) "
                    & "readable");
            Assert (Condition => not Entries.Is_Writable (E => Lvl3_Entry),
                    Message   => "Deserialized Level3 entry (" & I'Img & " ) "
                    & "writable");
            Assert (Condition => not Entries.Is_Executable (E => Lvl3_Entry),
                    Message   => "Deserialized Level3 entry (" & I'Img & " ) "
                    & "executable");
            Assert (Condition => not Entries.Maps_Page (E => Lvl3_Entry),
                    Message   => "Deserialized Level3 entry (" & I'Img & " ) "
                    & "maps page");
         end loop;

         Close (File => File);
      exception
         when others =>
            if Is_Open (File => File) then
               Close (File => File);
            end if;
            raise;
      end;
--  begin read only
   end Test_Deserialize_Level3_Entry;
--  end read only


--  begin read only
   procedure Test_Cache_Mapping (Gnattest_T : in out Test);
   procedure Test_Cache_Mapping_0314dc (Gnattest_T : in out Test) renames Test_Cache_Mapping;
--  id:2.2/0314dcfe32966210/Cache_Mapping/1/0/
   procedure Test_Cache_Mapping (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      --  All device memory is uncached.

      for I in Interfaces.Unsigned_64 range 2#0000# .. 2#0011# loop
         Assert (Condition => Cache_Mapping
                 (ARMv8a_Stage2_Memory_Attrs => I) = UC,
                 Message   => "Device UC caching mismatch (" & I'Img & " )");
      end loop;

      Assert (Condition => Cache_Mapping
              (ARMv8a_Stage2_Memory_Attrs => 2#0101#) = UC,
              Message   => "UC caching mismatch");
      Assert (Condition => Cache_Mapping
              (ARMv8a_Stage2_Memory_Attrs => 2#1010#) = WT,
              Message   => "WT caching mismatch");
      Assert (Condition => Cache_Mapping
              (ARMv8a_Stage2_Memory_Attrs => 2#1111#) = WB,
              Message   => "WB caching mismatch");

      declare
         Dummy : Caching_Type;
      begin
         Dummy := Cache_Mapping (ARMv8a_Stage2_Memory_Attrs => 2#0100#);
         Assert (Condition => True,
                 Message   => "Exception expected");

      exception
         when E : Constraint_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid ARMv8a stage 2 memory type: 4",
                    Message   => "Exception message mismatch");
      end;
--  begin read only
   end Test_Cache_Mapping;
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
end Paging.ARMv8a.Stage2.Test_Data.Tests;
