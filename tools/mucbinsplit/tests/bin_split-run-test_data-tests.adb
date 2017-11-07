--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bin_Split.Run.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Bin_Split.Run.Test_Data.Tests is


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_674d69 (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/674d6939a65f67a4/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  bin_split-run.ads:43:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);
   begin
      Run (Spec_File        => "data/test_cspec.xml",
           Binary_File      => "data/test_binary",
           Output_Spec_File => "cspec.xml",
           Output_Dir       => "obj/test-out-dir");
      
      Assert (Condition => Ada.Directories.Exists ("obj/test-out-dir/cspec.xml"),
              Message   => "Output component specification not created");
--  begin read only
   end Test_Run;
--  end read only


--  begin read only
   procedure Test_Check_Alignment (Gnattest_T : in out Test);
   procedure Test_Check_Alignment_8cdc1a (Gnattest_T : in out Test) renames Test_Check_Alignment;
--  id:2.2/8cdc1ad08a738ce5/Check_Alignment/1/0/
   procedure Test_Check_Alignment (Gnattest_T : in out Test) is
   --  bin_split-run.ads:105:4:Check_Alignment
--  end read only

      pragma Unreferenced (Gnattest_T);

      function Address_To_Iterator is
        new Ada.Unchecked_Conversion (Source => System.Address,
                                      Target => Bfd.Sections.Section_Iterator);

      Sec_Name : constant Interfaces.C.Strings.chars_ptr
        := Interfaces.C.Strings.New_String (".deadbeef");
      Sec : Bfd.Sections.Section
        := ( Opaque => Address_To_Iterator (Sec_Name'Address),
             others => <>);

      -----------------------------------------------------------------------
      
      procedure Positive
      is
      begin
         Sec.Vma  := 16#5000#;
         Sec.Lma  := 16#5000#;
         Sec.Size := 16#62f1#;

         Check_Alignment (Section => Sec);
      exception
         when E : Bin_Split_Error =>
            Assert (Condition => False,
                    Message   => "Section is wrongly classified as unaligned");
      end Positive;

      -----------------------------------------------------------------------

      procedure Negative
      is
      begin
         Sec.Vma  := 16#5001#;
         Sec.Lma  := 16#5001#;
         Sec.Size := 16#62f1#;

         Check_Alignment (Section => Sec);

         Assert (Condition => False,
                 Message   => "Exception expected (1)");
      exception
         when E : Bin_Split_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Section '.deadbeef' is not page-aligned",
                    Message   => "Exception mismatch");
      end Negative;

      -----------------------------------------------------------------------

      procedure Other_Negative
      is
      begin
         Sec.Vma  := 16#5000#;
         Sec.Lma  := 16#5003#;
         Sec.Size := 16#62f1#;

         Check_Alignment (Section => Sec);

         Assert (Condition => False,
                 Message   => "Exception expected (2)");
      exception
         when E : Bin_Split_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "LMA address of section '.deadbeef' is not equal to its VMA address",
                    Message   => "Exception mismatch");
      end Other_Negative;
      
   begin
      Positive;
      Negative;
      Other_Negative;
--  begin read only
   end Test_Check_Alignment;
--  end read only


--  begin read only
   procedure Test_Check_Section_Names (Gnattest_T : in out Test);
   procedure Test_Check_Section_Names_e4abb0 (Gnattest_T : in out Test) renames Test_Check_Section_Names;
--  id:2.2/e4abb0415956f991/Check_Section_Names/1/0/
   procedure Test_Check_Section_Names (Gnattest_T : in out Test) is
   --  bin_split-run.ads:110:4:Check_Section_Names
--  end read only

      pragma Unreferenced (Gnattest_T);

      -----------------------------------------------------------------------

      procedure All_Sections_Ok
      is
         Fd : Bfd.Files.File_Type;
      begin
         Bin_Split.Files.Open (Filename   => "data/test_binary",
                               Descriptor => Fd);

         Check_Section_Names (Descriptor => Fd);
      end All_Sections_Ok;

      -----------------------------------------------------------------------

      procedure Wrong_Section_Name
      is
         Fd : Bfd.Files.File_Type;
      begin
         Bin_Split.Files.Open (Filename   => "data/wrong_name",
                               Descriptor => Fd);

         Check_Section_Names (Descriptor => Fd);

         Assert (Condition => False,
                 Message => "Exception expected");

      exception
         when E : Bin_Split_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Unexpected section name '.code'",
                    Message   => "Exception mismatch");
      end Wrong_Section_Name;
   begin
      All_Sections_Ok;
      Wrong_Section_Name;
--  begin read only
   end Test_Check_Section_Names;
--  end read only


--  begin read only
   procedure Test_Check_Flags (Gnattest_T : in out Test);
   procedure Test_Check_Flags_8371d7 (Gnattest_T : in out Test) renames Test_Check_Flags;
--  id:2.2/8371d72dcbf3486f/Check_Flags/1/0/
   procedure Test_Check_Flags (Gnattest_T : in out Test) is
   --  bin_split-run.ads:116:4:Check_Flags
--  end read only

      pragma Unreferenced (Gnattest_T);

      -----------------------------------------------------------------------

      procedure Text_Section_Ok
      is
         package BC renames Bfd.Constants;

         use type Bfd.Section_Flags;

         Fd : Bfd.Files.File_Type;
      begin
         Bin_Split.Files.Open (Filename   => "data/test_binary",
                               Descriptor => Fd);

         Check_Flags
           (Sec_Info   =>  (Name => Ada.Strings.Unbounded.To_Unbounded_String
                              (".text"),
                            Write_To_File => True,
                            Flags         =>
                              BC.SEC_HAS_CONTENTS or BC.SEC_ALLOC or BC.SEC_LOAD
                                or BC.SEC_READONLY or BC.SEC_CODE,
                            Fill          => False,
                            Writable      => False,
                            Executable    => True),
            Descriptor => Fd);

      exception
         when others =>
            Bfd.Files.Close (File => Fd);
            raise;
      end Text_Section_Ok;

      -----------------------------------------------------------------------

      procedure Text_Section_Writable
      is
         package BC renames Bfd.Constants;

         use type Bfd.Section_Flags;

         Fd : Bfd.Files.File_Type;
      begin
         Bin_Split.Files.Open (Filename   => "data/wrong_flags",
                               Descriptor => Fd);

         Check_Flags
           (Sec_Info   =>  (Name => Ada.Strings.Unbounded.To_Unbounded_String
                              (".text"),
                            Write_To_File => True,
                            Flags         =>
                              BC.SEC_HAS_CONTENTS or BC.SEC_ALLOC or BC.SEC_LOAD
                                or BC.SEC_READONLY or BC.SEC_CODE,
                            Fill          => False,
                            Writable      => False,
                            Executable    => True),
            Descriptor => Fd);

      exception
         when E : Bin_Split_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Unexpected flags for section '.text': 16#011b# /= 16#0113#",
                    Message   => "Exception mismatch");

      end Text_Section_Writable;
   begin
      Text_Section_Ok;
      Text_Section_Writable;
--  begin read only
   end Test_Check_Flags;
--  end read only


--  begin read only
   procedure Test_Is_Valid_Section (Gnattest_T : in out Test);
   procedure Test_Is_Valid_Section_9930bc (Gnattest_T : in out Test) renames Test_Is_Valid_Section;
--  id:2.2/9930bc1a48e5d507/Is_Valid_Section/1/0/
   procedure Test_Is_Valid_Section (Gnattest_T : in out Test) is
   --  bin_split-run.ads:121:4:Is_Valid_Section
--  end read only

      pragma Unreferenced (Gnattest_T);

      Infos : constant SI_Array
        := ((Name   => Ada.Strings.Unbounded.To_Unbounded_String (".text"),
             others => <>),
            (Name   => Ada.Strings.Unbounded.To_Unbounded_String (".foo"),
             others => <>));
   begin
      Assert (Condition => Is_Valid_Section (Section_Name => ".text", Section_Infos => Infos),
              Message   => "Valid section not recognized");
      
      Assert (Condition => not Is_Valid_Section (Section_Name => ".bar", Section_Infos => Infos),
              Message   => "Invalid section not recognized");
--  begin read only
   end Test_Is_Valid_Section;
--  end read only

end Bin_Split.Run.Test_Data.Tests;
