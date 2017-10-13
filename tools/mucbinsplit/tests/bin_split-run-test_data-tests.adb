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
   --  bin_split-run.ads:41:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);


   begin

      Run (Spec_File        => "test_data/test_cspec.xml",
           Binary_File      => "test_data/test_binary",
           Output_Spec_File => "cspec.xml",
           Output_Dir       => "test-out-dir");

--  begin read only
   end Test_Run;
--  end read only


--  begin read only
   procedure Test_Check_Alignment (Gnattest_T : in out Test);
   procedure Test_Check_Alignment_e0d5ff (Gnattest_T : in out Test) renames Test_Check_Alignment;
--  id:2.2/e0d5ff13d7bb5b80/Check_Alignment/1/0/
   procedure Test_Check_Alignment (Gnattest_T : in out Test) is
   --  bin_split-run.ads:52:4:Check_Alignment
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Positive
      is

         function Address_To_Iterator is
           new Ada.Unchecked_Conversion (Source => System.Address,
                                         Target => Bfd.Sections.Section_Iterator);

         function Bfd_Section_To_Section is
           new Ada.Unchecked_Conversion (Source => Bfd.Sections.Section,
                                         Target => Bin_Split.Binary.Sections.Section);

         Sec_Name : constant Interfaces.C.Strings.chars_ptr
           := Interfaces.C.Strings.New_String (".deadbeef");

         Sec : constant Bin_Split.Binary.Sections.Section
           := Bfd_Section_To_Section
             (Bfd.Sections.Section'(Vma    => 16#5000#,
                                    Lma    => 16#5000#,
                                    Size   => 16#62f1#,
                                    Flags  => Bfd.Sections.SEC_CODE,
                                    Opaque => Address_To_Iterator (Sec_Name'Address)));

      begin

         Check_Alignment (Section => Sec);

      exception
         when E : Bin_Split_Error =>
            Assert (Condition => True,
                    Message   =>"Section is wrongly classified as unaligned");
      end Positive;

      -----------------------------------------------------------------------

      procedure Negative
      is

         function Address_To_Iterator is
           new Ada.Unchecked_Conversion (Source => System.Address,
                                         Target => Bfd.Sections.Section_Iterator);

         function Bfd_Section_To_Section is
           new Ada.Unchecked_Conversion (Source => Bfd.Sections.Section,
                                         Target => Bin_Split.Binary.Sections.Section);

         Sec_Name : constant Interfaces.C.Strings.chars_ptr
           := Interfaces.C.Strings.New_String (".deadbeef");

         Sec : constant Bin_Split.Binary.Sections.Section
           := Bfd_Section_To_Section
             (Bfd.Sections.Section'(Vma    => 16#5001#,
                                    Lma    => 16#5001#,
                                    Size   => 16#62f1#,
                                    Flags  => Bfd.Sections.SEC_CODE,
                                    Opaque => Address_To_Iterator (Sec_Name'Address)));

      begin

         Check_Alignment (Section => Sec);

         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Bin_Split_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Section '.deadbeef' is not page-aligned.",
                    Message   => "Exception mismatch");
      end Negative;

      -----------------------------------------------------------------------

      procedure Other_Negative
      is

         function Address_To_Iterator is
           new Ada.Unchecked_Conversion (Source => System.Address,
                                         Target => Bfd.Sections.Section_Iterator);

         function Bfd_Section_To_Section is
           new Ada.Unchecked_Conversion (Source => Bfd.Sections.Section,
                                         Target => Bin_Split.Binary.Sections.Section);

         Sec_Name : constant Interfaces.C.Strings.chars_ptr
           := Interfaces.C.Strings.New_String (".deadbeef");

         Sec : constant Bin_Split.Binary.Sections.Section
           := Bfd_Section_To_Section
             (Bfd.Sections.Section'(Vma    => 16#5000#,
                                    Lma    => 16#5003#,
                                    Size   => 16#62f1#,
                                    Flags  => Bfd.Sections.SEC_CODE,
                                    Opaque => Address_To_Iterator (Sec_Name'Address)));

      begin

         Check_Alignment (Section => Sec);

         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Bin_Split_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "LMA address of section '.deadbeef' is not equal to its VMA address.",
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
   procedure Test_Check_Section_Names_e5ddfa (Gnattest_T : in out Test) renames Test_Check_Section_Names;
--  id:2.2/e5ddfa901e1ea0ab/Check_Section_Names/1/0/
   procedure Test_Check_Section_Names (Gnattest_T : in out Test) is
   --  bin_split-run.ads:57:4:Check_Section_Names
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure All_Sections_Ok is
         Fd : Bin_Split.Binary.Files.File_Type;
      begin
         Bin_Split.Binary.Files.Open (Filename   => "test_data/test_binary",
                                      Descriptor => Fd);

         Check_Section_Names (Descriptor => Fd);

      end All_Sections_Ok;

      -----------------------------------------------------------------------

      procedure Wrong_Section_Name is
         Fd : Bin_Split.Binary.Files.File_Type;
      begin
         Bin_Split.Binary.Files.Open (Filename   => "test_data/wrong_name",
                                      Descriptor => Fd);

         Check_Section_Names (Descriptor => Fd);

         Assert (Condition => False,
                 Message => "Exception expected");

      exception
         when E : Bin_Split_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Unexpected section name '.code'.",
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
   procedure Test_Check_Flags_1421c2 (Gnattest_T : in out Test) renames Test_Check_Flags;
--  id:2.2/1421c226c2e270a9/Check_Flags/1/0/
   procedure Test_Check_Flags (Gnattest_T : in out Test) is
   --  bin_split-run.ads:64:4:Check_Flags
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Text_Section_Ok is
         package B renames Bin_Split.Binary;

         use type B.Section_Flags;

         Fd : Bin_Split.Binary.Files.File_Type;
      begin
         Bin_Split.Binary.Files.Open (Filename   => "test_data/test_binary",
                                      Descriptor => Fd);

         Check_Flags
           (Sec_Info   =>  (Name => Ada.Strings.Unbounded.To_Unbounded_String
                              (".text"),
                            Write_To_File => True,
                            Flags         =>
                              B.Contents or B.Alloc or B.Load or B.Readonly or B.Code,
                            Fill          => False,
                            Writable      => False,
                            Executable    => True),
            Descriptor => Fd);

      exception
         when others =>
            Bin_Split.Binary.Files.Close (File => Fd);
            raise;
      end Text_Section_Ok;

      procedure Text_Section_Writable is
         package B renames Bin_Split.Binary;

         use type B.Section_Flags;

         Fd : Bin_Split.Binary.Files.File_Type;
      begin
         Bin_Split.Binary.Files.Open (Filename   => "test_data/wrong_flags",
                                      Descriptor => Fd);

         Check_Flags
           (Sec_Info   =>  (Name => Ada.Strings.Unbounded.To_Unbounded_String
                              (".text"),
                            Write_To_File => True,
                            Flags         =>
                              B.Contents or B.Alloc or B.Load or B.Readonly or B.Code,
                            Fill          => False,
                            Writable      => False,
                            Executable    => True),
            Descriptor => Fd);

      exception
         when E : Bin_Split_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Unexpected flags for section '.text': 16#011b# /= 16#0113#.",
                    Message   => "Exception mismatch");

      end Text_Section_Writable;

   begin

      Text_Section_Ok;
      Text_Section_Writable;

--  begin read only
   end Test_Check_Flags;
--  end read only


--  begin read only
   procedure Test_Get_Section_Infos (Gnattest_T : in out Test);
   procedure Test_Get_Section_Infos_cb511a (Gnattest_T : in out Test) renames Test_Get_Section_Infos;
--  id:2.2/cb511a2281a5d347/Get_Section_Infos/1/0/
   procedure Test_Get_Section_Infos (Gnattest_T : in out Test) is
   --  bin_split-run.ads:71:4:Get_Section_Infos
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Bin_Split.Binary.Section_Flags;

      Infos : constant Types.SI_Array := Get_Section_Infos;

   begin

      for SI of Infos loop
            Assert
              (Condition => not (SI.Writable and SI.Executable),
               Message   => "Section is writable and exectuable");
      end loop;

--  begin read only
   end Test_Get_Section_Infos;
--  end read only


--  begin read only
   --  procedure Test_Add_Entry (Gnattest_T : in out Test_);
   --  procedure Test_Add_Entry_94b898 (Gnattest_T : in out Test_) renames Test_Add_Entry;
--  id:2.2/94b898f97f2e3130/Add_Entry/1/1/
   --  procedure Test_Add_Entry (Gnattest_T : in out Test_) is
--  end read only
--
--        pragma Unreferenced (Gnattest_T);
--
--     begin
--
--        AUnit.Assertions.Assert
--          (Gnattest_Generated.Default_Assert_Value,
--           "Test not implemented.");
--
--  begin read only
   --  end Test_Add_Entry;
--  end read only


--  begin read only
   --  procedure Test_Get_Compound_Section_Infos (Gnattest_T : in out Test_);
   --  procedure Test_Get_Compound_Section_Infos_a96037 (Gnattest_T : in out Test_) renames Test_Get_Compound_Section_Infos;
--  id:2.2/a960377bd2b09f4a/Get_Compound_Section_Infos/1/1/
   --  procedure Test_Get_Compound_Section_Infos (Gnattest_T : in out Test_) is
--  end read only
--
--        pragma Unreferenced (Gnattest_T);
--
--     begin
--
--        --  I do not think there is anything to test here.
--        null;
--
--  begin read only
   --  end Test_Get_Compound_Section_Infos;
--  end read only

end Bin_Split.Run.Test_Data.Tests;
