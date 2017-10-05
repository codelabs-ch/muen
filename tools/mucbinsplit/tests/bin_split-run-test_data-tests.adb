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
   procedure Test_Run_e84213 (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/e84213e130018c54/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  bin_split-run.ads:39:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Run;
--  end read only


--  begin read only
   procedure Test_Check_Alignment (Gnattest_T : in out Test);
   procedure Test_Check_Alignment_e0d5ff (Gnattest_T : in out Test) renames Test_Check_Alignment;
--  id:2.2/e0d5ff13d7bb5b80/Check_Alignment/1/0/
   procedure Test_Check_Alignment (Gnattest_T : in out Test) is
   --  bin_split-run.ads:48:4:Check_Alignment
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
   --  bin_split-run.ads:53:4:Check_Section_Names
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Check_Section_Names;
--  end read only


--  begin read only
   procedure Test_Check_Flags (Gnattest_T : in out Test);
   procedure Test_Check_Flags_1421c2 (Gnattest_T : in out Test) renames Test_Check_Flags;
--  id:2.2/1421c226c2e270a9/Check_Flags/1/0/
   procedure Test_Check_Flags (Gnattest_T : in out Test) is
   --  bin_split-run.ads:60:4:Check_Flags
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Check_Flags;
--  end read only


--  begin read only
   procedure Test_Get_Compound_Section_Infos (Gnattest_T : in out Test);
   procedure Test_Get_Compound_Section_Infos_a96037 (Gnattest_T : in out Test) renames Test_Get_Compound_Section_Infos;
--  id:2.2/a960377bd2b09f4a/Get_Compound_Section_Infos/1/0/
   procedure Test_Get_Compound_Section_Infos (Gnattest_T : in out Test) is
   --  bin_split-run.ads:70:4:Get_Compound_Section_Infos
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      --  I do not think there is anything to test here.
      null;

--  begin read only
   end Test_Get_Compound_Section_Infos;
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

end Bin_Split.Run.Test_Data.Tests;
