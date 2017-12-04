--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Elfcheck.Bfd_Utils.Test_Data.

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
package body Elfcheck.Bfd_Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Open (Gnattest_T : in out Test);
   procedure Test_Open_fd83b4 (Gnattest_T : in out Test) renames Test_Open;
--  id:2.2/fd83b40ddcdb3d67/Open/1/0/
   procedure Test_Open (Gnattest_T : in out Test) is
   --  elfcheck-bfd_utils.ads:31:4:Open
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Nonexistent_File
      is
         Fd : Bfd.Files.File_Type;
      begin
         Open (Filename   => "nonexistent",
               Descriptor => Fd);
         Bfd.Files.Close (File => Fd);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : ELF_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unable to open file 'nonexistent'",
                    Message   => "Exception mismatch");
      end Nonexistent_File;

      ----------------------------------------------------------------------

      procedure Wrong_Format
      is
         Fd : Bfd.Files.File_Type;
      begin
         Open (Filename   => "data/wrong_format",
               Descriptor => Fd);
         Bfd.Files.Close (File => Fd);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : ELF_Error =>
            Bfd.Files.Close (File => Fd);
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "File 'data/wrong_format' is not a binary object file",
                    Message   => "Exception mismatch");
         when others =>
            Bfd.Files.Close (File => Fd);
            raise;
      end Wrong_Format;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Fd : Bfd.Files.File_Type;
      begin
         Open (Filename   => "data/binary",
               Descriptor => Fd);
         Assert (Condition => Bfd.Files.Is_Open (File => Fd),
                 Message   => "File not open");
         Assert (Condition => Bfd.Files.Get_Filename (File => Fd)
                 = "data/binary",
                 Message   => "Filename mismatch");
         Assert (Condition => Bfd.Files.Check_Format
                 (File   => Fd,
                  Expect => Bfd.Files.OBJECT),
                 Message   => "File is not a binary object");

         Bfd.Files.Close (File => Fd);

      exception
         when others =>
            Bfd.Files.Close (File => Fd);
            raise;
      end Positive_Test;

   begin
      Nonexistent_File;
      Wrong_Format;
      Positive_Test;
--  begin read only
   end Test_Open;
--  end read only


--  begin read only
   procedure Test_Get_Section (Gnattest_T : in out Test);
   procedure Test_Get_Section_eabc3e (Gnattest_T : in out Test) renames Test_Get_Section;
--  id:2.2/eabc3e5123f1b3fe/Get_Section/1/0/
   procedure Test_Get_Section (Gnattest_T : in out Test) is
   --  elfcheck-bfd_utils.ads:37:4:Get_Section
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Nonexistent_Section
      is
         Fd : Bfd.Files.File_Type;
         S  : Bfd.Sections.Section;
      begin
         Open (Filename   => "data/binary",
               Descriptor => Fd);
         S := Get_Section (Descriptor => Fd,
                           Name       => "nonexistent");
         Bfd.Files.Close (File => Fd);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : ELF_Error =>
            Bfd.Files.Close (File => Fd);
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Section 'nonexistent' not found",
                    Message   => "Exception mismatch");
         when others =>
            Bfd.Files.Close (File => Fd);
            raise;
      end Nonexistent_Section;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         use type Bfd.Size_Type;

         Fd : Bfd.Files.File_Type;
         S  : Bfd.Sections.Section;
      begin
         Open (Filename   => "data/binary",
               Descriptor => Fd);
         S := Get_Section (Descriptor => Fd,
                           Name       => ".text");
         Bfd.Files.Close (File => Fd);

         Assert (Condition => S.Size > 0,
                 Message   => "Section size is zero");

      exception
         when others =>
            Bfd.Files.Close (File => Fd);
            raise;
      end Positive_Test;

   begin
      Nonexistent_Section;
      Positive_Test;
--  begin read only
   end Test_Get_Section;
--  end read only


--  begin read only
   procedure Test_Check_Section (Gnattest_T : in out Test);
   procedure Test_Check_Section_490406 (Gnattest_T : in out Test) renames Test_Check_Section;
--  id:2.2/490406d12bf9e77a/Check_Section/1/0/
   procedure Test_Check_Section (Gnattest_T : in out Test) is
   --  elfcheck-bfd_utils.ads:45:4:Check_Section
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Nonexistent_Physical_Region
      is
         Policy : Muxml.XML_Data_Type;
         Fd     : Bfd.Files.File_Type;
         S      : Bfd.Sections.Section;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Open (Filename   => "data/binary",
               Descriptor => Fd);
         S := Get_Section (Descriptor => Fd,
                           Name       => ".text");
         Check_Section (Policy      => Policy,
                        Region_Name => "nonexistent",
                        Section     => S);
         Bfd.Files.Close (File => Fd);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : ELF_Error =>
            Bfd.Files.Close (File => Fd);
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical memory region 'nonexistent' not found in "
                    & "policy",
                    Message   => "Exception mismatch (1)");
         when others =>
            Bfd.Files.Close (File => Fd);
            raise;
      end Nonexistent_Physical_Region;

      ----------------------------------------------------------------------

      procedure Nonexistent_Virtual_Region
      is
         Policy : Muxml.XML_Data_Type;
         Fd     : Bfd.Files.File_Type;
         S      : Bfd.Sections.Section;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Open (Filename   => "data/binary",
               Descriptor => Fd);
         S := Get_Section (Descriptor => Fd,
                           Name       => ".text");

         Muxml.Utils.Set_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/kernel/memory/cpu/memory"
            & "[@physical='kernel_text']",
            Name  => "physical",
            Value => "nonexistent");
         Check_Section (Policy      => Policy,
                        Region_Name => "kernel_text",
                        Section     => S);
         Bfd.Files.Close (File => Fd);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : ELF_Error =>
            Bfd.Files.Close (File => Fd);
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Virtual memory region 'kernel_text' not found in policy",
                    Message   => "Exception mismatch (2)");
         when others =>
            Bfd.Files.Close (File => Fd);
            raise;
      end Nonexistent_Virtual_Region;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Policy : Muxml.XML_Data_Type;
         Fd     : Bfd.Files.File_Type;
         S      : Bfd.Sections.Section;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Open (Filename   => "data/binary",
               Descriptor => Fd);
         S := Get_Section (Descriptor => Fd,
                           Name       => ".text");
         Bfd.Files.Close (File => Fd);
         Check_Section (Policy      => Policy,
                        Region_Name => "kernel_text",
                        Section     => S);

      exception
         when others =>
            Bfd.Files.Close (File => Fd);
            raise;
      end Positive_Test;

   begin
      Nonexistent_Physical_Region;
      Nonexistent_Virtual_Region;
      Positive_Test;
--  begin read only
   end Test_Check_Section;
--  end read only


--  begin read only
   procedure Test_Validate_Size (Gnattest_T : in out Test);
   procedure Test_Validate_Size_f89357 (Gnattest_T : in out Test) renames Test_Validate_Size;
--  id:2.2/f89357f6993b636d/Validate_Size/1/0/
   procedure Test_Validate_Size (Gnattest_T : in out Test) is
   --  elfcheck-bfd_utils.ads:55:4:Validate_Size
--  end read only

      pragma Unreferenced (Gnattest_T);

      S : Bfd.Sections.Section := (Size => 16#2000#, others => <>);
   begin
      Validate_Size (Section      => S,
                     Section_Name => ".text",
                     Region_Name  => "kernel_text",
                     Size         => 16#1000#);
      Assert (Condition => False,
              Message   => "Exception expected");

   exception
      when E : ELF_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Size of physical memory region 'kernel_text' too small to "
                 & "store '.text' section: 16#1000# < 16#2000#",
                 Message   => "Exception mismatch");
--  begin read only
   end Test_Validate_Size;
--  end read only


--  begin read only
   procedure Test_Validate_VMA (Gnattest_T : in out Test);
   procedure Test_Validate_VMA_0341b9 (Gnattest_T : in out Test) renames Test_Validate_VMA;
--  id:2.2/0341b9398452ad8b/Validate_VMA/1/0/
   procedure Test_Validate_VMA (Gnattest_T : in out Test) is
   --  elfcheck-bfd_utils.ads:63:4:Validate_VMA
--  end read only

      pragma Unreferenced (Gnattest_T);

      S : Bfd.Sections.Section := (Vma => 16#1000#, others => <>);
   begin
      Validate_VMA (Section      => S,
                    Section_Name => ".text",
                    Region_Name  => "kernel_text",
                    Address      => 16#2000#);
      Assert (Condition => False,
              Message   => "Exception expected");

   exception
      when E : ELF_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Memory region 'kernel_text' virtual address 16#2000# not "
                 & "equal section '.text' VMA 16#1000#",
                 Message   => "Exception mismatch");
--  begin read only
   end Test_Validate_VMA;
--  end read only


--  begin read only
   procedure Test_Validate_LMA_In_Region (Gnattest_T : in out Test);
   procedure Test_Validate_LMA_In_Region_9a4ce6 (Gnattest_T : in out Test) renames Test_Validate_LMA_In_Region;
--  id:2.2/9a4ce6afc448ffbc/Validate_LMA_In_Region/1/0/
   procedure Test_Validate_LMA_In_Region (Gnattest_T : in out Test) is
   --  elfcheck-bfd_utils.ads:71:4:Validate_LMA_In_Region
--  end read only

      pragma Unreferenced (Gnattest_T);

      S : Bfd.Sections.Section :=
        (Lma    => 16#3000#,
         Size   => 16#2001#,
         others => <>);
   begin

      --  Positive tests.

      Validate_LMA_In_Region
        (Section      => S,
         Section_Name => ".trampoline",
         Region_Name  => "kernel_text",
         Address      => 16#2000#,
         Size         => 16#6000#);
      Validate_LMA_In_Region
        (Section      => S,
         Section_Name => ".trampoline",
         Region_Name  => "kernel_text",
         Address      => 16#3000#,
         Size         => 16#3000#);

      begin
         Validate_LMA_In_Region
           (Section      => S,
            Section_Name => ".text",
            Region_Name  => "kernel_text",
            Address      => 16#1000#,
            Size         => 16#4000#);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : ELF_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Section '.text' from 16#3000# .. 16#5000# not within "
                    & "physical memory region 'kernel_text' from 16#1000# .. 16#4fff#",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Validate_LMA_In_Region;
--  end read only


--  begin read only
   procedure Test_Validate_Permission (Gnattest_T : in out Test);
   procedure Test_Validate_Permission_619f4f (Gnattest_T : in out Test) renames Test_Validate_Permission;
--  id:2.2/619f4fd4744b4a1a/Validate_Permission/1/0/
   procedure Test_Validate_Permission (Gnattest_T : in out Test) is
   --  elfcheck-bfd_utils.ads:80:4:Validate_Permission
--  end read only

      pragma Unreferenced (Gnattest_T);

      S : Bfd.Sections.Section := (Flags => 0, others => <>);
   begin
      Validate_Permission (Section      => S,
                           Section_Name => ".text",
                           Region_Name  => "kernel_text",
                           Read_Only    => True);
      Assert (Condition => False,
              Message   => "Exception expected");

   exception
      when E : ELF_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Memory region 'kernel_text' is read-only but section "
                 & "'.text' READONLY flag is not set",
                 Message   => "Exception mismatch");
--  begin read only
   end Test_Validate_Permission;
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
end Elfcheck.Bfd_Utils.Test_Data.Tests;
