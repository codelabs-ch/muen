--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Elfcheck.Bfd_Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Elfcheck.Bfd_Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Open (Gnattest_T : in out Test);
   procedure Test_Open_fd83b4 (Gnattest_T : in out Test) renames Test_Open;
--  id:2.2/fd83b40ddcdb3d67/Open/1/0/
   procedure Test_Open (Gnattest_T : in out Test) is
   --  elfcheck-bfd_utils.ads:29:4:Open
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
   --  elfcheck-bfd_utils.ads:35:4:Get_Section
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
   procedure Test_Validate_Size (Gnattest_T : in out Test);
   procedure Test_Validate_Size_f89357 (Gnattest_T : in out Test) renames Test_Validate_Size;
--  id:2.2/f89357f6993b636d/Validate_Size/1/0/
   procedure Test_Validate_Size (Gnattest_T : in out Test) is
   --  elfcheck-bfd_utils.ads:44:4:Validate_Size
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

end Elfcheck.Bfd_Utils.Test_Data.Tests;
