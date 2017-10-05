--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bin_Split.Binary.Files.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Bin_Split.Binary.Files.Test_Data.Tests is


--  begin read only
   procedure Test_Write_Compound_Section (Gnattest_T : in out Test);
   procedure Test_Write_Compound_Section_fd921e (Gnattest_T : in out Test) renames Test_Write_Compound_Section;
--  id:2.2/fd921eb414f5c1d7/Write_Compound_Section/1/0/
   procedure Test_Write_Compound_Section (Gnattest_T : in out Test) is
   --  bin_split-binary-files.ads:31:4:Write_Compound_Section
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Write_Compound_Section;
--  end read only


--  begin read only
   procedure Test_Open (Gnattest_T : in out Test);
   procedure Test_Open_24e2a7 (Gnattest_T : in out Test) renames Test_Open;
--  id:2.2/24e2a71d5e71a2c3/Open/1/0/
   procedure Test_Open (Gnattest_T : in out Test) is
   --  bin_split-binary-files.ads:40:4:Open
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Nonexistent_File
      is
         Fd : File_Type;
      begin
         Open (Filename   => "nonexistent",
               Descriptor => Fd);
         Bfd.Files.Close (File => Bfd.Files.File_Type (Fd));
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Bin_Split_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unable to open file 'nonexistent'",
                    Message   => "Exception mismatch");
      end Nonexistent_File;

      ----------------------------------------------------------------------

      procedure Wrong_Format
      is
         Fd : File_Type;
      begin
         Open (Filename   => "test_data/wrong_format",
               Descriptor => Fd);
         Bfd.Files.Close (File => Bfd.Files.File_Type (Fd));
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Bin_Split_Error =>
            Bfd.Files.Close (File => Bfd.Files.File_Type (Fd));
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "File 'test_data/wrong_format' is "
                      & "not a binary object file",
                    Message   => "Exception mismatch");
         when others =>
            Bfd.Files.Close (File => Bfd.Files.File_Type (Fd));
            raise;
      end Wrong_Format;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Fd : File_Type;
      begin
         Open (Filename   => "test_data/test_binary",
               Descriptor => Fd);
         Assert
           (Condition => Bfd.Files.Is_Open (File => Bfd.Files.File_Type (Fd)),
            Message   => "File not open");
         Assert
           (Condition => Bfd.Files.Get_Filename (File => Bfd.Files.File_Type (Fd))
                           = "test_data/test_binary",
            Message   => "Filename mismatch");
         Assert (Condition => Bfd.Files.Check_Format
                   (File   => Bfd.Files.File_Type (Fd),
                    Expect => Bfd.Files.OBJECT),
                 Message   => "File is not a binary object");

         Bfd.Files.Close (File => Bfd.Files.File_Type (Fd));

      exception
         when others =>
            Bfd.Files.Close (File => Bfd.Files.File_Type (Fd));
            raise;
      end Positive_Test;

   begin
      Nonexistent_File;
      Wrong_Format;
      Positive_Test;

--  begin read only
   end Test_Open;
--  end read only

end Bin_Split.Binary.Files.Test_Data.Tests;
