--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Bfd.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Mutools.Bfd.Test_Data.Tests is


--  begin read only
   procedure Test_Open (Gnattest_T : in out Test);
   procedure Test_Open_fd83b4 (Gnattest_T : in out Test) renames Test_Open;
--  id:2.2/fd83b40ddcdb3d67/Open/1/0/
   procedure Test_Open (Gnattest_T : in out Test) is
   --  mutools-bfd.ads:28:4:Open
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Nonexistent_File
      is
         Fd : Standard.Bfd.Files.File_Type;
      begin
         Open (Filename   => "nonexistent",
               Descriptor => Fd);
         Standard.Bfd.Files.Close (File => Fd);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : ELF_Error =>
            Assert (Condition =>
                       Ada.Exceptions.Exception_Message (X => E)
                       = "Unable to open file 'nonexistent'",
                    Message   => "Exception mismatch (1)");
      end Nonexistent_File;

      ----------------------------------------------------------------------

      procedure Wrong_Format
      is
         Fd : Standard.Bfd.Files.File_Type;
      begin
         Open (Filename   => "data/wrong_format",
               Descriptor => Fd);
         Standard.Bfd.Files.Close (File => Fd);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : ELF_Error =>
            Standard.Bfd.Files.Close (File => Fd);

            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "File 'data/wrong_format' is "
                      & "not a binary object file",
                    Message   => "Exception mismatch (2): "
                      & Ada.Exceptions.Exception_Message (X => E));
         when others =>
            Standard.Bfd.Files.Close (File => Fd);
            raise;
      end Wrong_Format;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Fd : Standard.Bfd.Files.File_Type;
      begin
         Open (Filename   => "data/test_binary",
               Descriptor => Fd);
         Assert
           (Condition => Standard.Bfd.Files.Is_Open (File => Fd),
            Message   => "File not open");
         Assert
           (Condition => Standard.Bfd.Files.Get_Filename (File => Fd)
                           = "data/test_binary",
            Message   => "Filename mismatch");
         Assert (Condition => Standard.Bfd.Files.Check_Format
                   (File   => Fd,
                    Expect => Standard.Bfd.Files.OBJECT),
                 Message   => "File is not a binary object");

         Standard.Bfd.Files.Close (File => Fd);

      exception
         when others =>
            Standard.Bfd.Files.Close (File => Fd);
            raise;
      end Positive_Test;
   begin
      Nonexistent_File;
      Wrong_Format;
      Positive_Test;
--  begin read only
   end Test_Open;
--  end read only

end Mutools.Bfd.Test_Data.Tests;
