--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bin_Split.Files.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Bin_Split.Files.Test_Data.Tests is


--  begin read only
   procedure Test_Write_Section (Gnattest_T : in out Test);
   procedure Test_Write_Section_6af65c (Gnattest_T : in out Test) renames Test_Write_Section;
--  id:2.2/6af65ca91ddd2c4b/Write_Section/1/0/
   procedure Test_Write_Section (Gnattest_T : in out Test) is
   --  bin_split-files.ads:29:4:Write_Section
--  end read only
      
      pragma Unreferenced (Gnattest_T);

      Fd    : Bfd.Files.File_Type;
      Infos : constant SI_Array := Run.Section_Infos;
      Dir   : constant String   := "obj/test-out-dir";
   begin
      Open (Filename   => "data/test_binary",
            Descriptor => Fd);

      Ada.Directories.Create_Directory (New_Directory => Dir);

      for SI of Infos loop
         if SI.Write_To_File then
            declare
               Out_Filename : constant String
                 := Ada.Directories.Compose
                   (Containing_Directory => Dir,
                    Name                 =>
                      Ada.Strings.Unbounded.To_String (SI.Name));
            begin
               Write_Section
                 (Info             => SI,
                  Output_File_Name => Out_Filename,
                  Descriptor       => Fd);

               Assert
                 (Condition => Ada.Directories.Exists (Name => Out_Filename),
                  Message   => "Output not created");
            end;
         end if;
      end loop;
--  begin read only
   end Test_Write_Section;
--  end read only


--  begin read only
   procedure Test_Open (Gnattest_T : in out Test);
   procedure Test_Open_fd83b4 (Gnattest_T : in out Test) renames Test_Open;
--  id:2.2/fd83b40ddcdb3d67/Open/1/0/
   procedure Test_Open (Gnattest_T : in out Test) is
   --  bin_split-files.ads:37:4:Open
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
                 Message   => "Exception expected (1)");

      exception
         when E : Bin_Split_Error =>
            Assert (Condition =>
                       Ada.Exceptions.Exception_Message (X => E)
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
                 Message   => "Exception expected (2)");

      exception
         when E : Bin_Split_Error =>
            Bfd.Files.Close (File => Fd);
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "File 'data/wrong_format' is "
                      & "not a binary object file",
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
         Open (Filename   => "data/test_binary",
               Descriptor => Fd);
         Assert
           (Condition => Bfd.Files.Is_Open (File => Fd),
            Message   => "File not open");
         Assert
           (Condition => Bfd.Files.Get_Filename (File => Fd)
                           = "data/test_binary",
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

end Bin_Split.Files.Test_Data.Tests;
