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
   --  bin_split-files.ads:27:4:Write_Section
--  end read only
      
      pragma Unreferenced (Gnattest_T);

      Fd    : Bfd.Files.File_Type;
      Infos : constant SI_Array := Run.Section_Infos;
      Dir   : constant String   := "obj/test-out-dir";
   begin
      Mutools.Bfd.Open (Filename   => "data/test_binary",
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

end Bin_Split.Files.Test_Data.Tests;
