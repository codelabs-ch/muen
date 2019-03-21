--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bin_Split.Files.Test_Data.

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
package body Bin_Split.Files.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

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
               Section_Name : constant String
                 := Ada.Strings.Unbounded.To_String (SI.Name);
               Out_Filename : constant String
                 := Ada.Directories.Compose
                   (Containing_Directory => Dir,
                    Name                 => Section_Name);
            begin
               Write_Section
                 (Info             => SI,
                  Output_File_Name => Out_Filename,
                  Descriptor       => Fd);

               Assert
                 (Condition => Ada.Directories.Exists (Name => Out_Filename),
                  Message   => "Output not created");
               Assert
                 (Condition => Test_Utils.Equal_Files
                    (Filename1 => Out_Filename,
                     Filename2 => "obj/" & Section_Name
                       (Section_Name'First + 1 .. Section_Name'Last) & ".ref"),
                  Message   => "Written section '" & Section_Name
                  & "' mismatch");
            end;
         end if;
      end loop;
--  begin read only
   end Test_Write_Section;
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
end Bin_Split.Files.Test_Data.Tests;
