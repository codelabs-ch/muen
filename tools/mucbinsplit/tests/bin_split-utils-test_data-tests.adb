--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bin_Split.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Bin_Split.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Round_To_Page (Gnattest_T : in out Test);
   procedure Test_Round_To_Page_db1a50 (Gnattest_T : in out Test) renames Test_Round_To_Page;
--  id:2.2/db1a505e3138c6d0/Round_To_Page/1/0/
   procedure Test_Round_To_Page (Gnattest_T : in out Test) is
   --  bin_split-utils.ads:23:4:Round_To_Page
--  end read only

      pragma Unreferenced (Gnattest_T);
      
      use type Interfaces.Unsigned_64;

      P : constant Interfaces.Unsigned_64 := Mutools.Constants.Page_Size;

   begin

      Assert (Condition => Round_To_Page (10 * P + 1) = 11 * P,
             Message => "Not rounded upwards");

      Assert (Condition => Round_To_Page (5 * P - 1) = 5 * P,
              Message => "Not rounded upwards");
      
      Assert (Condition => Round_To_Page (7 * P) = 7 * P,
              Message => "Should not have rounded");

--  begin read only
   end Test_Round_To_Page;
--  end read only


--  begin read only
   procedure Test_Make_Output_Directory (Gnattest_T : in out Test);
   procedure Test_Make_Output_Directory_5d2f51 (Gnattest_T : in out Test) renames Test_Make_Output_Directory;
--  id:2.2/5d2f51bdd370a693/Make_Output_Directory/1/0/
   procedure Test_Make_Output_Directory (Gnattest_T : in out Test) is
   --  bin_split-utils.ads:30:4:Make_Output_Directory
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Positive is

         use type Ada.Directories.File_Kind;

         D : constant String := "test_data/out";

      begin
         if Ada.Directories.Exists (D) then
            Ada.Directories.Delete_Directory (D);
         end if;
         
         Make_Output_Directory (D);

         Assert (Condition =>
                   Ada.Directories.Exists (D) and then Ada.Directories.Kind (D)
                     = Ada.Directories.Directory,
                 Message   => "Directory not created");
         
      end Positive;
      
      procedure Exists_As_Dir is

         use type Ada.Directories.File_Kind;

         D : constant String := "test_data/out-ex";

      begin

         Ada.Directories.Create_Directory (D);
         
         Make_Output_Directory (D);

         Assert (Condition =>
                   Ada.Directories.Exists (D) and then Ada.Directories.Kind (D)
                     = Ada.Directories.Directory,
                 Message   => "Directory has been deleted");
         
      end Exists_As_Dir;
      
      procedure Exists_As_File is
         D : constant String := "test_data/not_a_dir";
      begin

         Make_Output_Directory (D);

         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Bin_Split_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Cannot create output directory 'test_data/not_a_dir': File exists",
                    Message   => "Exception mismatch");
      end Exists_As_File;

   begin

      Positive;
      Exists_As_Dir;
      Exists_As_File;

--  begin read only
   end Test_Make_Output_Directory;
--  end read only


--  begin read only
   procedure Test_With_Output_Dir (Gnattest_T : in out Test);
   procedure Test_With_Output_Dir_1b2f05 (Gnattest_T : in out Test) renames Test_With_Output_Dir;
--  id:2.2/1b2f058bf4a3889f/With_Output_Dir/1/0/
   procedure Test_With_Output_Dir (Gnattest_T : in out Test) is
   --  bin_split-utils.ads:34:4:With_Output_Dir
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_With_Output_Dir;
--  end read only

end Bin_Split.Utils.Test_Data.Tests;
