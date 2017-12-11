--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bin_Split.Utils.Test_Data.

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
package body Bin_Split.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Round_Up (Gnattest_T : in out Test);
   procedure Test_Round_Up_a5410f (Gnattest_T : in out Test) renames Test_Round_Up;
--  id:2.2/a5410f420366d247/Round_Up/1/0/
   procedure Test_Round_Up (Gnattest_T : in out Test) is
   --  bin_split-utils.ads:27:4:Round_Up
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      P : constant Interfaces.Unsigned_64 := Mutools.Constants.Page_Size;
   begin
      Assert (Condition => Round_Up (10 * P + 1) = 11 * P,
              Message   => "Not rounded upwards (1)");

      Assert (Condition => Round_Up (5 * P - 1) = 5 * P,
              Message   => "Not rounded upwards (2)");

      Assert (Condition => Round_Up (7 * P) = 7 * P,
              Message   => "Should not have rounded");
--  begin read only
   end Test_Round_Up;
--  end read only


--  begin read only
   procedure Test_Make_Output_Directory (Gnattest_T : in out Test);
   procedure Test_Make_Output_Directory_5d2f51 (Gnattest_T : in out Test) renames Test_Make_Output_Directory;
--  id:2.2/5d2f51bdd370a693/Make_Output_Directory/1/0/
   procedure Test_Make_Output_Directory (Gnattest_T : in out Test) is
   --  bin_split-utils.ads:35:4:Make_Output_Directory
--  end read only

      pragma Unreferenced (Gnattest_T);

      -----------------------------------------------------------------------
      
      procedure Positive
      is
         use type Ada.Directories.File_Kind;

         D : constant String := "data/out";
      begin
         Make_Output_Directory (D);

         Assert (Condition =>
                   Ada.Directories.Exists (D) and then Ada.Directories.Kind (D)
                     = Ada.Directories.Directory,
                 Message   => "Directory not created");
      end Positive;

      -----------------------------------------------------------------------

      procedure Exists_As_Dir
      is
         use type Ada.Directories.File_Kind;

         D : constant String := "data/out-ex";
      begin
         Ada.Directories.Create_Directory (D);

         Make_Output_Directory (D);

         Assert (Condition =>
                   Ada.Directories.Exists (D) and then Ada.Directories.Kind (D)
                     = Ada.Directories.Directory,
                 Message   => "Directory has been deleted");
      end Exists_As_Dir;

      -----------------------------------------------------------------------

      procedure Exists_As_File
      is
         D : constant String := "data/not_a_dir";
      begin
         Make_Output_Directory (D);

         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Bin_Split_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Cannot create output directory 'data/not_a_dir': File exists",
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
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Bin_Split.Utils.Test_Data.Tests;
