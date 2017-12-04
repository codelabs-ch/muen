--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bzpatch.Test_Data.

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
package body Bzpatch.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Patch (Gnattest_T : in out Test);
   procedure Test_Patch_4c544e (Gnattest_T : in out Test) renames Test_Patch;
--  id:2.2/4c544ef13505f519/Patch/1/0/
   procedure Test_Patch (Gnattest_T : in out Test) is
   --  bzpatch.ads:24:4:Patch
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Patch_Bzimage
      is
      begin
         Patch (Input  => "data/bzimage.32",
                Output => "obj/bzimage.32.patched");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/bzimage.32.ref",
                  Filename2 => "obj/bzimage.32.patched"),
                 Message   => "Mismatch in 32-bit image");
         Ada.Directories.Delete_File (Name => "obj/bzimage.32.patched");

         Patch (Input  => "data/bzimage.64",
                Output => "obj/bzimage.64.patched");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/bzimage.64.ref",
                  Filename2 => "obj/bzimage.64.patched"),
                 Message   => "Mismatch in 64-bit image");
         Ada.Directories.Delete_File (Name => "obj/bzimage.64.patched");
      end Patch_Bzimage;

      ----------------------------------------------------------------------

      procedure Invalid_Bzimage
      is
      begin
         Patch (Input  => "data/invalid_bzimage",
                Output => "obj/foobar");
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Patch_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unable to find entry point in bzImage "
                    & "'data/invalid_bzimage'",
                    Message   => "Exception mismatch");
      end Invalid_Bzimage;
   begin
      Patch_Bzimage;
      Invalid_Bzimage;
--  begin read only
   end Test_Patch;
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
end Bzpatch.Test_Data.Tests;
