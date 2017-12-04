--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Files.Test_Data.

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
package body Mutools.Files.Test_Data.Tests is

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
   procedure Test_Open_190c3e (Gnattest_T : in out Test) renames Test_Open;
--  id:2.2/190c3e2a11233ba0/Open/1/0/
   procedure Test_Open (Gnattest_T : in out Test) is
   --  mutools-files.ads:25:4:Open
--  end read only

      pragma Unreferenced (Gnattest_T);

      Fname   : constant String := "obj/testfile";
      My_File : Ada.Streams.Stream_IO.File_Type;
   begin
      Open (Filename => Fname,
            File     => My_File);
      Assert (Condition => Ada.Directories.Exists (Name => Fname),
              Message   => "File missing (1)");
      Ada.Streams.Stream_IO.Close (File => My_File);

      --  Open existing file.

      Open (Filename => Fname,
            File     => My_File);
      Assert (Condition => Ada.Directories.Exists (Name => Fname),
              Message   => "File missing (2)");
      Ada.Streams.Stream_IO.Close (File => My_File);
      Ada.Directories.Delete_File (Name => Fname);

      --  Try to open a file which is a directory.

      begin
         Open (Filename => "obj",
               File     => My_File);

      exception
         when IO_Error => null;
      end;
--  begin read only
   end Test_Open;
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
end Mutools.Files.Test_Data.Tests;
