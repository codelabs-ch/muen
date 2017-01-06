--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Stackcheck.Files.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Stackcheck.Files.Test_Data.Tests is


--  begin read only
   procedure Test_Get_Object_Dirs (Gnattest_T : in out Test);
   procedure Test_Get_Object_Dirs_8173a5 (Gnattest_T : in out Test) renames Test_Get_Object_Dirs;
--  id:2.2/8173a511f05b084e/Get_Object_Dirs/1/0/
   procedure Test_Get_Object_Dirs (Gnattest_T : in out Test) is
   --  stackcheck-files.ads:29:4:Get_Object_Dirs
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref_Paths : constant Path_Names
        := (1 => To_Unbounded_String ("data/sm/"),
            2 => To_Unbounded_String ("data/libdebuglog/"),
            3 => To_Unbounded_String ("data/libmuchannel/"),
            4 => To_Unbounded_String ("data/libmutime/"),
            5 => To_Unbounded_String ("data/libmusinfo/"));
   begin
      declare
         Paths : constant Path_Names
           := Get_Object_Dirs (GPR_File => "data/sm.gpr");
      begin
         for I in Paths'Range loop
            Assert (Condition => Tail
                    (Source => Paths (I),
                     Count  => Length (Ref_Paths (I))) = Ref_Paths (I),
                    Message   => "Object dir path mismatch (" & I'Img & " )");
         end loop;
      end;

      begin
         declare
            Paths : constant Path_Names
              := Get_Object_Dirs (GPR_File => "data/invalid.gpr");
         begin
            Assert (Condition => False,
                    Message   => "Exception expected");
         end;

      exception
         when E : IO_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "invalid.gpr:1:06: unknown project file: "
                    & """nonexistent""",
                    Message   => "Exception message mismatch");
      end;
--  begin read only
   end Test_Get_Object_Dirs;
--  end read only

end Stackcheck.Files.Test_Data.Tests;
