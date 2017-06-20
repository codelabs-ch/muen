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
   --  stackcheck-files.ads:32:4:Get_Object_Dirs
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref_Paths : constant Path_Names
        := (1 => To_Unbounded_String ("obj/testci/"),
            2 => To_Unbounded_String ("obj/liblog/"),
            3 => To_Unbounded_String ("obj/libbar/"));
   begin
      declare
         Paths : constant Path_Names
           := Get_Object_Dirs (GPR_File => "data/testci.gpr");
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


--  begin read only
   procedure Test_Get_Control_Flow_Info_Files (Gnattest_T : in out Test);
   procedure Test_Get_Control_Flow_Info_Files_13717a (Gnattest_T : in out Test) renames Test_Get_Control_Flow_Info_Files;
--  id:2.2/13717ae3f3576588/Get_Control_Flow_Info_Files/1/0/
   procedure Test_Get_Control_Flow_Info_Files (Gnattest_T : in out Test) is
   --  stackcheck-files.ads:36:4:Get_Control_Flow_Info_Files
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref_CIs : constant Path_Names (1 .. 4)
        := (To_Unbounded_String ("obj/testci/testci.ci"),
            To_Unbounded_String ("obj/testci/foo.ci"),
            To_Unbounded_String ("obj/liblog/log.ci"),
            To_Unbounded_String ("obj/libbar/bar.ci"));
      Paths   : constant Path_Names
        := Get_Control_Flow_Info_Files (GPR_File => "data/testci.gpr");
   begin
      Assert (Condition => Paths'Length = Ref_CIs'Length,
              Message   => "CI file count mismatch");
      for I in Paths'Range loop
         declare
            Cur_Len  : constant Natural := Length (Ref_CIs (I));
            Cur_Path : constant Unbounded_String
              := Ada.Strings.Unbounded.Tail (Source => Paths (I),
                                             Count  => Cur_Len);
         begin
            Assert (Condition => Cur_Path = Ref_CIs (I),
                    Message   => "Path number" & I'Img & " mismatch: '"
                    & To_String (Cur_Path) & "' /= '" & To_String
                    (Ref_CIs (I)) & "'");
         end;
      end loop;
--  begin read only
   end Test_Get_Control_Flow_Info_Files;
--  end read only


--  begin read only
   procedure Test_For_Each_File (Gnattest_T : in out Test);
   procedure Test_For_Each_File_5086f9 (Gnattest_T : in out Test) renames Test_For_Each_File;
--  id:2.2/5086f9e3e428110d/For_Each_File/1/0/
   procedure Test_For_Each_File (Gnattest_T : in out Test) is
   --  stackcheck-files.ads:40:4:For_Each_File
--  end read only

      pragma Unreferenced (Gnattest_T);

      Counter : Natural := 0;

      Test_Ex_Msg    : constant String
        := "Don't panic, this is a test exception";
      Test_Exception : exception;

      --  Increment counter.
      procedure Inc_Counter (File : Ada.Text_IO.File_Type);

      --  Raise exception.
      procedure Raise_Exception (File : Ada.Text_IO.File_Type);

      ----------------------------------------------------------------------

      procedure Inc_Counter (File : Ada.Text_IO.File_Type)
      is
      begin
         Counter := Counter + 1;
      end Inc_Counter;

      ----------------------------------------------------------------------

      procedure Raise_Exception (File : Ada.Text_IO.File_Type)
      is
      begin
         raise Test_Exception with Test_Ex_Msg;
      end Raise_Exception;
   begin
      begin
         For_Each_File (Path    => "nonexistent/path",
                        Pattern => "",
                        Process => Inc_Counter'Access);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : IO_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E) =
                      "Directory 'nonexistent/path' does not exist",
                    Message   => "Exception message mismatch (1)");
      end;

      begin
         For_Each_File (Path    => "",
                        Pattern => "",
                        Process => Inc_Counter'Access);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : IO_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E) =
                      "Invalid directory name ''",
                    Message   => "Exception message mismatch (2)");
      end;

      begin
         For_Each_File (Path    => "data/testci/",
                        Pattern => "",
                        Process => Raise_Exception'Access);
         Assert (Condition => False,
                 Message   => "Exception expected (3)");

      exception
         when E : Test_Exception =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = Test_Ex_Msg,
                    Message   => "Exception message mismatch (3)");
      end;

      For_Each_File (Path    => "data/testci/",
                     Pattern => "",
                     Process => Inc_Counter'Access);
      Assert (Condition => Counter = 3,
              Message   => "Processed file count mismatch (1)");

      Counter := 0;
      For_Each_File (Path    => "data/testci/",
                     Pattern => "foo*.ads",
                     Process => Inc_Counter'Access);
      Assert (Condition => Counter = 1,
              Message   => "Processed file count mismatch (2)");
--  begin read only
   end Test_For_Each_File;
--  end read only


--  begin read only
   procedure Test_To_Path_Names (Gnattest_T : in out Test);
   procedure Test_To_Path_Names_d73253 (Gnattest_T : in out Test) renames Test_To_Path_Names;
--  id:2.2/d7325353fbbb4d24/To_Path_Names/1/0/
   procedure Test_To_Path_Names (Gnattest_T : in out Test) is
   --  stackcheck-files.ads:50:4:To_Path_Names
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;
      use GNATCOLL.VFS;

      No_Paths : constant Path_Names (1 .. 0) := (others => <>);

      Ref_Arr : constant Path_Names (1 .. 3)
        := (To_Unbounded_String ("/sbin/foo"),
            To_Unbounded_String ("/usr/bin/bar"),
            To_Unbounded_String ("tmp/foobar"));
      Arr : File_Array_Access;
   begin
      for P of Ref_Arr loop
         Append
           (Files => Arr,
            F     => Create (Full_Filename => +(To_String (P))));
      end loop;
      Assert (Condition => To_Path_Names (Files => Arr.all) = Ref_Arr,
              Message   => "File array paths mismatch");

      Assert (Condition => To_Path_Names
              (Files => GNATCOLL.VFS.Empty_File_Array) = No_Paths,
              Message   => "Empty file array mismatch");
--  begin read only
   end Test_To_Path_Names;
--  end read only

end Stackcheck.Files.Test_Data.Tests;
