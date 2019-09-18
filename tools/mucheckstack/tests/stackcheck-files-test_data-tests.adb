--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Stackcheck.Files.Test_Data.

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
package body Stackcheck.Files.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Get_Control_Flow_Info_Files (Gnattest_T : in out Test);
   procedure Test_Get_Control_Flow_Info_Files_13717a (Gnattest_T : in out Test) renames Test_Get_Control_Flow_Info_Files;
--  id:2.2/13717ae3f3576588/Get_Control_Flow_Info_Files/1/0/
   procedure Test_Get_Control_Flow_Info_Files (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Paths_1 : constant Path_Names
        := Get_Control_Flow_Info_Files (GPR_File => "data/testci.gpr");
      Paths_2 : constant Path_Names
        := Get_Control_Flow_Info_Files (GPR_File => "data/testci_main.gpr");

       --  Returns True if the given path is found in the reference CI paths.
      function Has_Match (Path : Unbounded_String) return Boolean;

      ----------------------------------------------------------------------

      function Has_Match (Path : Unbounded_String) return Boolean
      is
      begin
         for R of Ref_CI_Paths loop
            declare
               Cur_Len  : constant Natural := Length (R);
               Cur_Path : constant Unbounded_String
                 := Ada.Strings.Unbounded.Tail (Source => Path,
                                                Count  => Cur_Len);
            begin
               if Cur_Path = R then
                  return True;
               end if;
            end;
         end loop;
         return False;
      end Has_Match;
   begin
      Assert (Condition => Paths_1'Length = Ref_CI_Paths'Length,
              Message   => "CI file count mismatch");
      Assert (Condition => Paths_2'Length = Ref_CI_Paths'Length + 1,
              Message   => "Main CI file count mismatch" & Paths_2'Length'Img);
      for I in Paths_1'Range loop
         Assert (Condition => Has_Match (Path => Paths_1 (I)),
                 Message   => "Path mismatch: '" & To_String (Paths_1 (I))
                 & "'");
      end loop;

      declare
         Ref_Binder_File : constant Unbounded_String
           :=  To_Unbounded_String ("obj/testci_main/b__testci.ci");
         Ref_Length      : constant Natural := Length (Ref_Binder_File);

         Contains_Binder_Path : Boolean := False;
      begin
         for P of Paths_2 loop
            if Ada.Strings.Unbounded.Tail (Source => P,
                                           Count  => Ref_Length)
              = Ref_Binder_File
            then
               Contains_Binder_Path := True;
            end if;
         end loop;
         Assert (Condition => Contains_Binder_Path,
                 Message   => "Path to binder file missing");
      end;

      begin
         declare
            Paths : constant Path_Names
              := Get_Control_Flow_Info_Files (GPR_File => "data/invalid.gpr");
         begin
            Assert (Condition => False,
                    Message   => "Exception expected (1)");
         end;

      exception
         when E : IO_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "invalid.gpr:1:06: unknown project file: "
                    & """nonexistent""",
                    Message   => "Exception message mismatch (1)");
      end;

      begin
         declare
            Dummy : constant Path_Names
              := Get_Control_Flow_Info_Files (GPR_File => "nonexistent.gpr");
         begin
            Assert (Condition => False,
                    Message   => "Exception expected (2)");
         end;

      exception
         when E : IO_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "nonexistent.gpr is not a regular file",
                    Message   => "Exception message mismatch (2)");
      end;
--  begin read only
   end Test_Get_Control_Flow_Info_Files;
--  end read only


--  begin read only
   procedure Test_For_Each_File (Gnattest_T : in out Test);
   procedure Test_For_Each_File_8b6767 (Gnattest_T : in out Test) renames Test_For_Each_File;
--  id:2.2/8b6767cad92eafd5/For_Each_File/0/0/
   procedure Test_For_Each_File (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

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

      No_Path          : constant Path_Names (1 .. 0) := (others => <>);
      Nonexistent_Path : constant Path_Names (1 .. 1)
        := (1 => To_Unbounded_String ("nonexistent/path"));
   begin
      begin
         For_Each_File (Files   => Nonexistent_Path,
                        Process => Inc_Counter'Access);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : IO_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E) =
                      "Unable to open file 'nonexistent/path' - "
                    & "nonexistent/path: No such file or directory",
                    Message   => "Exception message mismatch (1)");
      end;

      begin
         For_Each_File (Files   => Ref_CI_Paths,
                        Process => Raise_Exception'Access);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Test_Exception =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = Test_Ex_Msg,
                    Message   => "Exception message mismatch (2)");
      end;

      For_Each_File (Files   => No_Path,
                     Process => Inc_Counter'Access);
      Assert (Condition => Counter = 0,
              Message   => "Processed file count mismatch (1)");

      For_Each_File (Files   => Ref_CI_Paths,
                     Process => Inc_Counter'Access);
      Assert (Condition => Counter = Ref_CI_Paths'Length,
              Message   => "Processed file count mismatch (2)");

      Counter := 0;
      For_Each_File (Files   => Ref_CI_Paths (2 .. 3),
                     Process => Inc_Counter'Access);
      Assert (Condition => Counter = 2,
              Message   => "Processed file count mismatch (3)");
--  begin read only
   end Test_For_Each_File;
--  end read only


--  begin read only
   procedure Test_To_Path_Names (Gnattest_T : in out Test);
   procedure Test_To_Path_Names_d73253 (Gnattest_T : in out Test) renames Test_To_Path_Names;
--  id:2.2/d7325353fbbb4d24/To_Path_Names/1/0/
   procedure Test_To_Path_Names (Gnattest_T : in out Test) is
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
end Stackcheck.Files.Test_Data.Tests;
