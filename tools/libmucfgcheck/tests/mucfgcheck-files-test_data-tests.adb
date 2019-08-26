--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Files.Test_Data.

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
package body Mucfgcheck.Files.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Set_Input_Directory (Gnattest_T : in out Test);
   procedure Test_Set_Input_Directory_400f2c (Gnattest_T : in out Test) renames Test_Set_Input_Directory;
--  id:2.2/400f2c005681d9c3/Set_Input_Directory/1/0/
   procedure Test_Set_Input_Directory (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Strings.Unbounded.Unbounded_String;

   begin
      Set_Input_Directory (Dir => "testdir");
      Assert (Condition => Input_Dir = "testdir",
              Message   => "Dir mismatch");
--  begin read only
   end Test_Set_Input_Directory;
--  end read only


--  begin read only
   procedure Test_Get_Input_Directory (Gnattest_T : in out Test);
   procedure Test_Get_Input_Directory_e68eb2 (Gnattest_T : in out Test) renames Test_Get_Input_Directory;
--  id:2.2/e68eb225be5fe66d/Get_Input_Directory/1/0/
   procedure Test_Get_Input_Directory (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Input_Dir := Ada.Strings.Unbounded.To_Unbounded_String ("some_dir");
      Assert (Condition => Get_Input_Directory = "some_dir",
              Message   => "Dir mismatch");
--  begin read only
   end Test_Get_Input_Directory;
--  end read only


--  begin read only
   procedure Test_Files_Exist (Gnattest_T : in out Test);
   procedure Test_Files_Exist_00e851 (Gnattest_T : in out Test) renames Test_Files_Exist;
--  id:2.2/00e8516a56d54bdf/Files_Exist/1/0/
   procedure Test_Files_Exist (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Files  : DOM.Core.Node_List;
      Policy : Muxml.XML_Data_Type;
   begin
      Set_Input_Directory (Dir => "data");

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Remove existing files.

      Files := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/memory/memory/file");
      for I in 0 .. DOM.Core.Nodes.Length (List => Files) - 1 loop
         Muxml.Utils.Remove_Child
           (Node       => DOM.Core.Nodes.Parent_Node
              (N => DOM.Core.Nodes.Item
                   (List  => Files,
                    Index => I)),
            Child_Name => "file");
      end loop;

      --  Must not raise an exception.

      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Policy,
         Name        => "policy",
         Address     => "16#0010_0000#",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "system",
         File_Name   => "test_policy.xml",
         File_Offset => "none");
      Files_Exist (Data => Policy);

      --  Add entry with invalid filename.

      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Policy,
         Name        => "linux|acpi_rsdp",
         Address     => "16#0010_0000#",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_acpi_rsdp",
         File_Name   => "nonexistent",
         File_Offset => "none");

      begin
         Files_Exist (Data => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "File 'data/nonexistent' referenced by physical memory "
                    & "region 'linux|acpi_rsdp' not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Files_Exist;
--  end read only


--  begin read only
   procedure Test_Files_Size (Gnattest_T : in out Test);
   procedure Test_Files_Size_d33017 (Gnattest_T : in out Test) renames Test_Files_Size;
--  id:2.2/d3301771bce21920/Files_Size/1/0/
   procedure Test_Files_Size (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure File_Larger_Than_Memory
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Set_Input_Directory (Dir => "data");

         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Muxml.Utils.Remove_Elements
           (Doc   => Policy.Doc,
            XPath => "/system/memory/memory[file]");
         Mutools.XML_Utils.Add_Memory_Region
           (Policy      => Policy,
            Name        => "linux|acpi_rsdp",
            Address     => "16#0010_0000#",
            Size        => "16#0000#",
            Caching     => "WB",
            Alignment   => "16#1000#",
            Memory_Type => "subject_acpi_rsdp",
            File_Name   => "testfile",
            File_Offset => "none");

         begin
            Files_Size (Data => Policy);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "File 'data/testfile' too large for physical memory"
                       & " region 'linux|acpi_rsdp': 16#001e# > 16#0000#",
                       Message   => "Exception mismatch");
         end;
      end File_Larger_Than_Memory;

      ----------------------------------------------------------------------

      procedure Offset_Larger_Than_File
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Set_Input_Directory (Dir => "data");

         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Muxml.Utils.Remove_Elements
           (Doc   => Policy.Doc,
            XPath => "/system/memory/memory[file]");
         Mutools.XML_Utils.Add_Memory_Region
           (Policy      => Policy,
            Name        => "linux|acpi_rsdp",
            Address     => "16#0010_0000#",
            Size        => "16#0000#",
            Caching     => "WB",
            Alignment   => "16#1000#",
            Memory_Type => "subject_acpi_rsdp",
            File_Name   => "testfile",
            File_Offset => "16#ffff#");

         begin
            Files_Size (Data => Policy);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Offset of file 'data/testfile' referenced by "
                       & "physical memory region 'linux|acpi_rsdp' larger than "
                       & "file size: 16#ffff# > 16#001e#",
                       Message   => "Exception mismatch");
         end;
      end Offset_Larger_Than_File;
   begin
      File_Larger_Than_Memory;
      Offset_Larger_Than_File;
--  begin read only
   end Test_Files_Size;
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
end Mucfgcheck.Files.Test_Data.Tests;
