--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pack.Content_Providers.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Pack.Content_Providers.Test_Data.Tests is


--  begin read only
   procedure Test_Process_Files (Gnattest_T : in out Test);
   procedure Test_Process_Files_fb406a (Gnattest_T : in out Test) renames Test_Process_Files;
--  id:2.2/fb406a8bcbec6e06/Process_Files/1/0/
   procedure Test_Process_Files (Gnattest_T : in out Test) is
   --  pack-content_providers.ads:42:4:Process_Files
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Streams.Stream_Element_Array;

      Policy   : Muxml.XML_Data_Type;
      Data_Dry : Param_Type (End_Address => 16#126000#,
                             Dry_Run     => True);
      Data     : Param_Type (End_Address => 16#126000#,
                             Dry_Run     => False);
   begin
      Set_Input_Directory (Dir => "data");

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Policy,
         Name        => "mboot",
         Address     => "16#0010_0000#",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "system",
         File_Name   => "mboot",
         File_Offset => "none");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Policy,
         Name        => "linux|acpi_rsdp",
         Address     => "16#0010_1000#",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_acpi_rsdp",
         File_Name   => "pattern",
         File_Offset => "none");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Policy,
         Name        => "linux|bin",
         Address     => "16#0010_2000#",
         Size        => "16#0001_3000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_binary",
         File_Name   => "obj1.o",
         File_Offset => "16#0004#");

      Data_Dry.XML_Doc := Policy.Doc;
      Process_Files (Data => Data_Dry);
      Manifest.Write (Manifest => Data_Dry.Manifest,
                      Filename => "obj/process_files_dry.manifest");
      Assert (Condition => Mutools.Image.Get_Buffer
              (Image   => Data_Dry.Image,
               Address => 0,
               Size    => 1000) = (1 .. 1000 => 0),
              Message   => "Image file differs (1)");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/process_files_dry.manifest",
               Filename2 => "data/process_files_dry.manifest"),
              Message   => "Manifest file differs (1)");

      Ada.Directories.Delete_File (Name => "obj/process_files_dry.manifest");

      Data.XML_Doc := Policy.Doc;
      Process_Files (Data => Data);

      Mutools.Image.Write (Image    => Data.Image,
                           Filename => "obj/process_files.img");
      Manifest.Write (Manifest => Data.Manifest,
                      Filename => "obj/process_files.manifest");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/process_files.img",
               Filename2 => "data/process_files.img"),
              Message   => "Image file differs");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/process_files.manifest",
               Filename2 => "data/process_files.manifest"),
              Message   => "Manifest file differs (2)");

      Ada.Directories.Delete_File (Name => "obj/process_files.img");
      Ada.Directories.Delete_File (Name => "obj/process_files.manifest");
--  begin read only
   end Test_Process_Files;
--  end read only


--  begin read only
   procedure Test_Process_Fills (Gnattest_T : in out Test);
   procedure Test_Process_Fills_08a9c0 (Gnattest_T : in out Test) renames Test_Process_Fills;
--  id:2.2/08a9c00f046ebaf6/Process_Fills/1/0/
   procedure Test_Process_Fills (Gnattest_T : in out Test) is
   --  pack-content_providers.ads:45:4:Process_Fills
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Streams.Stream_Element_Array;

      Policy   : Muxml.XML_Data_Type;
      Data_Dry : Param_Type (End_Address => 9,
                             Dry_Run     => True);
      Data     : Param_Type (End_Address => 9,
                             Dry_Run     => False);
   begin
      Set_Input_Directory (Dir => "data");

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Policy,
         Name        => "filled",
         Address     => "16#0000#",
         Size        => "16#000a#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject");

      declare
         Fill   : DOM.Core.Node;
         Memory : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/memory/memory[@name='filled']");
      begin
         Fill := DOM.Core.Documents.Create_Element
           (Doc      => Policy.Doc,
            Tag_Name => "fill");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Fill,
            Name  => "pattern",
            Value => "16#42#");
         Muxml.Utils.Append_Child
           (Node      => Memory,
            New_Child => Fill);
      end;

      Data_Dry.XML_Doc := Policy.Doc;
      Process_Fills (Data => Data_Dry);
      Manifest.Write (Manifest => Data_Dry.Manifest,
                      Filename => "obj/process_fills_dry.manifest");
      Assert (Condition => Mutools.Image.Get_Buffer
              (Image   => Data_Dry.Image,
               Address => 0,
               Size    => 9) = (1 .. 9 => 0),
              Message   => "Image file differs (1)");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/process_fills_dry.manifest",
               Filename2 => "data/process_fills.manifest"),
              Message   => "Manifest file differs (1)");

      Ada.Directories.Delete_File (Name => "obj/process_fills_dry.manifest");

      Data.XML_Doc := Policy.Doc;
      Process_Fills (Data => Data);

      Mutools.Image.Write (Image    => Data.Image,
                           Filename => "obj/process_fills.img");
      Manifest.Write (Manifest => Data.Manifest,
                      Filename => "obj/process_fills.manifest");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/process_fills.img",
               Filename2 => "data/process_fills.img"),
              Message   => "Image file differs (2)");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/process_fills.manifest",
               Filename2 => "data/process_fills.manifest"),
              Message   => "Manifest file differs (2)");

      Ada.Directories.Delete_File (Name => "obj/process_fills.img");
      Ada.Directories.Delete_File (Name => "obj/process_fills.manifest");
--  begin read only
   end Test_Process_Fills;
--  end read only


--  begin read only
   procedure Test_Register_All (Gnattest_T : in out Test);
   procedure Test_Register_All_3f90ea (Gnattest_T : in out Test) renames Test_Register_All;
--  id:2.2/3f90ea30314141bf/Register_All/1/0/
   procedure Test_Register_All (Gnattest_T : in out Test) is
   --  pack-content_providers.ads:48:4:Register_All
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Register_All;
      Assert (Condition => Content_Procs.Get_Count = 2,
              Message   => "Count mismatch");
--  begin read only
   end Test_Register_All;
--  end read only


--  begin read only
   procedure Test_Set_Input_Directory (Gnattest_T : in out Test);
   procedure Test_Set_Input_Directory_400f2c (Gnattest_T : in out Test) renames Test_Set_Input_Directory;
--  id:2.2/400f2c005681d9c3/Set_Input_Directory/1/0/
   procedure Test_Set_Input_Directory (Gnattest_T : in out Test) is
   --  pack-content_providers.ads:51:4:Set_Input_Directory
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
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_ca760b (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/ca760b43164ae370/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  pack-content_providers.ads:54:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      Unused : Param_Type := (End_Address => 10,
                              Dry_Run     => False,
                              others      => <>);
   begin
      Content_Procs.Register (Process => Inc_Counter'Access);
      Run (Data => Unused);

      Assert (Condition => Test_Counter = 1,
              Message   => "Counter mismatch");
--  begin read only
   end Test_Run;
--  end read only


--  begin read only
   procedure Test_Get_Count (Gnattest_T : in out Test);
   procedure Test_Get_Count_1fbd7c (Gnattest_T : in out Test) renames Test_Get_Count;
--  id:2.2/1fbd7c784b3d55c2/Get_Count/1/0/
   procedure Test_Get_Count (Gnattest_T : in out Test) is
   --  pack-content_providers.ads:57:4:Get_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Content_Procs.Get_Count = 0,
              Message   => "Procs not zero:" & Content_Procs.Get_Count'Img);
      Content_Procs.Register (Process => Inc_Counter'Access);
      Assert (Condition => Content_Procs.Get_Count = 1,
              Message   => "Procs not one:" & Content_Procs.Get_Count'Img);
--  begin read only
   end Test_Get_Count;
--  end read only


--  begin read only
   procedure Test_Clear (Gnattest_T : in out Test);
   procedure Test_Clear_4b4f85 (Gnattest_T : in out Test) renames Test_Clear;
--  id:2.2/4b4f85da05a9b689/Clear/1/0/
   procedure Test_Clear (Gnattest_T : in out Test) is
   --  pack-content_providers.ads:60:4:Clear
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Content_Procs.Register (Process => Inc_Counter'Access);
      Assert (Condition => Content_Procs.Get_Count = 1,
              Message   => "Procs not one:" & Content_Procs.Get_Count'Img);

      Clear;
      Assert (Condition => Content_Procs.Get_Count = 0,
              Message   => "Procs not cleared:" & Content_Procs.Get_Count'Img);
--  begin read only
   end Test_Clear;
--  end read only

end Pack.Content_Providers.Test_Data.Tests;
