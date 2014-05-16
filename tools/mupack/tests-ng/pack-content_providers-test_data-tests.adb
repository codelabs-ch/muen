--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pack.Content_Providers.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Pack.Content_Providers.Test_Data.Tests is


--  begin read only
   procedure Test_Process_Files (Gnattest_T : in out Test);
   procedure Test_Process_Files_fb406a (Gnattest_T : in out Test) renames Test_Process_Files;
--  id:2.2/fb406a8bcbec6e06/Process_Files/1/0/
   procedure Test_Process_Files (Gnattest_T : in out Test) is
   --  pack-content_providers.ads:36:4:Process_Files
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Data   : Param_Type (16#126000#);
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

      Data.XML_Doc := Policy.Doc;
      Process_Files (Data => Data);

      Image.Write (Image    => Data.Image,
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
              Message   => "Manifest file differs");

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
   --  pack-content_providers.ads:39:4:Process_Fills
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Process_Fills;
--  end read only


--  begin read only
   procedure Test_Register_All (Gnattest_T : in out Test);
   procedure Test_Register_All_3f90ea (Gnattest_T : in out Test) renames Test_Register_All;
--  id:2.2/3f90ea30314141bf/Register_All/1/0/
   procedure Test_Register_All (Gnattest_T : in out Test) is
   --  pack-content_providers.ads:42:4:Register_All
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Register_All;
--  end read only


--  begin read only
   procedure Test_Set_Input_Directory (Gnattest_T : in out Test);
   procedure Test_Set_Input_Directory_400f2c (Gnattest_T : in out Test) renames Test_Set_Input_Directory;
--  id:2.2/400f2c005681d9c3/Set_Input_Directory/1/0/
   procedure Test_Set_Input_Directory (Gnattest_T : in out Test) is
   --  pack-content_providers.ads:45:4:Set_Input_Directory
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Set_Input_Directory;
--  end read only


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_ca760b (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/ca760b43164ae370/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  pack-content_providers.ads:48:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Run;
--  end read only


--  begin read only
   procedure Test_Get_Count (Gnattest_T : in out Test);
   procedure Test_Get_Count_1fbd7c (Gnattest_T : in out Test) renames Test_Get_Count;
--  id:2.2/1fbd7c784b3d55c2/Get_Count/1/0/
   procedure Test_Get_Count (Gnattest_T : in out Test) is
   --  pack-content_providers.ads:51:4:Get_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Count;
--  end read only

end Pack.Content_Providers.Test_Data.Tests;
