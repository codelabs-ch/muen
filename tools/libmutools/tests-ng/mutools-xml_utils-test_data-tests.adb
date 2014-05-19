--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.XML_Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mutools.XML_Utils.Test_Data.Tests is


--  begin read only
   procedure Test_1_Add_Memory_Region (Gnattest_T : in out Test);
   procedure Test_Add_Memory_Region_6142bd (Gnattest_T : in out Test) renames Test_1_Add_Memory_Region;
--  id:2.2/6142bd7e03979890/Add_Memory_Region/1/0/
   procedure Test_1_Add_Memory_Region (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:25:4:Add_Memory_Region
--  end read only

      pragma Unreferenced (Gnattest_T);

      Filename : constant String := "obj/memory.xml";
      Policy   : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Add_Memory_Region
        (Policy      => Policy,
         Name        => "test",
         Address     => "16#9000_1000#",
         Size        => "16#3000#",
         Caching     => "UC",
         Alignment   => "16#1000#",
         Memory_Type => "");
      Add_Memory_Region
        (Policy      => Policy,
         Name        => "noaddress",
         Address     => "",
         Size        => "16#8000#",
         Caching     => "WC",
         Alignment   => "16#0020_0000#",
         Memory_Type => "");

      Muxml.Write (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => Filename);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Filename,
               Filename2 => "data/memory.xml"),
              Message   => "Policy mismatch");

      Ada.Directories.Delete_File (Name => Filename);
--  begin read only
   end Test_1_Add_Memory_Region;
--  end read only


--  begin read only
   procedure Test_2_Add_Memory_Region (Gnattest_T : in out Test);
   procedure Test_Add_Memory_Region_741476 (Gnattest_T : in out Test) renames Test_2_Add_Memory_Region;
--  id:2.2/741476e4715f4faa/Add_Memory_Region/0/0/
   procedure Test_2_Add_Memory_Region (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:36:4:Add_Memory_Region
--  end read only

      pragma Unreferenced (Gnattest_T);

      Filename : constant String := "obj/memory_with_file.xml";
      Policy   : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Add_Memory_Region
        (Policy      => Policy,
         Name        => "test",
         Address     => "16#2000#",
         Size        => "16#4000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "",
         File_Name   => "testfile",
         File_Offset => "16#1000#");

      Muxml.Write (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => Filename);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Filename,
               Filename2 => "data/memory_with_file.xml"),
              Message   => "Policy mismatch");

      Ada.Directories.Delete_File (Name => Filename);
--  begin read only
   end Test_2_Add_Memory_Region;
--  end read only

end Mutools.XML_Utils.Test_Data.Tests;
