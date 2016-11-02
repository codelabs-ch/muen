--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pack.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Pack.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Get_Image_Size (Gnattest_T : in out Test);
   procedure Test_Get_Image_Size_046aa6 (Gnattest_T : in out Test) renames Test_Get_Image_Size;
--  id:2.2/046aa68f3a717a5c/Get_Image_Size/1/0/
   procedure Test_Get_Image_Size (Gnattest_T : in out Test) is
   --  pack-utils.ads:27:4:Get_Image_Size
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "linux|bin",
         Address     => "16#0011_4000#",
         Size        => "16#0001_3000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_binary",
         File_Name   => "obj1.o",
         File_Offset => "none");

      Assert (Condition => Get_Image_Size (Policy => Data) = 16#127000#,
              Message   => "Image size mismatch");
--  begin read only
   end Test_Get_Image_Size;
--  end read only

end Pack.Utils.Test_Data.Tests;
