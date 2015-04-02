--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Spec.Scheduling.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Spec.Scheduling.Test_Data.Tests is


--  begin read only
   procedure Test_Write_Spec_File (Gnattest_T : in out Test);
   procedure Test_Write_Spec_File_5e6038 (Gnattest_T : in out Test) renames Test_Write_Spec_File;
--  id:2.2/5e60383215b0a822/Write_Spec_File/1/0/
   procedure Test_Write_Spec_File (Gnattest_T : in out Test) is
   --  spec-scheduling.ads:25:4:Write_Spec_File
--  end read only

      pragma Unreferenced (Gnattest_T);

      Spec_File : constant String := "obj/skp-scheduling.ads";
      Policy    : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Write_Spec_File (Output_Dir => "obj",
                       Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/skp-scheduling.ads",
               Filename2 => Spec_File),
              Message   => "Scheduling spec mismatch");

      Ada.Directories.Delete_File (Name => Spec_File);
--  begin read only
   end Test_Write_Spec_File;
--  end read only

end Spec.Scheduling.Test_Data.Tests;
