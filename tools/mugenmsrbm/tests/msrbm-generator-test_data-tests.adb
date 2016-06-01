--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Msrbm.Generator.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Msrbm.Generator.Test_Data.Tests is


--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_23ab15 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/23ab1562ae4604fa/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  msrbm-generator.ads:26:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;

      Tau0_Msrbm : constant String := "obj/tau0_msrbm";
      Sub1_Msrbm : constant String := "obj/subject1_msrbm";
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Write (Output_Dir => "obj",
             Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/tau0_msrbm.ref",
               Filename2 => Tau0_Msrbm),
              Message   => "Tau0 MSR bitmap mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/subject1_msrbm.ref",
               Filename2 => Sub1_Msrbm),
              Message   => "Subject 1 MSR bitmap mismatch");
--  begin read only
   end Test_Write;
--  end read only

end Msrbm.Generator.Test_Data.Tests;