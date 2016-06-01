--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Spec.Policy_Gpr.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Spec.Policy_Gpr.Test_Data.Tests is


--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_23ab15 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/23ab1562ae4604fa/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  spec-policy_gpr.ads:25:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy     : Muxml.XML_Data_Type;
      Policy_GPR : constant String := "obj/policy.gpr";
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/features/iommu",
         Name  => "enabled",
         Value => "false");

      Write (Output_Dir => "obj",
             Policy     => Policy);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/policy_iommu_disable.gpr.ref",
               Filename2 => Policy_GPR),
              Message   => "Policy project file mismatch");
      Ada.Directories.Delete_File (Name => Policy_GPR);
--  begin read only
   end Test_Write;
--  end read only

end Spec.Policy_Gpr.Test_Data.Tests;