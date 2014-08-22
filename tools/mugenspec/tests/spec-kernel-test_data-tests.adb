--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Spec.Kernel.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Spec.Kernel.Test_Data.Tests is


--  begin read only
   procedure Test_Write_Project_File (Gnattest_T : in out Test);
   procedure Test_Write_Project_File_e8c16c (Gnattest_T : in out Test) renames Test_Write_Project_File;
--  id:2.2/e8c16cd5ea7265cc/Write_Project_File/1/0/
   procedure Test_Write_Project_File (Gnattest_T : in out Test) is
   --  spec-kernel.ads:25:4:Write_Project_File
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy_GPR : constant String := "obj/policy.gpr";
      Policy     : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Remove all 'iommu' devices.

      declare
         Devices : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/kernel/devices");
         Node    : DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/kernel/devices/device[@physical='iommu_1']");
      begin
         Node := DOM.Core.Nodes.Remove_Child
           (N         => Devices,
            Old_Child => Node);
         Node := Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/kernel/devices/device[@physical='iommu_2']");
         Node := DOM.Core.Nodes.Remove_Child
           (N         => Devices,
            Old_Child => Node);
      end;

      Write_Project_File (Output_Dir => "obj",
                          Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/policy_iommu_disable.gpr.ref",
               Filename2 => Policy_GPR),
              Message   => "Policy project file mismatch");

      Ada.Directories.Delete_File (Name => Policy_GPR);
--  begin read only
   end Test_Write_Project_File;
--  end read only

end Spec.Kernel.Test_Data.Tests;
