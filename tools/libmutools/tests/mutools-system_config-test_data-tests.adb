--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.System_Config.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Mutools.System_Config.Test_Data.Tests is


--  begin read only
   procedure Test_Get_Value (Gnattest_T : in out Test);
   procedure Test_Get_Value_d13e21 (Gnattest_T : in out Test) renames Test_Get_Value;
--  id:2.2/d13e2143a0c1f788/Get_Value/1/0/
   procedure Test_Get_Value (Gnattest_T : in out Test) is
   --  mutools-system_config.ads:26:4:Get_Value
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Assert (Condition => Get_Value (Data => Policy,
                                      Name => "iommu_enabled"),
              Message   => "Boolean config value false");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/config/boolean[@name=""iommu_enabled""]",
         Name  => "value",
         Value => "false");
      Assert (Condition => not Get_Value (Data => Policy,
                                      Name => "iommu_enabled"),
              Message   => "Boolean config value true");

      begin
         declare
            Dummy : constant Boolean := Get_Value (Data => Policy,
                                                   Name => "nonexistent");
         begin
            Assert (Condition => False,
                    Message   => "Exception expected");
         end;

      exception
         when E : Not_Found =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "No boolean config option 'nonexistent' found",
                    Message   => "Exception message mismatch");
      end;
--  begin read only
   end Test_Get_Value;
--  end read only

end Mutools.System_Config.Test_Data.Tests;