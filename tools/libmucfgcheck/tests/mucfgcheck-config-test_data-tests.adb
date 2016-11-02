--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Config.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Mucfgcheck.Config.Test_Data.Tests is


--  begin read only
   procedure Test_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Name_Uniqueness_7f1559 (Gnattest_T : in out Test) renames Test_Name_Uniqueness;
--  id:2.2/7f15594730cfb6f0/Name_Uniqueness/1/0/
   procedure Test_Name_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-config.ads:25:4:Name_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must no raise exception.

      Name_Uniqueness (XML_Data => Data);

      --  Duplicate config entry.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/config/boolean[@name='debug_enabled']",
         Name  => "name",
         Value => "iommu_enabled");

      begin
         Name_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Multiple config variables with name 'iommu_enabled'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Required_Presence (Gnattest_T : in out Test);
   procedure Test_Required_Presence_f1ac2c (Gnattest_T : in out Test) renames Test_Required_Presence;
--  id:2.2/f1ac2c1e35edbd55/Required_Presence/1/0/
   procedure Test_Required_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-config.ads:28:4:Required_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must no raise exception.

      Required_Presence (XML_Data => Data);

      --  Remove required value.

      declare
         Config_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Data.Doc,
              XPath => "/system/config/boolean[@name='iommu_enabled']");
         Dummy       : DOM.Core.Node;
      begin
         Dummy := DOM.Core.Nodes.Remove_Child
           (N         => DOM.Core.Nodes.Parent_Node (N => Config_Node),
            Old_Child => Config_Node);

         Required_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Required boolean config value 'iommu_enabled' missing",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Required_Presence;
--  end read only

end Mucfgcheck.Config.Test_Data.Tests;
