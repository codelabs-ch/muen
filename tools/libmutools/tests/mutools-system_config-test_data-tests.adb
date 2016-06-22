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
   procedure Test_Has_Boolean (Gnattest_T : in out Test);
   procedure Test_Has_Boolean_89c551 (Gnattest_T : in out Test) renames Test_Has_Boolean;
--  id:2.2/89c551e2c6b63d90/Has_Boolean/1/0/
   procedure Test_Has_Boolean (Gnattest_T : in out Test) is
   --  mutools-system_config.ads:25:4:Has_Boolean
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Assert (Condition => Has_Boolean (Data => Policy,
                                        Name => "iommu_enabled"),
              Message   => "Boolean config variable not present");
      Assert (Condition => not Has_Boolean (Data => Policy,
                                            Name => "nonexistent"),
              Message   => "Boolean config variable present (non-existent)");
      Assert (Condition => not Has_Boolean (Data => Policy,
                                            Name => "session_count"),
              Message   => "Boolean config variable present (type mismatch)");
--  begin read only
   end Test_Has_Boolean;
--  end read only


--  begin read only
   procedure Test_Has_Integer (Gnattest_T : in out Test);
   procedure Test_Has_Integer_0bbd6e (Gnattest_T : in out Test) renames Test_Has_Integer;
--  id:2.2/0bbd6e7b6d7c7489/Has_Integer/1/0/
   procedure Test_Has_Integer (Gnattest_T : in out Test) is
   --  mutools-system_config.ads:31:4:Has_Integer
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Assert (Condition => Has_Integer (Data => Policy,
                                        Name => "session_count"),
              Message   => "Integer config variable not present");
      Assert (Condition => not Has_Integer (Data => Policy,
                                            Name => "nonexistent"),
              Message   => "Integer config variable present (non-existent)");
      Assert (Condition => not Has_Integer (Data => Policy,
                                            Name => "iommu_enabled"),
              Message   => "Integer config variable present (type mismatch)");
--  begin read only
   end Test_Has_Integer;
--  end read only


--  begin read only
   procedure Test_Has_String (Gnattest_T : in out Test);
   procedure Test_Has_String_06e83f (Gnattest_T : in out Test) renames Test_Has_String;
--  id:2.2/06e83fa4dda1248c/Has_String/1/0/
   procedure Test_Has_String (Gnattest_T : in out Test) is
   --  mutools-system_config.ads:37:4:Has_String
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Assert (Condition => Has_String (Data => Policy,
                                       Name => "system"),
              Message   => "String config variable not present");
      Assert (Condition => not Has_String (Data => Policy,
                                           Name => "nonexistent"),
              Message   => "String config variable present (non-existent)");
      Assert (Condition => not Has_String (Data => Policy,
                                           Name => "session_count"),
              Message   => "String config variable present (type mismatch)");
--  begin read only
   end Test_Has_String;
--  end read only


--  begin read only
   procedure Test_Has_Value (Gnattest_T : in out Test);
   procedure Test_Has_Value_b8a40a (Gnattest_T : in out Test) renames Test_Has_Value;
--  id:2.2/b8a40a3e1649e5a6/Has_Value/1/0/
   procedure Test_Has_Value (Gnattest_T : in out Test) is
   --  mutools-system_config.ads:43:4:Has_Value
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Assert (Condition => Has_Value (Data => Policy,
                                      Name => "system"),
              Message   => "Config variable not present (1)");
      Assert (Condition => Has_Value (Data => Policy,
                                      Name => "session_count"),
              Message   => "Config variable not present (2)");
      Assert (Condition => Has_Value (Data => Policy,
                                      Name => "iommu_enabled"),
              Message   => "Config variable not present (3)");
      Assert (Condition => not Has_Value (Data => Policy,
                                          Name => "nonexistent"),
              Message   => "Config variable present (non-existent)");
--  begin read only
   end Test_Has_Value;
--  end read only


--  begin read only
   procedure Test_1_Get_Value (Gnattest_T : in out Test);
   procedure Test_Get_Value_d13e21 (Gnattest_T : in out Test) renames Test_1_Get_Value;
--  id:2.2/d13e2143a0c1f788/Get_Value/1/0/
   procedure Test_1_Get_Value (Gnattest_T : in out Test) is
   --  mutools-system_config.ads:50:4:Get_Value
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
   end Test_1_Get_Value;
--  end read only


--  begin read only
   procedure Test_2_Get_Value (Gnattest_T : in out Test);
   procedure Test_Get_Value_2afad1 (Gnattest_T : in out Test) renames Test_2_Get_Value;
--  id:2.2/2afad142bea106b4/Get_Value/0/0/
   procedure Test_2_Get_Value (Gnattest_T : in out Test) is
   --  mutools-system_config.ads:57:4:Get_Value
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Assert (Condition => Get_Value (Data => Policy,
                                      Name => "session_count") = 4,
              Message   => "Integer config value mismatch");

      begin
         declare
            Dummy : constant Integer := Get_Value (Data => Policy,
                                                   Name => "nonexistent");
         begin
            Assert (Condition => False,
                    Message   => "Exception expected");
         end;

      exception
         when E : Not_Found =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "No integer config option 'nonexistent' found",
                    Message   => "Exception message mismatch");
      end;
--  begin read only
   end Test_2_Get_Value;
--  end read only


--  begin read only
   procedure Test_3_Get_Value (Gnattest_T : in out Test);
   procedure Test_Get_Value_4f8a85 (Gnattest_T : in out Test) renames Test_3_Get_Value;
--  id:2.2/4f8a8505e36090b7/Get_Value/0/0/
   procedure Test_3_Get_Value (Gnattest_T : in out Test) is
   --  mutools-system_config.ads:64:4:Get_Value
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Assert (Condition => Get_Value (Data => Policy,
                                      Name => "system") = "test_system.xml",
              Message   => "String config value mismatch (1)");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/config/string[@name=""system""]",
         Name  => "value",
         Value => "");
      Assert (Condition => Get_Value (Data => Policy,
                                      Name => "system") = "",
              Message   => "String config value mismatch (empty string)");

      begin
         declare
            Dummy : constant String := Get_Value (Data => Policy,
                                                  Name => "nonexistent");
         begin
            Assert (Condition => False,
                    Message   => "Exception expected");
         end;

      exception
         when E : Not_Found =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "No string config option 'nonexistent' found",
                    Message   => "Exception message mismatch");
      end;
--  begin read only
   end Test_3_Get_Value;
--  end read only


--  begin read only
   procedure Test_Set_Value (Gnattest_T : in out Test);
   procedure Test_Set_Value_ae6688 (Gnattest_T : in out Test) renames Test_Set_Value;
--  id:2.2/ae6688d2e29689e0/Set_Value/1/0/
   procedure Test_Set_Value (Gnattest_T : in out Test) is
   --  mutools-system_config.ads:71:4:Set_Value
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Set_Value (Data  => Policy,
                 Name  => "foobar",
                 Value => True);
      Assert (Condition => Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/config/boolean[@name='foobar']",
               Name  => "value") = "true",
              Message   => "Value mismatch (1)");

      Set_Value (Data  => Policy,
                 Name  => "foobar",
                 Value => False);
      Assert (Condition => Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/config/boolean[@name='foobar']",
               Name  => "value") = "false",
              Message   => "Value mismatch (2)");
--  begin read only
   end Test_Set_Value;
--  end read only

end Mutools.System_Config.Test_Data.Tests;