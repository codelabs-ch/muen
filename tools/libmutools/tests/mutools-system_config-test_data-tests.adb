--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.System_Config.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Mutools.System_Config.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Has_Boolean (Gnattest_T : in out Test);
   procedure Test_Has_Boolean_89c551 (Gnattest_T : in out Test) renames Test_Has_Boolean;
--  id:2.2/89c551e2c6b63d90/Has_Boolean/1/0/
   procedure Test_Has_Boolean (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Assert (Condition => Has_Boolean (Data => Policy,
                                        Name => "feature_enabled"),
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
                                            Name => "feature_enabled"),
              Message   => "Integer config variable present (type mismatch)");
--  begin read only
   end Test_Has_Integer;
--  end read only


--  begin read only
   procedure Test_Has_String (Gnattest_T : in out Test);
   procedure Test_Has_String_06e83f (Gnattest_T : in out Test) renames Test_Has_String;
--  id:2.2/06e83fa4dda1248c/Has_String/1/0/
   procedure Test_Has_String (Gnattest_T : in out Test) is
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
                                      Name => "feature_enabled"),
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
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Assert (Condition => Get_Value (Data => Policy,
                                      Name => "feature_enabled"),
              Message   => "Boolean config value false");

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/config/boolean[@name=""feature_enabled""]",
         Name  => "value",
         Value => "false");
      Assert (Condition => not Get_Value (Data => Policy,
                                      Name => "feature_enabled"),
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
   procedure Test_Get_Raw_Value (Gnattest_T : in out Test);
   procedure Test_Get_Raw_Value_e1d557 (Gnattest_T : in out Test) renames Test_Get_Raw_Value;
--  id:2.2/e1d557913a4e751b/Get_Raw_Value/1/0/
   procedure Test_Get_Raw_Value (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Assert (Condition => Get_Raw_Value
              (Data => Policy,
               Name => "feature_enabled") = "true",
              Message   => "Raw config value mismatch (1)");
      Assert (Condition => Get_Raw_Value
              (Data => Policy,
               Name => "session_count") = "4",
              Message   => "Raw config value mismatch (2)");
      Assert (Condition => Get_Raw_Value
              (Data => Policy,
               Name => "system") = "test_system.xml",
              Message   => "Raw config value mismatch (3)");

      begin
         declare
            Dummy : constant String := Get_Raw_Value
              (Data => Policy,
               Name => "nonexistent");
         begin
            Assert (Condition => False,
                    Message   => "Exception expected");
         end;

      exception
         when E : Not_Found =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "No config option 'nonexistent' found",
                    Message   => "Exception message mismatch");
      end;
--  begin read only
   end Test_Get_Raw_Value;
--  end read only


--  begin read only
   procedure Test_1_Set_Value (Gnattest_T : in out Test);
   procedure Test_Set_Value_ae6688 (Gnattest_T : in out Test) renames Test_1_Set_Value;
--  id:2.2/ae6688d2e29689e0/Set_Value/1/0/
   procedure Test_1_Set_Value (Gnattest_T : in out Test) is
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

      declare
         Cfgs : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (N     => Policy.Doc,
            XPath => "/system/config/*");
         Before : Boolean := True;
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Cfgs) - 1 loop
            declare
               Cur_Node : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => Cfgs,
                    Index => I);
               Tag  : constant String := DOM.Core.Nodes.Node_Name
                 (N => Cur_Node);
               Name : constant String := DOM.Core.Elements.Get_Attribute
                 (Elem => Cur_Node,
                  Name => "name");
            begin
               if Name = "foobar" and then Tag = "boolean" then
                  Before := False;
               elsif Before then
                  Assert (Condition => Tag = "boolean",
                          Message   => "Set boolean preceded by " & Tag
                          & " element " & Name);
               else
                  Assert (Condition => Tag /= "boolean",
                          Message   => "Set boolean followed by " &  Tag
                          & " element " & Name);
               end if;
            end;
         end loop;
      end;

      Set_Value (Data  => Policy,
                 Name  => "foobar",
                 Value => False);
      Assert (Condition => Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/config/boolean[@name='foobar']",
               Name  => "value") = "false",
              Message   => "Value mismatch (2)");
--  begin read only
   end Test_1_Set_Value;
--  end read only


--  begin read only
   procedure Test_2_Set_Value (Gnattest_T : in out Test);
   procedure Test_Set_Value_9fa904 (Gnattest_T : in out Test) renames Test_2_Set_Value;
--  id:2.2/9fa904c55ac3a46c/Set_Value/0/0/
   procedure Test_2_Set_Value (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Node   : DOM.Core.Node;
      Node_List : DOM.Core.Node_List;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Set_Value (Data  => Policy,
                 Name  => "new_name",
                 Value => "some_String");
      Node_List := McKae.XML.XPath.XIA.XPath_Query
           (N     => Policy.Doc,
            XPath => "//*[@name='new_name']");
      Assert (Condition => DOM.Core.Nodes.Length (List => Node_List) = 1,
              Message   => "More than one element created by Set_Value");

      Node := DOM.Core.Nodes.Item (List => Node_List, Index => 0);
      Assert (Condition => DOM.Core.Elements.Get_Attribute
                              (Elem => Node,
                               Name => "value") = "some_String",
              Message => "Set_Value assigned '"
              & DOM.Core.Elements.Get_Attribute
                              (Elem => Node,
                               Name => "value")
             & "' instead of 'some_String'");

       Set_Value (Data  => Policy,
                  Name  => "new_name",
                  Value => "another_String");
       Assert (Condition => DOM.Core.Elements.Get_Attribute
                              (Elem => Node,
                               Name => "value") = "another_String",
              Message => "Set_Value assigned '"
              & DOM.Core.Elements.Get_Attribute
                              (Elem => Node,
                               Name => "value")
             & "' instead of 'another_String'");

--  begin read only
   end Test_2_Set_Value;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Mutools.System_Config.Test_Data.Tests;
