--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mergers.Test_Data.

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
package body Mergers.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Merge_Hardware (Gnattest_T : in out Test);
   procedure Test_Merge_Hardware_1be979 (Gnattest_T : in out Test) renames Test_Merge_Hardware;
--  id:2.2/1be9794c0d96dee0/Merge_Hardware/1/0/
   procedure Test_Merge_Hardware (Gnattest_T : in out Test) is
   --  mergers.ads:25:4:Merge_Hardware
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Merge_Hardware
      is
         Filename     : constant String := "obj/merged_hardware.xml";
         Ref_Filename : constant String := "data/merged_hardware.xml";

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => "data/test_policy.xml");
         Merge_Hardware (Policy        => Policy,
                         Hardware_File => "data/hardware.xml");
         Muxml.Write (Data => Policy,
                      Kind => Muxml.None,
                      File => Filename);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => Ref_Filename),
                 Message   => "Policy mismatch: " & Filename);

         Ada.Directories.Delete_File (Name => Filename);
      end Merge_Hardware;

      ----------------------------------------------------------------------

      procedure Merge_Hardware_Null
      is
         Filename     : constant String := "obj/merged_hardware_null.xml";
         Ref_Filename : constant String := "data/merged_hardware_null.xml";

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => "data/test_policy.xml");
         Muxml.Utils.Remove_Child
           (Node       => DOM.Core.Nodes.First_Child (N => Policy.Doc),
            Child_Name => "hardware");

         Merge_Hardware (Policy        => Policy,
                         Hardware_File => "data/hardware.xml");
         Muxml.Write (Data => Policy,
                      Kind => Muxml.None,
                      File => Filename);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => Ref_Filename),
                 Message   => "Policy mismatch: " & Filename);

         Ada.Directories.Delete_File (Name => Filename);
      end Merge_Hardware_Null;
   begin
      Merge_Hardware;
      Merge_Hardware_Null;
--  begin read only
   end Test_Merge_Hardware;
--  end read only


--  begin read only
   procedure Test_Merge_Platform (Gnattest_T : in out Test);
   procedure Test_Merge_Platform_0cd3e6 (Gnattest_T : in out Test) renames Test_Merge_Platform;
--  id:2.2/0cd3e620cc856c4c/Merge_Platform/1/0/
   procedure Test_Merge_Platform (Gnattest_T : in out Test) is
   --  mergers.ads:30:4:Merge_Platform
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Merge_Platform
      is
         Filename     : constant String := "obj/merged_platform.xml";
         Ref_Filename : constant String := "data/merged_platform.xml";

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => "data/test_policy.xml");
         Merge_Platform (Policy        => Policy,
                         Platform_File => "data/platform.xml");
         Muxml.Write (Data => Policy,
                      Kind => Muxml.None,
                      File => Filename);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => Ref_Filename),
                 Message   => "Policy mismatch: " & Filename);

         Ada.Directories.Delete_File (Name => Filename);
      end Merge_Platform;

      ----------------------------------------------------------------------

      procedure Merge_Platform_Null
      is
         Filename     : constant String := "obj/merged_platform_null.xml";
         Ref_Filename : constant String := "data/merged_platform_null.xml";

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => "data/test_policy.xml");
         Muxml.Utils.Remove_Child
           (Node       => DOM.Core.Nodes.First_Child (N => Policy.Doc),
            Child_Name => "platform");

         Merge_Platform (Policy        => Policy,
                         Platform_File => "data/platform.xml");
         Muxml.Write (Data => Policy,
                      Kind => Muxml.None,
                      File => Filename);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => Ref_Filename),
                 Message   => "Policy mismatch: " & Filename);

         Ada.Directories.Delete_File (Name => Filename);
      end Merge_Platform_Null;
   begin
      Merge_Platform;
      Merge_Platform_Null;
--  begin read only
   end Test_Merge_Platform;
--  end read only


--  begin read only
   procedure Test_Merge_Platform_Config (Gnattest_T : in out Test);
   procedure Test_Merge_Platform_Config_7575a0 (Gnattest_T : in out Test) renames Test_Merge_Platform_Config;
--  id:2.2/7575a0b480cdf7ec/Merge_Platform_Config/1/0/
   procedure Test_Merge_Platform_Config (Gnattest_T : in out Test) is
   --  mergers.ads:36:4:Merge_Platform_Config
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Platform_Config
      is
         Filename     : constant String := "obj/merged_platform_config.xml";
         Ref_Filename : constant String := "data/merged_platform_config.xml";

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => "data/merged_platform.xml");

         Merge_Platform_Config (Policy => Policy);
         Muxml.Write (Data => Policy,
                      Kind => Muxml.None,
                      File => Filename);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => Ref_Filename),
                 Message   => "Policy mismatch (1): " & Filename);

         Ada.Directories.Delete_File (Name => Filename);
      end Platform_Config;

      ----------------------------------------------------------------------

      procedure Platform_Config_Value_Order
      is
         Filename     : constant String
           := "obj/merged_platform_config_order.xml";
         Ref_Filename : constant String
           := "data/merged_platform_config_order.xml";

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => "data/merged_platform.xml");
         Muxml.Utils.Remove_Child
           (Node       => Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system/config"),
            Child_Name => "integer");
         Merge_Platform_Config (Policy => Policy);
         Muxml.Write (Data => Policy,
                      Kind => Muxml.None,
                      File => Filename);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => Ref_Filename),
                 Message   => "Policy mismatch (3): " & Filename);

         Ada.Directories.Delete_File (Name => Filename);
      end Platform_Config_Value_Order;

      ----------------------------------------------------------------------

      procedure Platform_Without_Config
      is
         Filename     : constant String
           := "obj/merged_platform_no_config.xml";
         Ref_Filename : constant String
           := "data/merged_platform_no_config.xml";

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => "data/merged_platform.xml");

         Muxml.Utils.Remove_Child
           (Node       => Muxml.Utils.Get_Element (Doc   => Policy.Doc,
                                                   XPath => "/system/platform"),
            Child_Name => "config");

         Merge_Platform_Config (Policy => Policy);
         Muxml.Write (Data => Policy,
                      Kind => Muxml.None,
                      File => Filename);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => Ref_Filename),
                 Message   => "Policy mismatch (2): " & Filename);

         Ada.Directories.Delete_File (Name => Filename);
      end Platform_Without_Config;
   begin
      Platform_Config;
      Platform_Without_Config;
      Platform_Config_Value_Order;
--  begin read only
   end Test_Merge_Platform_Config;
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
end Mergers.Test_Data.Tests;
