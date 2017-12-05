--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Spec.VMX_Types.Test_Data.

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
package body Spec.VMX_Types.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Get_Pin_Controls (Gnattest_T : in out Test);
   procedure Test_Get_Pin_Controls_725b92 (Gnattest_T : in out Test) renames Test_Get_Pin_Controls;
--  id:2.2/725b929d399e5a1c/Get_Pin_Controls/1/0/
   procedure Test_Get_Pin_Controls (Gnattest_T : in out Test) is
   --  spec-vmx_types.ads:37:4:Get_Pin_Controls
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type interfaces.Unsigned_64;

      Policy : Muxml.XML_Data_Type;
      Nodes  : DOM.Core.Node_List;
   begin
      Assert (Condition => Get_Pin_Controls (Fields => Nodes) = 0,
              Message   => "Value mismatch (1)");
      Assert (Condition => Get_Pin_Controls
              (Fields  => Nodes,
               Default => 3) = 3,
              Message   => "Value mismatch (2)");

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Nodes := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject[@name='tau0']"
         & "/vcpu/vmx/controls/pin/*");

      Assert (Condition => Get_Pin_Controls (Fields => Nodes) = 2#0100_1001#,
              Message   => "Value mismatch (3)");

      --  Set unassigned bit to test mangling with default.

      Assert (Condition => Get_Pin_Controls
              (Fields  => Nodes,
               Default => 2) = 2#0100_1011#,
              Message   => "Value mismatch (4)");
--  begin read only
   end Test_Get_Pin_Controls;
--  end read only


--  begin read only
   procedure Test_Get_Proc_Controls (Gnattest_T : in out Test);
   procedure Test_Get_Proc_Controls_4f4761 (Gnattest_T : in out Test) renames Test_Get_Proc_Controls;
--  id:2.2/4f4761031b17916f/Get_Proc_Controls/1/0/
   procedure Test_Get_Proc_Controls (Gnattest_T : in out Test) is
   --  spec-vmx_types.ads:75:4:Get_Proc_Controls
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type interfaces.Unsigned_64;

      Policy : Muxml.XML_Data_Type;
      Nodes  : DOM.Core.Node_List;
   begin
      Assert (Condition => Get_Proc_Controls (Fields => Nodes) = 0,
              Message   => "Value mismatch (1)");
      Assert (Condition => Get_Proc_Controls
              (Fields  => Nodes,
               Default => 53) = 53,
              Message   => "Value mismatch (2)");

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Nodes := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject[@name='tau0']"
         & "/vcpu/vmx/controls/proc/*");

      Assert (Condition => Get_Proc_Controls (Fields => Nodes)
              = 2#1011_0010_1001_1001_1001_1110_0000_0000#,
              Message   => "Value mismatch (3)");

      --  Set unassigned bit to test mangling with default.

      Assert (Condition => Get_Proc_Controls
              (Fields  => Nodes,
               Default => 1)
              = 2#1011_0010_1001_1001_1001_1110_0000_0001#,
              Message   => "Value mismatch (4)");
--  begin read only
   end Test_Get_Proc_Controls;
--  end read only


--  begin read only
   procedure Test_Get_Proc2_Controls (Gnattest_T : in out Test);
   procedure Test_Get_Proc2_Controls_d0b89f (Gnattest_T : in out Test) renames Test_Get_Proc2_Controls;
--  id:2.2/d0b89fb21b402084/Get_Proc2_Controls/1/0/
   procedure Test_Get_Proc2_Controls (Gnattest_T : in out Test) is
   --  spec-vmx_types.ads:122:4:Get_Proc2_Controls
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type interfaces.Unsigned_64;

      Policy : Muxml.XML_Data_Type;
      Nodes  : DOM.Core.Node_List;
   begin
      Assert (Condition => Get_Proc2_Controls (Fields => Nodes) = 0,
              Message   => "Value mismatch (1)");
      Assert (Condition => Get_Proc2_Controls
              (Fields  => Nodes,
               Default => 5653) = 5653,
              Message   => "Value mismatch (2)");

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Nodes := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject[@name='tau0']"
         & "/vcpu/vmx/controls/proc2/*");

      Assert (Condition => Get_Proc2_Controls (Fields => Nodes)
              = 2#0100_0000#,
              Message   => "Value mismatch (3)");
--  begin read only
   end Test_Get_Proc2_Controls;
--  end read only


--  begin read only
   procedure Test_Get_Entry_Controls (Gnattest_T : in out Test);
   procedure Test_Get_Entry_Controls_bde183 (Gnattest_T : in out Test) renames Test_Get_Entry_Controls;
--  id:2.2/bde1831ce58d7fe3/Get_Entry_Controls/1/0/
   procedure Test_Get_Entry_Controls (Gnattest_T : in out Test) is
   --  spec-vmx_types.ads:155:4:Get_Entry_Controls
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type interfaces.Unsigned_64;

      Policy : Muxml.XML_Data_Type;
      Nodes  : DOM.Core.Node_List;
   begin
      Assert (Condition => Get_Entry_Controls (Fields => Nodes) = 0,
              Message   => "Value mismatch (1)");
      Assert (Condition => Get_Entry_Controls
              (Fields  => Nodes,
               Default => 111) = 111,
              Message   => "Value mismatch (2)");

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Nodes := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject[@name='tau0']"
         & "/vcpu/vmx/controls/entry/*");

      Assert (Condition => Get_Entry_Controls (Fields => Nodes)
              = 2#0010_0000_0000#,
              Message   => "Value mismatch (3)");
--  begin read only
   end Test_Get_Entry_Controls;
--  end read only


--  begin read only
   procedure Test_Get_Exit_Controls (Gnattest_T : in out Test);
   procedure Test_Get_Exit_Controls_779aa6 (Gnattest_T : in out Test) renames Test_Get_Exit_Controls;
--  id:2.2/779aa60f8679631c/Get_Exit_Controls/1/0/
   procedure Test_Get_Exit_Controls (Gnattest_T : in out Test) is
   --  spec-vmx_types.ads:183:4:Get_Exit_Controls
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type interfaces.Unsigned_64;

      Policy : Muxml.XML_Data_Type;
      Nodes  : DOM.Core.Node_List;
   begin
      Assert (Condition => Get_Exit_Controls (Fields => Nodes) = 0,
              Message   => "Value mismatch (1)");
      Assert (Condition => Get_Exit_Controls
              (Fields  => Nodes,
               Default => 1112) = 1112,
              Message   => "Value mismatch (2)");

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Nodes := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject[@name='tau0']"
         & "/vcpu/vmx/controls/exit/*");

      Assert (Condition => Get_Exit_Controls (Fields => Nodes)
              = 2#0100_0000_1000_0010_0000_0000#,
              Message   => "Value mismatch (3)");
--  begin read only
   end Test_Get_Exit_Controls;
--  end read only


--  begin read only
   procedure Test_Get_CR0 (Gnattest_T : in out Test);
   procedure Test_Get_CR0_a27155 (Gnattest_T : in out Test) renames Test_Get_CR0;
--  id:2.2/a2715543face5b96/Get_CR0/1/0/
   procedure Test_Get_CR0 (Gnattest_T : in out Test) is
   --  spec-vmx_types.ads:213:4:Get_CR0
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type interfaces.Unsigned_64;

      Policy : Muxml.XML_Data_Type;
      Nodes  : DOM.Core.Node_List;
   begin
      Assert (Condition => Get_CR0 (Fields => Nodes) = 0,
              Message   => "Value mismatch (1)");
      Assert (Condition => Get_CR0
              (Fields  => Nodes,
               Default => 11123) = 11123,
              Message   => "Value mismatch (2)");

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Nodes := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject[@name='tau0']"
         & "/vcpu/registers/cr0/*");

      Assert (Condition => Get_CR0 (Fields => Nodes)
              = 2#1000_0000_0000_0001_0000_0000_0011_0101#,
              Message   => "Value mismatch (3)");
--  begin read only
   end Test_Get_CR0;
--  end read only


--  begin read only
   procedure Test_Get_CR4 (Gnattest_T : in out Test);
   procedure Test_Get_CR4_5f36b7 (Gnattest_T : in out Test) renames Test_Get_CR4;
--  id:2.2/5f36b7a10eb73457/Get_CR4/1/0/
   procedure Test_Get_CR4 (Gnattest_T : in out Test) is
   --  spec-vmx_types.ads:255:4:Get_CR4
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type interfaces.Unsigned_64;

      Policy : Muxml.XML_Data_Type;
      Nodes  : DOM.Core.Node_List;
   begin
      Assert (Condition => Get_CR4 (Fields => Nodes) = 0,
              Message   => "Value mismatch (1)");
      Assert (Condition => Get_CR4
              (Fields  => Nodes,
               Default => 111232) = 111232,
              Message   => "Value mismatch (2)");

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Nodes := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject[@name='tau0']"
         & "/vcpu/registers/cr4/*");

      Assert (Condition => Get_CR4 (Fields => Nodes)
              = 2#0010_0000_0010_0000#,
              Message   => "Value mismatch (3)");
--  begin read only
   end Test_Get_CR4;
--  end read only


--  begin read only
   procedure Test_Get_Exceptions (Gnattest_T : in out Test);
   procedure Test_Get_Exceptions_fe16dd (Gnattest_T : in out Test) renames Test_Get_Exceptions;
--  id:2.2/fe16dd3e82a9e7f0/Get_Exceptions/1/0/
   procedure Test_Get_Exceptions (Gnattest_T : in out Test) is
   --  spec-vmx_types.ads:304:4:Get_Exceptions
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type interfaces.Unsigned_64;

      Policy : Muxml.XML_Data_Type;
      Nodes  : DOM.Core.Node_List;
   begin
      Assert (Condition => Get_Exceptions (Fields => Nodes) = 0,
              Message   => "Value mismatch (1)");
      Assert (Condition => Get_Exceptions
              (Fields  => Nodes,
               Default => 366) = 366,
              Message   => "Value mismatch (2)");

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Nodes := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject[@name='tau0']"
         & "/vcpu/vmx/masks/exception/*");

      Assert (Condition => Get_Exceptions (Fields => Nodes)
              = 2#1111_0111_1111_1111_1011#,
              Message   => "Value mismatch (3)");
--  begin read only
   end Test_Get_Exceptions;
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
end Spec.VMX_Types.Test_Data.Tests;
