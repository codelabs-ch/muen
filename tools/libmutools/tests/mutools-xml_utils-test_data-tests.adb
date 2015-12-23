--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.XML_Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mutools.XML_Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Add_Resource (Gnattest_T : in out Test);
   procedure Test_Add_Resource_848eaf (Gnattest_T : in out Test) renames Test_Add_Resource;
--  id:2.2/848eafd2990f0db4/Add_Resource/1/0/
   procedure Test_Add_Resource (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:31:4:Add_Resource
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Add_IRQ_Resource;
      procedure Add_IO_Port_Resource;
      procedure Add_Memory_Resource;

      ----------------------------------------------------------------------

        procedure Add_IO_Port_Resource
      is
         Dom_Impl : DOM.Core.DOM_Implementation;
         Policy   : Muxml.XML_Data_Type;
         Resource : DOM.Core.Node;
         Dev_Node : DOM.Core.Node;

         Ref_Name : constant String := "port1";
      begin
         Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);

         Resource := DOM.Core.Documents.Create_Element
             (Doc      => Policy.Doc,
              Tag_Name => "ioPort");
         DOM.Core.Elements.Set_Attribute (Elem  => Resource,
                                          Name  => "name",
                                          Value => Ref_Name);

         Dev_Node := DOM.Core.Documents.Create_Element
             (Doc      => Policy.Doc,
              Tag_Name => "device");

         Add_Resource (Logical_Device    => Dev_Node,
                       Physical_Resource => Resource);
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Dev_Node,
                  XPath => "ioPort",
                  Name  => "physical") = Ref_Name,
                 Message   => "I/O port physical name mismatch");
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Dev_Node,
                  XPath => "ioPort",
                  Name  => "logical") = Ref_Name,
                 Message   => "I/O port name mismatch");
      end Add_IO_Port_Resource;

      ----------------------------------------------------------------------

      procedure Add_IRQ_Resource
      is
         Dom_Impl : DOM.Core.DOM_Implementation;
         Policy   : Muxml.XML_Data_Type;
         Resource : DOM.Core.Node;
         Dev_Node : DOM.Core.Node;

         Ref_Name : constant String := "irq1";
         Ref_Num  : constant String := "143";
      begin
         Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);

         Resource := DOM.Core.Documents.Create_Element
             (Doc      => Policy.Doc,
              Tag_Name => "irq");
         DOM.Core.Elements.Set_Attribute (Elem  => Resource,
                                          Name  => "name",
                                          Value => Ref_Name);
         DOM.Core.Elements.Set_Attribute (Elem  => Resource,
                                          Name  => "number",
                                          Value => Ref_Num);

         Dev_Node := DOM.Core.Documents.Create_Element
             (Doc      => Policy.Doc,
              Tag_Name => "device");

         Add_Resource (Logical_Device    => Dev_Node,
                       Physical_Resource => Resource);
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Dev_Node,
                  XPath => "irq",
                  Name  => "physical") = Ref_Name,
                 Message   => "IRQ physical name mismatch");
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Dev_Node,
                  XPath => "irq",
                  Name  => "logical") = Ref_Name,
                 Message   => "IRQ name mismatch");
      end Add_IRQ_Resource;

      ----------------------------------------------------------------------

      procedure Add_Memory_Resource
      is
         Dom_Impl : DOM.Core.DOM_Implementation;
         Policy   : Muxml.XML_Data_Type;
         Resource : DOM.Core.Node;
         Dev_Node : DOM.Core.Node;

         Ref_Addr : constant String := "16#beef_2000#";
         Ref_Name : constant String := "mmio1";
      begin
         Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);

         Resource := DOM.Core.Documents.Create_Element
             (Doc      => Policy.Doc,
              Tag_Name => "memory");
         DOM.Core.Elements.Set_Attribute (Elem  => Resource,
                                          Name  => "name",
                                          Value => Ref_Name);
         DOM.Core.Elements.Set_Attribute (Elem  => Resource,
                                          Name  => "physicalAddress",
                                          Value => Ref_Addr);
         DOM.Core.Elements.Set_Attribute (Elem  => Resource,
                                          Name  => "size",
                                          Value => "16#2000#");
         DOM.Core.Elements.Set_Attribute (Elem  => Resource,
                                          Name  => "caching",
                                          Value => "UC");

         Dev_Node := DOM.Core.Documents.Create_Element
             (Doc      => Policy.Doc,
              Tag_Name => "device");

         Add_Resource (Logical_Device    => Dev_Node,
                       Physical_Resource => Resource);
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Dev_Node,
                  XPath => "memory",
                  Name  => "physical") = Ref_Name,
                 Message   => "Memory physical name mismatch");
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Dev_Node,
                  XPath => "memory",
                  Name  => "logical") = Ref_Name,
                 Message   => "Memory logical name mismatch");
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Dev_Node,
                  XPath => "memory",
                  Name  => "virtualAddress") = Ref_Addr,
                 Message   => "Memory virtual address mismatch");
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Dev_Node,
                  XPath => "memory",
                  Name  => "executable") = "false",
                 Message   => "Memory is executable");
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Dev_Node,
                  XPath => "memory",
                  Name  => "writable") = "true",
                 Message   => "Memory is not writable");
      end Add_Memory_Resource;
   begin
      Add_IRQ_Resource;
      Add_IO_Port_Resource;
      Add_Memory_Resource;
--  begin read only
   end Test_Add_Resource;
--  end read only


--  begin read only
   procedure Test_1_Add_Memory_Region (Gnattest_T : in out Test);
   procedure Test_Add_Memory_Region_6142bd (Gnattest_T : in out Test) renames Test_1_Add_Memory_Region;
--  id:2.2/6142bd7e03979890/Add_Memory_Region/1/0/
   procedure Test_1_Add_Memory_Region (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:36:4:Add_Memory_Region
--  end read only

      pragma Unreferenced (Gnattest_T);

      Filename : constant String := "obj/memory.xml";
      Policy   : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Add_Memory_Region
        (Policy      => Policy,
         Name        => "test",
         Address     => "16#9000_1000#",
         Size        => "16#3000#",
         Caching     => "UC",
         Alignment   => "16#1000#",
         Memory_Type => "");
      Add_Memory_Region
        (Policy      => Policy,
         Name        => "noaddress",
         Address     => "",
         Size        => "16#8000#",
         Caching     => "WC",
         Alignment   => "16#0020_0000#",
         Memory_Type => "");

      Muxml.Write (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => Filename);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Filename,
               Filename2 => "data/memory.xml"),
              Message   => "Policy mismatch");

      Ada.Directories.Delete_File (Name => Filename);
--  begin read only
   end Test_1_Add_Memory_Region;
--  end read only


--  begin read only
   procedure Test_2_Add_Memory_Region (Gnattest_T : in out Test);
   procedure Test_Add_Memory_Region_741476 (Gnattest_T : in out Test) renames Test_2_Add_Memory_Region;
--  id:2.2/741476e4715f4faa/Add_Memory_Region/0/0/
   procedure Test_2_Add_Memory_Region (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:47:4:Add_Memory_Region
--  end read only

      pragma Unreferenced (Gnattest_T);

      Filename : constant String := "obj/memory_with_file.xml";
      Policy   : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Add_Memory_Region
        (Policy      => Policy,
         Name        => "test",
         Address     => "16#2000#",
         Size        => "16#4000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "",
         File_Name   => "testfile",
         File_Offset => "16#1000#");

      Muxml.Write (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => Filename);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Filename,
               Filename2 => "data/memory_with_file.xml"),
              Message   => "Policy mismatch");

      Ada.Directories.Delete_File (Name => Filename);
--  begin read only
   end Test_2_Add_Memory_Region;
--  end read only


--  begin read only
   procedure Test_3_Add_Memory_Region (Gnattest_T : in out Test);
   procedure Test_Add_Memory_Region_4bc9f0 (Gnattest_T : in out Test) renames Test_3_Add_Memory_Region;
--  id:2.2/4bc9f02de0465231/Add_Memory_Region/0/0/
   procedure Test_3_Add_Memory_Region (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:60:4:Add_Memory_Region
--  end read only

      pragma Unreferenced (Gnattest_T);

      Filename : constant String := "obj/memory_with_fill.xml";
      Policy   : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Add_Memory_Region
        (Policy       => Policy,
         Name         => "test",
         Address      => "16#2000#",
         Size         => "16#4000#",
         Caching      => "WB",
         Alignment    => "16#1000#",
         Memory_Type  => "",
         Fill_Pattern => "16#fe#");

      Muxml.Write (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => Filename);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Filename,
               Filename2 => "data/memory_with_fill.xml"),
              Message   => "Policy mismatch");

      Ada.Directories.Delete_File (Name => Filename);
--  begin read only
   end Test_3_Add_Memory_Region;
--  end read only


--  begin read only
   procedure Test_Create_Memory_Node (Gnattest_T : in out Test);
   procedure Test_Create_Memory_Node_4c3d32 (Gnattest_T : in out Test) renames Test_Create_Memory_Node;
--  id:2.2/4c3d32628a5ec36a/Create_Memory_Node/1/0/
   procedure Test_Create_Memory_Node (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:71:4:Create_Memory_Node
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := Create_Memory_Node (Policy      => Data,
                                  Name        => "region1",
                                  Address     => "16#1000#",
                                  Size        => "16#0200_1000#",
                                  Caching     => "WB",
                                  Alignment   => "16#1000#",
                                  Memory_Type => "subject");

      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "name") = "region1",
              Message   => "Name mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "physicalAddress") = "16#1000#",
              Message   => "Address mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "size") = "16#0200_1000#",
              Message   => "Size mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "caching") = "WB",
              Message   => "Caching mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "alignment") = "16#1000#",
              Message   => "Alignment mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "type") = "subject",
              Message   => "Memory type mismatch");

      Node := Create_Memory_Node (Policy      => Data,
                                  Name        => "region2",
                                  Address     => "",
                                  Size        => "16#1000#",
                                  Caching     => "UC",
                                  Alignment   => "",
                                  Memory_Type => "");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "physicalAddress") = "",
              Message   => "Address set");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "alignment") = "",
              Message   => "Alignment set");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "type") = "",
              Message   => "Memory type set");


--  begin read only
   end Test_Create_Memory_Node;
--  end read only


--  begin read only
   procedure Test_Create_Virtual_Memory_Node (Gnattest_T : in out Test);
   procedure Test_Create_Virtual_Memory_Node_b6e99c (Gnattest_T : in out Test) renames Test_Create_Virtual_Memory_Node;
--  id:2.2/b6e99c7daf116e37/Create_Virtual_Memory_Node/1/0/
   procedure Test_Create_Virtual_Memory_Node (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:82:4:Create_Virtual_Memory_Node
--  end read only

      pragma Unreferenced (Gnattest_T);

      Dom_Impl : DOM.Core.DOM_Implementation;
      Policy   : Muxml.XML_Data_Type;
      Node     : DOM.Core.Node;
      Logical  : constant String := "testl";
      Physical : constant String := "testp";
      Address  : constant String := "16#2000#";
   begin
      Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);

      Node := Create_Virtual_Memory_Node
        (Policy        => Policy,
         Logical_Name  => Logical,
         Physical_Name => Physical,
         Address       => Address,
         Writable      => True,
         Executable    => False);

      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "logical") = Logical,
              Message   => "Logical name mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "physical") = "testp",
              Message   => "Physical name mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "virtualAddress") = Address,
              Message   => "Address mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "writable") = "true",
              Message   => "Writable mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "executable") = "false",
              Message   => "Executable mismatch");
--  begin read only
   end Test_Create_Virtual_Memory_Node;
--  end read only


--  begin read only
   procedure Test_Has_Managed_DEBUGCTL (Gnattest_T : in out Test);
   procedure Test_Has_Managed_DEBUGCTL_07c840 (Gnattest_T : in out Test) renames Test_Has_Managed_DEBUGCTL;
--  id:2.2/07c840ea4cf93188/Has_Managed_DEBUGCTL/1/0/
   procedure Test_Has_Managed_DEBUGCTL (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:93:4:Has_Managed_DEBUGCTL
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Ctrls  : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Ctrls := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/vcpu/vmx/controls");

      Assert (Condition => not Has_Managed_DEBUGCTL (Controls => Ctrls),
              Message   => "DEBUGCTL is managed");

      DOM.Core.Nodes.Set_Node_Value
        (N     => DOM.Core.Nodes.First_Child
           (N => Muxml.Utils.Get_Element
                (Doc   => Ctrls,
                 XPath => "entry/LoadDebugControls")),
         Value => "1");
      DOM.Core.Nodes.Set_Node_Value
        (N     => DOM.Core.Nodes.First_Child
           (N => Muxml.Utils.Get_Element
                (Doc   => Ctrls,
                 XPath => "exit/SaveDebugControls")),
         Value => "1");
      Assert (Condition => Has_Managed_DEBUGCTL (Controls => Ctrls),
              Message   => "DEBUGCTL not managed");
--  begin read only
   end Test_Has_Managed_DEBUGCTL;
--  end read only


--  begin read only
   procedure Test_Has_Managed_PERFGLOBALCTRL (Gnattest_T : in out Test);
   procedure Test_Has_Managed_PERFGLOBALCTRL_811a8a (Gnattest_T : in out Test) renames Test_Has_Managed_PERFGLOBALCTRL;
--  id:2.2/811a8a093040ed89/Has_Managed_PERFGLOBALCTRL/1/0/
   procedure Test_Has_Managed_PERFGLOBALCTRL (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:97:4:Has_Managed_PERFGLOBALCTRL
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Ctrls  : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Ctrls := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/vcpu/vmx/controls");

      Assert (Condition => not Has_Managed_PERFGLOBALCTRL (Controls => Ctrls),
              Message   => "PERFGLOBALCTL is managed");

      DOM.Core.Nodes.Set_Node_Value
        (N     => DOM.Core.Nodes.First_Child
           (N => Muxml.Utils.Get_Element
                (Doc   => Ctrls,
                 XPath => "entry/LoadIA32PERFGLOBALCTRL")),
         Value => "1");
      Assert (Condition => Has_Managed_PERFGLOBALCTRL (Controls => Ctrls),
              Message   => "DEBUGCTL not managed");
--  begin read only
   end Test_Has_Managed_PERFGLOBALCTRL;
--  end read only


--  begin read only
   procedure Test_Has_Managed_PAT (Gnattest_T : in out Test);
   procedure Test_Has_Managed_PAT_0e0b54 (Gnattest_T : in out Test) renames Test_Has_Managed_PAT;
--  id:2.2/0e0b54fd46c7da60/Has_Managed_PAT/1/0/
   procedure Test_Has_Managed_PAT (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:103:4:Has_Managed_PAT
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Ctrls  : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Ctrls := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/vcpu/vmx/controls");

      Assert (Condition => not Has_Managed_PAT (Controls => Ctrls),
              Message   => "PAT is managed");

      DOM.Core.Nodes.Set_Node_Value
        (N     => DOM.Core.Nodes.First_Child
           (N => Muxml.Utils.Get_Element
                (Doc   => Ctrls,
                 XPath => "entry/LoadIA32PAT")),
         Value => "1");
      DOM.Core.Nodes.Set_Node_Value
        (N     => DOM.Core.Nodes.First_Child
           (N => Muxml.Utils.Get_Element
                (Doc   => Ctrls,
                 XPath => "exit/SaveIA32PAT")),
         Value => "1");
      Assert (Condition => Has_Managed_PAT (Controls => Ctrls),
              Message   => "PAT not managed");
--  begin read only
   end Test_Has_Managed_PAT;
--  end read only


--  begin read only
   procedure Test_Has_Managed_EFER (Gnattest_T : in out Test);
   procedure Test_Has_Managed_EFER_29e528 (Gnattest_T : in out Test) renames Test_Has_Managed_EFER;
--  id:2.2/29e528793cfc9400/Has_Managed_EFER/1/0/
   procedure Test_Has_Managed_EFER (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:107:4:Has_Managed_EFER
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Ctrls  : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Ctrls := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/vcpu/vmx/controls");

      Assert (Condition => not Has_Managed_EFER (Controls => Ctrls),
              Message   => "EFER is managed");

      DOM.Core.Nodes.Set_Node_Value
        (N     => DOM.Core.Nodes.First_Child
           (N => Muxml.Utils.Get_Element
                (Doc   => Ctrls,
                 XPath => "entry/LoadIA32EFER")),
         Value => "1");
      DOM.Core.Nodes.Set_Node_Value
        (N     => DOM.Core.Nodes.First_Child
           (N => Muxml.Utils.Get_Element
                (Doc   => Ctrls,
                 XPath => "exit/SaveIA32EFER")),
         Value => "1");
      Assert (Condition => Has_Managed_EFER (Controls => Ctrls),
              Message   => "EFER not managed");
--  begin read only
   end Test_Has_Managed_EFER;
--  end read only


--  begin read only
   procedure Test_Calculate_MSR_Count (Gnattest_T : in out Test);
   procedure Test_Calculate_MSR_Count_5d62ce (Gnattest_T : in out Test) renames Test_Calculate_MSR_Count;
--  id:2.2/5d62ce190007559a/Calculate_MSR_Count/1/0/
   procedure Test_Calculate_MSR_Count (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:112:4:Calculate_MSR_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      MSRs : DOM.Core.Node_List;

      --  Append MSR with given attributes to MSR list.
      procedure Append_MSR
        (MSR_Start : String;
         MSR_End   : String;
         Mode      : String)
      is
         Node : DOM.Core.Node;
      begin
         Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "msr");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "mode",
            Value => Mode);
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "start",
            Value => MSR_Start);
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "end",
            Value => MSR_End);

         DOM.Core.Append_Node (List => MSRs,
                               N    => Node);
      end Append_MSR;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Assert (Condition => Calculate_MSR_Count
              (MSRs                   => MSRs,
               DEBUGCTL_Control       => False,
               PAT_Control            => False,
               PERFGLOBALCTRL_Control => False,
               EFER_Control           => False) = 0,
              Message   => "Empty list count not 0");

      Append_MSR (MSR_Start => "16#0010#",
                  MSR_End   => "16#0010#",
                  Mode      => "r");
      Append_MSR (MSR_Start => "16#0174#",
                  MSR_End   => "16#0176#",
                  Mode      => "rw");
      Append_MSR (MSR_Start => "16#c000_0080#",
                  MSR_End   => "16#c000_0084#",
                  Mode      => "rw");
      Append_MSR (MSR_Start => "16#c000_0100#",
                  MSR_End   => "16#c000_0102#",
                  Mode      => "rw");

      Assert (Condition => Calculate_MSR_Count
              (MSRs                   => MSRs,
               DEBUGCTL_Control       => False,
               PAT_Control            => False,
               PERFGLOBALCTRL_Control => False,
               EFER_Control           => True) = 6,
              Message   => "MSR count mismatch");
--  begin read only
   end Test_Calculate_MSR_Count;
--  end read only


--  begin read only
   procedure Test_Get_Occupied_PCI_Buses (Gnattest_T : in out Test);
   procedure Test_Get_Occupied_PCI_Buses_0b9ce6 (Gnattest_T : in out Test) renames Test_Get_Occupied_PCI_Buses;
--  id:2.2/0b9ce63ef86880a3/Get_Occupied_PCI_Buses/1/0/
   procedure Test_Get_Occupied_PCI_Buses (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:126:4:Get_Occupied_PCI_Buses
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Containers.Count_Type;

      Policy : Muxml.XML_Data_Type;
      Buses  : PCI_Bus_Set.Set;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Buses := Get_Occupied_PCI_Buses (Data => Policy);

      Assert (Condition => Buses.Length = 2,
              Message   => "Bus count not 2");
      Assert (Condition => Buses.First_Element = 16#02#,
              Message   => "First bus not 16#02#");
      Assert (Condition => Buses.Last_Element = 16#23#,
              Message   => "Last bus not 16#23#");
--  begin read only
   end Test_Get_Occupied_PCI_Buses;
--  end read only


--  begin read only
   procedure Test_Get_Switch_Sources (Gnattest_T : in out Test);
   procedure Test_Get_Switch_Sources_e0b744 (Gnattest_T : in out Test) renames Test_Get_Switch_Sources;
--  id:2.2/e0b744f992ef1869/Get_Switch_Sources/1/0/
   procedure Test_Get_Switch_Sources (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:132:4:Get_Switch_Sources
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy  : Muxml.XML_Data_Type;
      Sources : DOM.Core.Node_List;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/switch_events.xml");

      Sources := Get_Switch_Sources
        (Data   => Policy,
         Target => Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/subjects/subject[@name='subj1']"));
      Assert (Condition => DOM.Core.Nodes.Length (List => Sources) = 1,
              Message   => "Missing source subjects (1)");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => DOM.Core.Nodes.Item (List  => Sources,
                                            Index => 0),
               Name => "name") = "subj3",
              Message   => "Subj1 source switch mismatch");

      Sources := Get_Switch_Sources
        (Data   => Policy,
         Target => Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/subjects/subject[@name='subj3']"));
      Assert (Condition => DOM.Core.Nodes.Length (List => Sources) = 2,
              Message   => "Missing source subjects (2)");
--  begin read only
   end Test_Get_Switch_Sources;
--  end read only


--  begin read only
   procedure Test_Get_Active_CPU_Count (Gnattest_T : in out Test);
   procedure Test_Get_Active_CPU_Count_8e636b (Gnattest_T : in out Test) renames Test_Get_Active_CPU_Count;
--  id:2.2/8e636bf46ceb65f2/Get_Active_CPU_Count/1/0/
   procedure Test_Get_Active_CPU_Count (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:138:4:Get_Active_CPU_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Assert (Condition => Get_Active_CPU_Count (Data => Policy) = 2,
              Message   => "Active CPU count mismatch");
--  begin read only
   end Test_Get_Active_CPU_Count;
--  end read only


--  begin read only
   procedure Test_Get_Executing_CPU (Gnattest_T : in out Test);
   procedure Test_Get_Executing_CPU_1c35bd (Gnattest_T : in out Test) renames Test_Get_Executing_CPU;
--  id:2.2/1c35bd06b291292c/Get_Executing_CPU/1/0/
   procedure Test_Get_Executing_CPU (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:143:4:Get_Executing_CPU
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_Src,
                   File => "data/switch-event-chains.xml");

      Assert (Condition => Get_Executing_CPU
              (Data    => Data,
               Subject => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/subjects/subject[@name='subj1']")) = 0,
              Message   => "Subj1 CPU mismatch");

      Assert (Condition => Get_Executing_CPU
              (Data    => Data,
               Subject => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/subjects/subject[@name='subj2']")) = 0,
              Message   => "Subj2 CPU mismatch");
      Assert (Condition => Get_Executing_CPU
              (Data    => Data,
               Subject => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/subjects/subject[@name='subj3']")) = 0,
              Message   => "Subj3 CPU mismatch");
      Assert (Condition => Get_Executing_CPU
              (Data    => Data,
               Subject => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/subjects/subject[@name='subj4']")) = 0,
              Message   => "Subj4 CPU mismatch (1)");
      Assert (Condition => Get_Executing_CPU
              (Data    => Data,
               Subject => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/subjects/subject[@name='subj5']")) = 1,
              Message   => "Subj5 CPU mismatch");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/events/event[@name='to_subj4_from_subj2']",
         Name  => "mode",
         Value => "ipi");

      --  Must not change CPU of subj4 since it is still executable via subj3.

      Assert (Condition => Get_Executing_CPU
              (Data    => Data,
               Subject => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/subjects/subject[@name='subj4']")) = 0,
              Message   => "Subj4 CPU mismatch (2)");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/events/event[@name='to_subj3_from_subj2']",
         Name  => "mode",
         Value => "ipi");

      Assert (Condition => Get_Executing_CPU
              (Data    => Data,
               Subject => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/subjects/subject[@name='subj3']")) = -1,
              Message   => "Subj3 CPU mismatch (2)");
--  begin read only
   end Test_Get_Executing_CPU;
--  end read only


--  begin read only
   procedure Test_Is_PCI_Device_Reference (Gnattest_T : in out Test);
   procedure Test_Is_PCI_Device_Reference_c44529 (Gnattest_T : in out Test) renames Test_Is_PCI_Device_Reference;
--  id:2.2/c4452982639ee4fa/Is_PCI_Device_Reference/1/0/
   procedure Test_Is_PCI_Device_Reference (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:149:4:Is_PCI_Device_Reference
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      declare
         PCI_Dev_Ref : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/subjects/subject/devices/"
            & "device[@physical='ethernet']");
      begin
         Assert (Condition => Is_PCI_Device_Reference
                 (Data       => Policy,
                  Device_Ref => PCI_Dev_Ref),
                 Message   => "'ethernet' not a PCI device");
      end;

      declare
         Devices_Node    : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/subjects/subject[@name='lnx']/devices");
         Non_PCI_Dev_Ref : DOM.Core.Node
           := DOM.Core.Documents.Create_Element
             (Doc      => Policy.Doc,
              Tag_Name => "device");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Non_PCI_Dev_Ref,
            Name  => "physical",
            Value => "debugconsole");
         Muxml.Utils.Append_Child
           (Node      => Devices_Node,
            New_Child => Non_PCI_Dev_Ref);

         Assert (Condition => not Is_PCI_Device_Reference
                 (Data       => Policy,
                  Device_Ref => Non_PCI_Dev_Ref),
                 Message   => "'debuglog' is a PCI device");
      end;
--  begin read only
   end Test_Is_PCI_Device_Reference;
--  end read only


--  begin read only
   procedure Test_Get_Minor_Frame_Deadlines (Gnattest_T : in out Test);
   procedure Test_Get_Minor_Frame_Deadlines_8d9fe0 (Gnattest_T : in out Test) renames Test_Get_Minor_Frame_Deadlines;
--  id:2.2/8d9fe0ebb1ef6589/Get_Minor_Frame_Deadlines/1/0/
   procedure Test_Get_Minor_Frame_Deadlines (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:164:4:Get_Minor_Frame_Deadlines
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Deadlines : constant array (1 .. 7) of Interfaces.Unsigned_64
        := (1 => 20,
            2 => 40,
            3 => 40,
            4 => 60,
            5 => 60,
            6 => 80,
            7 => 80);
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      declare
         use type Interfaces.Unsigned_64;

         Major     : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/scheduling/majorFrame");
         Deadlines : constant Deadline_Array := Get_Minor_Frame_Deadlines
           (Major => Major);
      begin
         for I in Deadlines'Range loop
            Assert (Condition => Deadlines (I).Exit_Time = Ref_Deadlines (I),
                    Message   => "Minor frame deadline" & I'Img & " mismatch");
         end loop;
      end;
--  begin read only
   end Test_Get_Minor_Frame_Deadlines;
--  end read only


--  begin read only
   procedure Test_Get_IOMMU_Paging_Levels (Gnattest_T : in out Test);
   procedure Test_Get_IOMMU_Paging_Levels_d551c5 (Gnattest_T : in out Test) renames Test_Get_IOMMU_Paging_Levels;
--  id:2.2/d551c5832ddd8f54/Get_IOMMU_Paging_Levels/1/0/
   procedure Test_Get_IOMMU_Paging_Levels (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:174:4:Get_IOMMU_Paging_Levels
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Assert (Condition => Get_IOMMU_Paging_Levels (Data => Policy) = 4,
              Message   => "Paging-levels not 4");
--  begin read only
   end Test_Get_IOMMU_Paging_Levels;
--  end read only


--  begin read only
   procedure Test_Has_Feature_Enabled (Gnattest_T : in out Test);
   procedure Test_Has_Feature_Enabled_51713d (Gnattest_T : in out Test) renames Test_Has_Feature_Enabled;
--  id:2.2/51713df93516916c/Has_Feature_Enabled/1/0/
   procedure Test_Has_Feature_Enabled (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:183:4:Has_Feature_Enabled
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Node   : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Assert (Condition => Has_Feature_Enabled
              (Data => Policy,
               F    => Feature_IOMMU),
              Message   => "Feature 'iommu' not enabled");

      Node := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/features/iommu");
      DOM.Core.Elements.Set_Attribute (Elem  => Node,
                                       Name  => "enabled",
                                       Value => "false");

      Assert (Condition => not Has_Feature_Enabled
              (Data => Policy,
               F    => Feature_IOMMU),
              Message   => "Feature 'iommu' enabled");
--  begin read only
   end Test_Has_Feature_Enabled;
--  end read only


--  begin read only
   procedure Test_Get_IOAPIC_RTE_Idx (Gnattest_T : in out Test);
   procedure Test_Get_IOAPIC_RTE_Idx_46a118 (Gnattest_T : in out Test) renames Test_Get_IOAPIC_RTE_Idx;
--  id:2.2/46a1180676847d54/Get_IOAPIC_RTE_Idx/1/0/
   procedure Test_Get_IOAPIC_RTE_Idx (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:198:4:Get_IOAPIC_RTE_Idx
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Get_IOAPIC_RTE_Idx (IRQ => 0) = 2,
              Message   => "Timer RTE idx mismatch");

      for I in Legacy_IRQ_Range'First_Valid + 1 .. Legacy_IRQ_Range'Last_Valid
      loop
         if I /= 2 then
            Assert (Condition => Get_IOAPIC_RTE_Idx
                    (IRQ => I) = IOAPIC_RTE_Range (I),
                    Message   => "RTE idx for IRQ" & I'Img & " mismatch");
         end if;
      end loop;
--  begin read only
   end Test_Get_IOAPIC_RTE_Idx;
--  end read only


--  begin read only
   procedure Test_Get_IRQ_Kind (Gnattest_T : in out Test);
   procedure Test_Get_IRQ_Kind_43e0bc (Gnattest_T : in out Test) renames Test_Get_IRQ_Kind;
--  id:2.2/43e0bc53c1cd9b89/Get_IRQ_Kind/1/0/
   procedure Test_Get_IRQ_Kind (Gnattest_T : in out Test) is
   --  mutools-xml_utils.ads:209:4:Get_IRQ_Kind
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl      : DOM.Core.DOM_Implementation;
      Data      : Muxml.XML_Data_Type;
      Node, Dev : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Dev := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "device");
      Muxml.Utils.Append_Child
        (Node      => Data.Doc,
         New_Child => Dev);

      Assert (Condition => Get_IRQ_Kind (Dev => Dev) = IRQ_ISA,
              Message   => "Not ISA IRQ");

      Node := DOM.Core.Nodes.Append_Child
        (N         => Dev,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "pci"));

      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "msi",
         Value => "false");
      Assert (Condition => Get_IRQ_Kind (Dev => Dev) = IRQ_PCI_LSI,
              Message   => "Not PCI LSI IRQ");

      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "msi",
         Value => "true");
      Assert (Condition => Get_IRQ_Kind (Dev => Dev) = IRQ_PCI_MSI,
              Message   => "Not PCI MSI IRQ");
--  begin read only
   end Test_Get_IRQ_Kind;
--  end read only

end Mutools.XML_Utils.Test_Data.Tests;
