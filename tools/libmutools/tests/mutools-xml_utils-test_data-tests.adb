--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.XML_Utils.Test_Data.

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
package body Mutools.XML_Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_1_Add_Resource (Gnattest_T : in out Test);
   procedure Test_Add_Resource_bbc2a0 (Gnattest_T : in out Test) renames Test_1_Add_Resource;
--  id:2.2/bbc2a0834a8bd1f4/Add_Resource/1/0/
   procedure Test_1_Add_Resource (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Add_IRQ_Resource;
      procedure Add_IO_Port_Resource;
      procedure Add_Memory_Resource;
      procedure Add_Mmconf_Resource;

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

         Add_Resource (Logical_Device        => Dev_Node,
                       Physical_Resource     => Resource,
                       Logical_Resource_Name => "IO_Port_Logical_Name");
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Dev_Node,
                  XPath => "ioPort",
                  Name  => "physical") = Ref_Name,
                 Message   => "I/O port physical name mismatch");
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Dev_Node,
                  XPath => "ioPort",
                  Name  => "logical") = "IO_Port_Logical_Name",
                 Message   => "I/O port logical name mismatch");
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

         Add_Resource (Logical_Device        => Dev_Node,
                       Physical_Resource     => Resource,
                       Logical_Resource_Name => "IRQ_Logical_Name");
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Dev_Node,
                  XPath => "irq",
                  Name  => "physical") = Ref_Name,
                 Message   => "IRQ physical name mismatch");
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Dev_Node,
                  XPath => "irq",
                  Name  => "logical") = "IRQ_Logical_Name",
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

      ----------------------------------------------------------------------

      procedure Add_Memory_Resource_No_VAddr
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

         Add_Resource (Logical_Device       => Dev_Node,
                       Physical_Resource    => Resource,
                       Set_Logical_Mem_Addr => False);
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
                  Name  => "virtualAddress") = "",
                 Message   => "Memory virtual address set");
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
      end Add_Memory_Resource_No_VAddr;

      ----------------------------------------------------------------------

      procedure Add_Mmconf_Resource
      is
         Dom_Impl : DOM.Core.DOM_Implementation;
         Policy   : Muxml.XML_Data_Type;

         Resource, Dev, Devices, PCI : DOM.Core.Node;

         Ref_Addr : constant String := "16#302b_3000#";
         Ref_Name : constant String := "mmconf";
      begin
         Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);

         Resource := DOM.Core.Documents.Create_Element
           (Doc      => Policy.Doc,
            Tag_Name => "memory");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Resource,
            Name  => "name",
            Value => Ref_Name);
         DOM.Core.Elements.Set_Attribute
           (Elem  => Resource,
            Name  => "physicalAddress",
            Value => "16#e000_1000#");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Resource,
            Name  => "size",
            Value => "16#1000#");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Resource,
            Name  => "caching",
            Value => "UC");

         PCI := DOM.Core.Documents.Create_Element
           (Doc      => Policy.Doc,
            Tag_Name => "pci");
         DOM.Core.Elements.Set_Attribute
           (Elem  => PCI,
            Name  => "bus",
            Value => "16#02#");
         DOM.Core.Elements.Set_Attribute
           (Elem  => PCI,
            Name  => "device",
            Value => "16#16#");
         DOM.Core.Elements.Set_Attribute
           (Elem  => PCI,
            Name  => "function",
            Value => "16#03#");

         Devices := DOM.Core.Documents.Create_Element
           (Doc      => Policy.Doc,
            Tag_Name => "devices");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Devices,
            Name  => "pciConfigAddress",
            Value => "16#e000_0000#");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Devices,
            Name  => "pciConfigSize",
            Value => "16#1000_0000#");

         Dev := DOM.Core.Documents.Create_Element
           (Doc      => Policy.Doc,
            Tag_Name => "device");

         Add_Resource
           (Logical_Device         => Dev,
            Physical_Resource      => Resource,
            Mmconf_Devices_Node    => Devices,
            Mmconf_Device_PCI_Node => PCI,
            Mmconf_Virt_Base       => 16#3000_0000#);
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Dev,
                  XPath => "memory",
                  Name  => "physical") = Ref_Name,
                 Message   => "Memory physical name mismatch");
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Dev,
                  XPath => "memory",
                  Name  => "logical") = Ref_Name,
                 Message   => "Memory logical name mismatch");
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Dev,
                  XPath => "memory",
                  Name  => "virtualAddress") = Ref_Addr,
                 Message   => "Memory virtual address mismatch");
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Dev,
                  XPath => "memory",
                  Name  => "executable") = "false",
                 Message   => "Memory is executable");
         Assert (Condition => Muxml.Utils.Get_Attribute
                 (Doc   => Dev,
                  XPath => "memory",
                  Name  => "writable") = "true",
                 Message   => "Memory is not writable");
      end Add_Mmconf_Resource;
   begin
      Add_IRQ_Resource;
      Add_IO_Port_Resource;
      Add_Memory_Resource;
      Add_Memory_Resource_No_VAddr;
      Add_Mmconf_Resource;
--  begin read only
   end Test_1_Add_Resource;
--  end read only


--  begin read only
   procedure Test_2_Add_Resource (Gnattest_T : in out Test);
   procedure Test_Add_Resource_961f4c (Gnattest_T : in out Test) renames Test_2_Add_Resource;
--  id:2.2/961f4cd333e6c38d/Add_Resource/0/0/
   procedure Test_2_Add_Resource (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert
        (Condition => True,
         Message   => "Implicitly tested by Add_Resource procedure tests.");
--  begin read only
   end Test_2_Add_Resource;
--  end read only


--  begin read only
   procedure Test_1_Add_Memory_Region (Gnattest_T : in out Test);
   procedure Test_Add_Memory_Region_6142bd (Gnattest_T : in out Test) renames Test_1_Add_Memory_Region;
--  id:2.2/6142bd7e03979890/Add_Memory_Region/1/0/
   procedure Test_1_Add_Memory_Region (Gnattest_T : in out Test) is
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
   procedure Test_Add_Memory_Region_0d672c (Gnattest_T : in out Test) renames Test_2_Add_Memory_Region;
--  id:2.2/0d672c4a356737db/Add_Memory_Region/0/0/
   procedure Test_2_Add_Memory_Region (Gnattest_T : in out Test) is
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

      Add_Memory_Region
        (Policy      => Policy,
         Name        => "test_with_file_size",
         Address     => "16#5000#",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "",
         File_Name   => "testfile",
         File_Offset => "16#0010#",
         File_Size   => "16#3550#");

      Muxml.Write (Data => Policy,
                   Kind => Muxml.None,
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
   procedure Test_Create_Component_Memory_Node (Gnattest_T : in out Test);
   procedure Test_Create_Component_Memory_Node_234769 (Gnattest_T : in out Test) renames Test_Create_Component_Memory_Node;
--  id:2.2/23476998984362c9/Create_Component_Memory_Node/1/0/
   procedure Test_Create_Component_Memory_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Dom_Impl : DOM.Core.DOM_Implementation;
      Policy   : Muxml.XML_Data_Type;
      Node     : DOM.Core.Node;
      Logical  : constant String := "testl";
      Address  : constant String := "16#2000#";
      Size     : constant String := "16#3000#";
   begin
      Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);

      Node := Create_Component_Memory_Node
        (Policy       => Policy,
         Logical_Name => Logical,
         Address      => Address,
         Size         => Size,
         Writable     => True,
         Executable   => False);

      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "logical") = Logical,
              Message   => "Logical name mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "virtualAddress") = Address,
              Message   => "Address mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "size") = Size,
              Message   => "Size mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "writable") = "true",
              Message   => "Writable mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "executable") = "false",
              Message   => "Executable mismatch");
--  begin read only
   end Test_Create_Component_Memory_Node;
--  end read only


--  begin read only
   procedure Test_Get_Enclosing_Virtual_Region (Gnattest_T : in out Test);
   procedure Test_Get_Enclosing_Virtual_Region_356300 (Gnattest_T : in out Test) renames Test_Get_Enclosing_Virtual_Region;
--  id:2.2/3563001b0a0f34f2/Get_Enclosing_Virtual_Region/1/0/
   procedure Test_Get_Enclosing_Virtual_Region (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type DOM.Core.Node;

      Policy   : Muxml.XML_Data_Type;
      Phys_Mem : DOM.Core.Node_List;
      Log_Mem  : DOM.Core.Node_List;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Phys_Mem := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/memory/memory");
      Log_Mem := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject[@name='lnx']/memory/memory");

      Assert (Condition => Get_Enclosing_Virtual_Region
              (Virtual_Address => 0,
               Physical_Memory => Phys_Mem,
               Logical_Memory  => Log_Mem) = null,
              Message   => "Enclosing region found (1)");
      Assert (Condition => Get_Enclosing_Virtual_Region
              (Virtual_Address => 16#2000#,
               Physical_Memory => Phys_Mem,
               Logical_Memory  => Log_Mem) = null,
              Message   => "Enclosing region found (2)");
      Assert (Condition => Get_Enclosing_Virtual_Region
              (Virtual_Address => 16#cafe_0000#,
               Physical_Memory => Phys_Mem,
               Logical_Memory  => Log_Mem) = null,
              Message   => "Enclosing region found (3)");

      declare
         Empty_List : DOM.Core.Node_List;
      begin
         Assert (Condition => Get_Enclosing_Virtual_Region
                 (Virtual_Address => 16#1000#,
                  Physical_Memory => Empty_List,
                  Logical_Memory  => Log_Mem) = null,
                 Message   => "Enclosing region found (4)");
         Assert (Condition => Get_Enclosing_Virtual_Region
                 (Virtual_Address => 16#1000#,
                  Physical_Memory => Phys_Mem,
                  Logical_Memory  => Empty_List) = null,
                 Message   => "Enclosing region found (5)");
         Assert (Condition => Get_Enclosing_Virtual_Region
                 (Virtual_Address => 16#1000#,
                  Physical_Memory => Empty_List,
                  Logical_Memory  => Empty_List) = null,
                 Message   => "Enclosing region found (6)");
      end;

      declare
         Mem_Node : constant DOM.Core.Node
           := Get_Enclosing_Virtual_Region
             (Virtual_Address => 16#1000#,
              Physical_Memory => Phys_Mem,
              Logical_Memory  => Log_Mem);
      begin
         Assert (Condition => Mem_Node /= null,
                 Message   => "Enclosing region not found (1)");
         Assert (Condition => DOM.Core.Elements.Get_Attribute
                 (Elem => Mem_Node,
                  Name => "logical") = "dummy",
                 Message   => "Enclosing region name mismatch (1)");
      end;

      declare
         Mem_Node : constant DOM.Core.Node
           := Get_Enclosing_Virtual_Region
             (Virtual_Address => 16#1caf#,
              Physical_Memory => Phys_Mem,
              Logical_Memory  => Log_Mem);
      begin
         Assert (Condition => Mem_Node /= null,
                 Message   => "Enclosing region not found (2)");
         Assert (Condition => DOM.Core.Elements.Get_Attribute
                 (Elem => Mem_Node,
                  Name => "logical") = "dummy",
                 Message   => "Enclosing region name mismatch (2)");
      end;

      declare
         Mem_Node : constant DOM.Core.Node
           := Get_Enclosing_Virtual_Region
             (Virtual_Address => 16#1fff#,
              Physical_Memory => Phys_Mem,
              Logical_Memory  => Log_Mem);
      begin
         Assert (Condition => Mem_Node /= null,
                 Message   => "Enclosing region not found (3)");
         Assert (Condition => DOM.Core.Elements.Get_Attribute
                 (Elem => Mem_Node,
                  Name => "logical") = "dummy",
                 Message   => "Enclosing region name mismatch (3)");
      end;
--  begin read only
   end Test_Get_Enclosing_Virtual_Region;
--  end read only


--  begin read only
   procedure Test_Has_Managed_DEBUGCTL (Gnattest_T : in out Test);
   procedure Test_Has_Managed_DEBUGCTL_07c840 (Gnattest_T : in out Test) renames Test_Has_Managed_DEBUGCTL;
--  id:2.2/07c840ea4cf93188/Has_Managed_DEBUGCTL/1/0/
   procedure Test_Has_Managed_DEBUGCTL (Gnattest_T : in out Test) is
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
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      MSRs : DOM.Core.Node_List;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Assert (Condition => Calculate_MSR_Count
              (MSRs                   => MSRs,
               DEBUGCTL_Control       => False,
               PAT_Control            => False,
               PERFGLOBALCTRL_Control => False,
               EFER_Control           => False) = 0,
              Message   => "Empty list count not 0");

      Append_MSR (Doc       => Data.Doc,
                  List      => MSRs,
                  MSR_Start => "16#0010#",
                  MSR_End   => "16#0010#",
                  Mode      => "r");
      Append_MSR (Doc       => Data.Doc,
                  List      => MSRs,
                  MSR_Start => "16#0174#",
                  MSR_End   => "16#0176#",
                  Mode      => "rw");
      Append_MSR (Doc       => Data.Doc,
                  List      => MSRs,
                  MSR_Start => "16#c000_0080#",
                  MSR_End   => "16#c000_0084#",
                  Mode      => "rw");
      Append_MSR (Doc       => Data.Doc,
                  List      => MSRs,
                  MSR_Start => "16#c000_0100#",
                  MSR_End   => "16#c000_0102#",
                  Mode      => "rw");

      Assert (Condition => Calculate_MSR_Count
              (MSRs                   => MSRs,
               DEBUGCTL_Control       => False,
               PAT_Control            => False,
               PERFGLOBALCTRL_Control => False,
               EFER_Control           => True) = 6,
              Message   => "MSR count mismatch (1)");

      Append_MSR (Doc       => Data.Doc,
                  List      => MSRs,
                  MSR_Start => "16#00ce#",
                  MSR_End   => "16#00ce#",
                  Mode      => "r");
      Append_MSR (Doc       => Data.Doc,
                  List      => MSRs,
                  MSR_Start => "16#01a2#",
                  MSR_End   => "16#01a2#",
                  Mode      => "rw");
      Assert (Condition => Calculate_MSR_Count
              (MSRs                   => MSRs,
               DEBUGCTL_Control       => False,
               PAT_Control            => False,
               PERFGLOBALCTRL_Control => False,
               EFER_Control           => True) = 6,
              Message   => "MSR count mismatch (2)");
--  begin read only
   end Test_Calculate_MSR_Count;
--  end read only


--  begin read only
   procedure Test_Is_MSR_Accessible (Gnattest_T : in out Test);
   procedure Test_Is_MSR_Accessible_652a62 (Gnattest_T : in out Test) renames Test_Is_MSR_Accessible;
--  id:2.2/652a62dcfa198d1c/Is_MSR_Accessible/1/0/
   procedure Test_Is_MSR_Accessible (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      MSRs : DOM.Core.Node_List;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Assert (Condition => not Is_MSR_Accessible (MSR  => 16#0174#,
                                                  MSRs => MSRs),
              Message   => "MSR accessible (empty list)");

      Append_MSR (Doc       => Data.Doc,
                  List      => MSRs,
                  MSR_Start => "16#0174#",
                  MSR_End   => "16#0176#",
                  Mode      => "rw");
      Append_MSR (Doc       => Data.Doc,
                  List      => MSRs,
                  MSR_Start => "16#c000_0080#",
                  MSR_End   => "16#c000_0084#",
                  Mode      => "rw");
      Append_MSR (Doc       => Data.Doc,
                  List      => MSRs,
                  MSR_Start => "16#c000_0100#",
                  MSR_End   => "16#c000_0102#",
                  Mode      => "rw");
      Assert (Condition => Is_MSR_Accessible (MSR  => 16#0174#,
                                              MSRs => MSRs),
              Message   => "MSR not accessible (1)");
      Assert (Condition => Is_MSR_Accessible (MSR  => 16#0175#,
                                              MSRs => MSRs),
              Message   => "MSR not accessible (2)");
      Assert (Condition => Is_MSR_Accessible (MSR  => 16#c000_0101#,
                                              MSRs => MSRs),
              Message   => "MSR not accessible (3)");
      Assert (Condition => not Is_MSR_Accessible (MSR  => 16#0173#,
                                                  MSRs => MSRs),
              Message   => "MSR accessible (1)");
      Assert (Condition => not Is_MSR_Accessible (MSR  => 16#0177#,
                                                  MSRs => MSRs),
              Message   => "MSR accessible (2)");
      Assert (Condition => not Is_MSR_Accessible (MSR  => 16#cafe#,
                                              MSRs => MSRs),
              Message   => "MSR accessible (3)");
      Assert (Condition => not Is_MSR_Accessible
              (MSR  => Interfaces.Unsigned_64'First,
               MSRs => MSRs),
              Message   => "MSR accessible (4)");
      Assert (Condition => not Is_MSR_Accessible
              (MSR  => Interfaces.Unsigned_64'Last,
               MSRs => MSRs),
              Message   => "MSR accessible (5)");
--  begin read only
   end Test_Is_MSR_Accessible;
--  end read only


--  begin read only
   procedure Test_Get_Occupied_PCI_Buses (Gnattest_T : in out Test);
   procedure Test_Get_Occupied_PCI_Buses_0b9ce6 (Gnattest_T : in out Test) renames Test_Get_Occupied_PCI_Buses;
--  id:2.2/0b9ce63ef86880a3/Get_Occupied_PCI_Buses/1/0/
   procedure Test_Get_Occupied_PCI_Buses (Gnattest_T : in out Test) is
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

      --  PCI bus of devices only referenced by device domain must also be
      --  occupied.

      Muxml.Utils.Remove_Elements
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/devices/device[@physical='xhci']");

      Buses := Get_Occupied_PCI_Buses (Data => Policy);
      Assert (Condition => Buses.Length = 2,
              Message   => "Bus count not 2 (2)");
      Assert (Condition => Buses.First_Element = 16#02#,
              Message   => "First bus not 16#02# (2)");
      Assert (Condition => Buses.Last_Element = 16#23#,
              Message   => "Last bus not 16#23# (2)");
--  begin read only
   end Test_Get_Occupied_PCI_Buses;
--  end read only


--  begin read only
   procedure Test_Get_Switch_Sources (Gnattest_T : in out Test);
   procedure Test_Get_Switch_Sources_bec022 (Gnattest_T : in out Test) renames Test_Get_Switch_Sources;
--  id:2.2/bec02284d574640c/Get_Switch_Sources/1/0/
   procedure Test_Get_Switch_Sources (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy  : Muxml.XML_Data_Type;
      Events, Src_Events, Sources : DOM.Core.Node_List;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/switch_events.xml");
      Events := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/events/event[@mode='switch']");
      Src_Events := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject/events/source/group"
           & "/*[self::event or self::default]");

      Sources := Get_Switch_Sources
        (Physical_Events => Events,
         Source_Events   => Src_Events,
         Target          => Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/subjects/subject[@name='subj1']"));
      Assert (Condition => DOM.Core.Nodes.Length (List => Sources) = 1,
              Message   => "Missing source subjects (1)");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => DOM.Core.Nodes.Item (List  => Sources,
                                            Index => 0),
               Name => "name") = "subj3",
              Message   => "Subj1 source switch mismatch");

      Sources :=  Get_Switch_Sources
        (Physical_Events => Events,
         Source_Events   => Src_Events,
         Target          => Muxml.Utils.Get_Element
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
   procedure Test_Get_Executing_CPU_92e387 (Gnattest_T : in out Test) renames Test_Get_Executing_CPU;
--  id:2.2/92e387b4d71b98ec/Get_Executing_CPU/1/0/
   procedure Test_Get_Executing_CPU (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
      Events, Src_Events, Sources, Minor_Frames : DOM.Core.Node_List;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_Src,
                   File => "data/switch-event-chains.xml");
      Events := McKae.XML.XPath.XIA.XPath_Query
        (N     => Data.Doc,
         XPath => "/system/events/event[@mode='switch']");
      Src_Events := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/events/source/group"
           & "/*[self::event or self::default]");
      Minor_Frames := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/scheduling/majorFrame/cpu/minorFrame");


      Assert (Condition => Get_Executing_CPU
              (Physical_Events => Events,
               Source_Events   => Src_Events,
               Minor_Frames    => Minor_Frames,
               Subject => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/subjects/subject[@name='subj1']")) = 0,
              Message   => "Subj1 CPU mismatch");

      Assert (Condition => Get_Executing_CPU
              (Physical_Events => Events,
               Source_Events   => Src_Events,
               Minor_Frames    => Minor_Frames,
               Subject         => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/subjects/subject[@name='subj2']")) = 0,
              Message   => "Subj2 CPU mismatch");
      Assert (Condition => Get_Executing_CPU
              (Physical_Events => Events,
               Source_Events   => Src_Events,
               Minor_Frames    => Minor_Frames,
               Subject         => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/subjects/subject[@name='subj3']")) = 0,
              Message   => "Subj3 CPU mismatch");
      Assert (Condition => Get_Executing_CPU
              (Physical_Events => Events,
               Source_Events   => Src_Events,
               Minor_Frames    => Minor_Frames,
               Subject         => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/subjects/subject[@name='subj4']")) = 0,
              Message   => "Subj4 CPU mismatch (1)");
      Assert (Condition => Get_Executing_CPU
              (Physical_Events => Events,
               Source_Events   => Src_Events,
               Minor_Frames    => Minor_Frames,
               Subject         => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/subjects/subject[@name='subj5']")) = 1,
              Message   => "Subj5 CPU mismatch");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/events/event[@name='to_subj4_from_subj2']",
         Name  => "mode",
         Value => "ipi");
      Events := McKae.XML.XPath.XIA.XPath_Query
        (N     => Data.Doc,
         XPath => "/system/events/event[@mode='switch']");

      --  Must not change CPU of subj4 since it is still executable via subj3.

      Assert (Condition => Get_Executing_CPU
              (Physical_Events => Events,
               Source_Events   => Src_Events,
               Minor_Frames    => Minor_Frames,
               Subject         => Muxml.Utils.Get_Element
                 (Doc   => Data.Doc,
                  XPath => "/system/subjects/subject[@name='subj4']")) = 0,
              Message   => "Subj4 CPU mismatch (2)");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/events/event[@name='to_subj3_from_subj2']",
         Name  => "mode",
         Value => "ipi");
      Events := McKae.XML.XPath.XIA.XPath_Query
        (N     => Data.Doc,
         XPath => "/system/events/event[@mode='switch']");

      Assert (Condition => Get_Executing_CPU
              (Physical_Events => Events,
               Source_Events   => Src_Events,
               Minor_Frames    => Minor_Frames,
               Subject         => Muxml.Utils.Get_Element
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
   procedure Test_Get_IOAPIC_RTE_Idx (Gnattest_T : in out Test);
   procedure Test_Get_IOAPIC_RTE_Idx_634f02 (Gnattest_T : in out Test) renames Test_Get_IOAPIC_RTE_Idx;
--  id:2.2/634f027c2ad084df/Get_IOAPIC_RTE_Idx/1/0/
   procedure Test_Get_IOAPIC_RTE_Idx (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Get_IOAPIC_RTE_Idx (IRQ => 0) = 2,
              Message   => "Timer RTE idx mismatch");

      for I in IOAPIC_IRQ_Range'First_Valid + 1 .. IOAPIC_IRQ_Range'Last_Valid
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
   procedure Test_Get_IOAPIC_RTE_Index_Max (Gnattest_T : in out Test);
   procedure Test_Get_IOAPIC_RTE_Index_Max_f9e0d9 (Gnattest_T : in out Test) renames Test_Get_IOAPIC_RTE_Index_Max;
--  id:2.2/f9e0d90de9b7310d/Get_IOAPIC_RTE_Index_Max/1/0/
   procedure Test_Get_IOAPIC_RTE_Index_Max (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Assert (Condition => Get_IOAPIC_RTE_Index_Max (Data => Policy) = 23,
              Message   => "I/O APIC RTE Max not 23");

      begin
         Muxml.Utils.Remove_Elements
           (Doc   => Policy.Doc,
            XPath => "/system/hardware/devices/device[@name='ioapic_1']");
         declare
            Dummy : constant Natural
              := Get_IOAPIC_RTE_Index_Max (Data => Policy);
         begin
            Assert (Condition => False,
                    Message   => "Exception expected");
         end;

      exception
         when E : IOAPIC_Not_Found =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unable to determine maximum I/O APIC RTE index: no I/O "
                    & "APICs found",
                    Message   => "Exception message mismatch");
      end;

--  begin read only
   end Test_Get_IOAPIC_RTE_Index_Max;
--  end read only


--  begin read only
   procedure Test_Get_IRQ_Kind (Gnattest_T : in out Test);
   procedure Test_Get_IRQ_Kind_43e0bc (Gnattest_T : in out Test) renames Test_Get_IRQ_Kind;
--  id:2.2/43e0bc53c1cd9b89/Get_IRQ_Kind/1/0/
   procedure Test_Get_IRQ_Kind (Gnattest_T : in out Test) is
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


--  begin read only
   procedure Test_Sort_By_BDF (Gnattest_T : in out Test);
   procedure Test_Sort_By_BDF_df931d (Gnattest_T : in out Test) renames Test_Sort_By_BDF;
--  id:2.2/df931d787acb2f30/Sort_By_BDF/1/0/
   procedure Test_Sort_By_BDF (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Devs : DOM.Core.Node_List;

      function Create_PCI_Dev
        (Name : String;
         Bus  : String;
         Dev  : String;
         Fun  : String)
         return DOM.Core.Node
      is
         Device : DOM.Core.Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "device");
         Pci    : DOM.Core.Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "pci");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Device,
            Name  => "name",
            Value => Name);

         DOM.Core.Elements.Set_Attribute
           (Elem  => Pci,
            Name  => "bus",
            Value => Bus);
         DOM.Core.Elements.Set_Attribute
           (Elem  => Pci,
            Name  => "device",
            Value => Dev);
         DOM.Core.Elements.Set_Attribute
           (Elem  => Pci,
            Name  => "function",
            Value => Fun);

         Muxml.Utils.Append_Child (Node      => Device,
                                   New_Child => Pci);
         return Device;
      end Create_PCI_Dev;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      DOM.Core.Append_Node
        (List => Devs,
         N    => Create_PCI_Dev
           (Name => "4",
            Bus  => "9",
            Dev  => "31",
            Fun  => "7"));
      DOM.Core.Append_Node
        (List => Devs,
         N    => Create_PCI_Dev
           (Name => "0",
            Bus  => "0",
            Dev  => "0",
            Fun  => "0"));
      DOM.Core.Append_Node
        (List => Devs,
         N    => Create_PCI_Dev
           (Name => "1",
            Bus  => "0",
            Dev  => "1",
            Fun  => "0"));
      DOM.Core.Append_Node
        (List => Devs,
         N    => Create_PCI_Dev
           (Name => "3",
            Bus  => "0",
            Dev  => "8",
            Fun  => "2"));
      DOM.Core.Append_Node
        (List => Devs,
         N    => Create_PCI_Dev
           (Name => "2",
            Bus  => "0",
            Dev  => "8",
            Fun  => "1"));

      declare
         S_Devs : constant DOM.Core.Node_List
           := Sort_By_BDF (PCI_Devs => Devs);
      begin
         for I in Natural range 0 .. DOM.Core.Nodes.Length (List => S_Devs) - 1
         loop
            declare
               Dev : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item (List  => S_Devs,
                                         Index => I);
               Pos : constant Natural := Natural'Value
                 (DOM.Core.Elements.Get_Attribute (Elem => Dev,
                                                   Name => "name"));
            begin
               Assert (Condition => Pos = I,
                       Message   => "Unexpected position" & Pos'Img
                       & " of element" & I'Img);
            end;
         end loop;
      end;
--  begin read only
   end Test_Sort_By_BDF;
--  end read only


--  begin read only
   procedure Test_Equal_BDFs (Gnattest_T : in out Test);
   procedure Test_Equal_BDFs_f0c029 (Gnattest_T : in out Test) renames Test_Equal_BDFs;
--  id:2.2/f0c02998ab9c0c03/Equal_BDFs/1/0/
   procedure Test_Equal_BDFs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data   : Muxml.XML_Data_Type;
      Impl   : DOM.Core.DOM_Implementation;
      N1, N2 : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      N1 := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "pci");
      DOM.Core.Elements.Set_Attribute
        (Elem  => N1,
         Name  => "bus",
         Value => "16#05#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => N1,
         Name  => "device",
         Value => "16#11#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => N1,
         Name  => "function",
         Value => "2");

      Assert (Condition => Equal_BDFs
              (Left  => N1,
               Right => N1),
              Message   => "Same node not equal");

      N2 := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "pci");
      DOM.Core.Elements.Set_Attribute
        (Elem  => N1,
         Name  => "bus",
         Value => "16#06#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => N1,
         Name  => "device",
         Value => "16#11#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => N1,
         Name  => "function",
         Value => "2");
      Assert (Condition => not Equal_BDFs
              (Left  => N1,
               Right => N2),
              Message   => "Bus difference not detected");

      DOM.Core.Elements.Set_Attribute
        (Elem  => N1,
         Name  => "bus",
         Value => "16#05#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => N1,
         Name  => "device",
         Value => "16#12#");
      Assert (Condition => not Equal_BDFs
              (Left  => N1,
               Right => N2),
              Message   => "Device difference not detected");

      DOM.Core.Elements.Set_Attribute
        (Elem  => N1,
         Name  => "device",
         Value => "16#11#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => N1,
         Name  => "function",
         Value => "3");
      Assert (Condition => not Equal_BDFs
              (Left  => N1,
               Right => N2),
              Message   => "Function difference not detected");
--  begin read only
   end Test_Equal_BDFs;
--  end read only


--  begin read only
   procedure Test_1_Set_Memory_Size (Gnattest_T : in out Test);
   procedure Test_Set_Memory_Size_9298ea (Gnattest_T : in out Test) renames Test_1_Set_Memory_Size;
--  id:2.2/9298eacc38aef69c/Set_Memory_Size/1/0/
   procedure Test_1_Set_Memory_Size (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data  : Muxml.XML_Data_Type;
      Impl  : DOM.Core.DOM_Implementation;
      Nodes : DOM.Core.Node_List;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      DOM.Core.Append_Node
        (List => Nodes,
         N    => Create_Mem_Node
           (Doc     => Data.Doc,
            Name    => "mem1",
            Address => "16#1000#",
            Size    => "16#1000#"));
      DOM.Core.Append_Node
        (List => Nodes,
         N    => Create_Mem_Node
           (Doc     => Data.Doc,
            Name    => "mem2",
            Address => "16#2000#",
            Size    => "16#beef_0000#"));

      declare
         Vmem_Node : DOM.Core.Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "memory");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Vmem_Node,
            Name  => "physical",
            Value => "mem1");
         Set_Memory_Size (Virtual_Mem_Node => Vmem_Node,
                          Ref_Nodes        => Nodes);

         Assert (Condition => DOM.Core.Elements.Get_Attribute
                 (Elem => Vmem_Node,
                  Name => "size") = "16#1000#",
                 Message   => "'mem1' size mismatch");
      end;

      declare
         Vmem_Node : DOM.Core.Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "memory");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Vmem_Node,
            Name  => "physical",
            Value => "mem2");
         Set_Memory_Size (Virtual_Mem_Node => Vmem_Node,
                          Ref_Nodes        => Nodes);

         Assert (Condition => DOM.Core.Elements.Get_Attribute
                 (Elem => Vmem_Node,
                  Name => "size") = "16#beef_0000#",
                 Message   => "'mem2' size mismatch");
      end;
--  begin read only
   end Test_1_Set_Memory_Size;
--  end read only


--  begin read only
   procedure Test_2_Set_Memory_Size (Gnattest_T : in out Test);
   procedure Test_Set_Memory_Size_aa0598 (Gnattest_T : in out Test) renames Test_2_Set_Memory_Size;
--  id:2.2/aa059819d3720cbc/Set_Memory_Size/0/0/
   procedure Test_2_Set_Memory_Size (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data                 : Muxml.XML_Data_Type;
      Impl                 : DOM.Core.DOM_Implementation;
      Node                 : DOM.Core.Node;
      Mem_Nodes, Ref_Nodes : DOM.Core.Node_List;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      DOM.Core.Append_Node
        (List => Ref_Nodes,
         N    => Create_Mem_Node
           (Doc     => Data.Doc,
            Name    => "mem1",
            Address => "16#1000#",
            Size    => "16#1000#"));
      DOM.Core.Append_Node
        (List => Ref_Nodes,
         N    => Create_Mem_Node
           (Doc     => Data.Doc,
            Name    => "mem2",
            Address => "16#2000#",
            Size    => "16#beef_0000#"));

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "memory");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "physical",
         Value => "mem1");
      DOM.Core.Append_Node (List => Mem_Nodes,
                            N    => Node);
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "memory");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "physical",
         Value => "mem2");
      DOM.Core.Append_Node (List => Mem_Nodes,
                            N    => Node);

      Set_Memory_Size (Virtual_Mem_Nodes => Mem_Nodes,
                       Ref_Nodes         => Ref_Nodes);

      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => DOM.Core.Nodes.Item
               (List  => Mem_Nodes,
                Index => 0),
               Name => "size") = "16#1000#",
              Message   => "'mem1' size mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => DOM.Core.Nodes.Item
               (List  => Mem_Nodes,
                Index => 1),
               Name => "size") = "16#beef_0000#",
              Message   => "'mem2' size mismatch");
--  begin read only
   end Test_2_Set_Memory_Size;
--  end read only


--  begin read only
   procedure Test_Get_Initial_Scheduling_Group_Subjects (Gnattest_T : in out Test);
   procedure Test_Get_Initial_Scheduling_Group_Subjects_6ccbca (Gnattest_T : in out Test) renames Test_Get_Initial_Scheduling_Group_Subjects;
--  id:2.2/6ccbca255d8e4b4e/Get_Initial_Scheduling_Group_Subjects/1/0/
   procedure Test_Get_Initial_Scheduling_Group_Subjects (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;

      Ref_Mapping : constant ID_Map_Array
        := (1 => 0,
            2 => 1,
            3 => 6,
            4 => 4,
            5 => 5,
            6 => 7);
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/scheduling.xml");

      Assert (Condition => Get_Initial_Scheduling_Group_Subjects
              (Data => Policy) = Ref_Mapping,
              Message   => "Scheduling group to subject ID mapping mismatch");

      begin
         Muxml.Utils.Remove_Elements
           (Doc   => Policy.Doc,
            XPath => "/system/subjects/subject[@name='dbgserver']");

         declare
            Unused : constant ID_Map_Array
              := Get_Initial_Scheduling_Group_Subjects (Data => Policy);
         begin
            Assert (Condition => False,
                    Message   => "Exception expected");
         end;

      exception
         when E : Missing_Subject =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject 'dbgserver' referenced in scheduling plan not "
                    & "present",
                    Message   => "Exception message mismatch");
      end;

      begin
         Muxml.Utils.Remove_Elements
           (Doc   => Policy.Doc,
            XPath => "/system/subjects/subject[@name='nic_sm']");
         Muxml.Utils.Remove_Elements
           (Doc   => Policy.Doc,
            XPath => "/system/subjects/subject[@name='example']");

         declare
            Unused : constant ID_Map_Array
              := Get_Initial_Scheduling_Group_Subjects (Data => Policy);
         begin
            Assert (Condition => False,
                    Message   => "Exception expected");
         end;

      exception
         when E : Invalid_Subject_ID =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject 'nic_linux' referenced in scheduling plan has "
                    & "invalid ID 6, not in range 0.. 5",
                    Message   => "Exception message mismatch");
      end;
--  begin read only
   end Test_Get_Initial_Scheduling_Group_Subjects;
--  end read only


--  begin read only
   procedure Test_Get_Subject_To_Scheduling_Group_Map (Gnattest_T : in out Test);
   procedure Test_Get_Subject_To_Scheduling_Group_Map_8b4c66 (Gnattest_T : in out Test) renames Test_Get_Subject_To_Scheduling_Group_Map;
--  id:2.2/8b4c661263f9c87d/Get_Subject_To_Scheduling_Group_Map/1/0/
   procedure Test_Get_Subject_To_Scheduling_Group_Map (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;

      Ref_Mapping : constant ID_Map_Array
        := (0 => 1,
            1 => 2,
            2 => 3,
            3 => 6,
            4 => 4,
            5 => 5,
            6 => 3,
            7 => 6,
            8 => 3);
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/scheduling.xml");

      Assert (Condition => Get_Subject_To_Scheduling_Group_Map
              (Data => Policy) = Ref_Mapping,
              Message   => "Subject to scheduling group ID mapping mismatch");

      --  Verify that cyclic switch events are handled correctly, i.e. do not
      --  lead to infinite recursion.

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/scheduling/majorFrame/cpu/"
         & "minorFrame[@subject='nic_linux']",
         Name  => "subject",
         Value => "example");

      declare
         task Worker
         is
            entry Start;
            entry Done (Match : out Boolean);
         end Worker;

         task body Worker
         is
            Map_Match : Boolean;
         begin
            select
               accept Start;
            or
               terminate;
            end select;

            begin
               Map_Match := Get_Subject_To_Scheduling_Group_Map
                 (Data => Policy) = Ref_Mapping;

            exception
               when others =>
                  Map_Match := False;
            end;

            accept Done (Match : out Boolean) do
               Match := Map_Match;
            end Done;
         end Worker;

         Result : Boolean;
      begin

         Worker.Start;

         select
            Worker.Done (Match => Result);
         or
            delay 0.1;
            abort Worker;
            Assert (Condition => False,
                    Message   => "Unable to determine subject to scheduling "
                    & "group ID mapping with cyclic events");
         end select;

         Assert (Condition => Result,
                 Message   => "Subject to scheduling group ID mapping with"
                 & "cyclic events mismatch");
      end;

      --  Check that non-schedulable subjects are mapped to the No_Group ID.

      Muxml.Utils.Remove_Elements
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/events/source/group/"
         & "event[@physical='example_request']");
      Muxml.Utils.Remove_Elements
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/events/source/group/"
         & "event[@physical='example_response']");
      Muxml.Utils.Remove_Elements
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/events/target/"
         & "event[@physical='example_request']");
      Muxml.Utils.Remove_Elements
        (Doc   => Policy.Doc,
         XPath => "/system/subjects/subject/events/target/"
         & "event[@physical='example_response']");

      declare
         Ref_Non_Sched_Mapping : constant ID_Map_Array
        := (0 => 1,
            1 => 2,
            2 => 0,  -- nic_linux
            3 => 6,
            4 => 4,
            5 => 5,
            6 => 0,  -- nic_sm
            7 => 6,
            8 => 3);
      begin
         Assert (Condition => Get_Subject_To_Scheduling_Group_Map
                 (Data => Policy) = Ref_Non_Sched_Mapping,
                 Message   => "Subject to scheduling group ID mapping with "
                 & "non-schedulable subjects mismatch");
      end;
--  begin read only
   end Test_Get_Subject_To_Scheduling_Group_Map;
--  end read only


--  begin read only
   procedure Test_Has_Multiple_Major_Frames (Gnattest_T : in out Test);
   procedure Test_Has_Multiple_Major_Frames_07b8ca (Gnattest_T : in out Test) renames Test_Has_Multiple_Major_Frames;
--  id:2.2/07b8caa719665851/Has_Multiple_Major_Frames/1/0/
   procedure Test_Has_Multiple_Major_Frames (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Multiple_Major_Frames
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/scheduling.xml");
         Assert (Condition => Has_Multiple_Major_Frames (Data => Data),
                 Message   => "Multiple major frames not detected");
      end Multiple_Major_Frames;

      ----------------------------------------------------------------------

      procedure Single_Major_Frame
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_Src,
                      File => "data/test_policy.xml");
         Assert (Condition => not Has_Multiple_Major_Frames (Data => Data),
                 Message   => "Multiple major frames detected");
      end Single_Major_Frame;
   begin
      Multiple_Major_Frames;
      Single_Major_Frame;
--  begin read only
   end Test_Has_Multiple_Major_Frames;
--  end read only


--  begin read only
   procedure Test_Is_Tau0_Scheduled (Gnattest_T : in out Test);
   procedure Test_Is_Tau0_Scheduled_4f84f8 (Gnattest_T : in out Test) renames Test_Is_Tau0_Scheduled;
--  id:2.2/4f84f8e858e9a397/Is_Tau0_Scheduled/1/0/
   procedure Test_Is_Tau0_Scheduled (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/scheduling.xml");
      Assert (Condition => Is_Tau0_Scheduled (Data => Data),
              Message   => "Tau0 is not scheduled");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/scheduling/majorFrame/cpu/minorFrame[@subject='tau0']",
         Name  => "subject",
         Value => "foobar");
      Assert (Condition => not Is_Tau0_Scheduled (Data => Data),
              Message   => "Tau0 is scheduled");
--  begin read only
   end Test_Is_Tau0_Scheduled;
--  end read only


--  begin read only
   procedure Test_Merge_XIncludes (Gnattest_T : in out Test);
   procedure Test_Merge_XIncludes_504615 (Gnattest_T : in out Test) renames Test_Merge_XIncludes;
--  id:2.2/50461583a452b67a/Merge_XIncludes/1/0/
   procedure Test_Merge_XIncludes (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U
        (Source : String)
         return Ada.Strings.Unbounded.Unbounded_String
         renames Ada.Strings.Unbounded.To_Unbounded_String;

      ----------------------------------------------------------------------

      procedure Multiple_Inc_Dirs_Precedence
      is
         Inc_1    : Muxml.XML_Data_Type;
         XML_Doc  : Muxml.XML_Data_Type;
         Filename : constant String := "obj/xinclude.xml";
      begin
         Muxml.Parse (Data => Inc_1,
                      Kind => Muxml.None,
                      File => "data/xinclude_1.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Inc_1.Doc,
            XPath => "/hardware/include[@href='xinclude_2.xml']",
            Name  => "href",
            Value => "xinclude_4.xml");
         Muxml.Write (Data => Inc_1,
                      Kind => Muxml.None,
                      File => "obj/xinclude_1.xml");

         Muxml.Parse (Data => XML_Doc,
                      Kind => Muxml.None,
                      File => "data/xinclude.xml");

         --  Check that the obj/xinclude_1.xml file takes precedence over
         --  data/xinclude_1.xml.

         Merge_XIncludes (Policy       => XML_Doc,
                          Include_Dirs =>
                            (1 => U ("obj"),
                             2 => U ("data"),
                             3 => U ("./..")));

         Muxml.Write (Data => XML_Doc,
                      Kind => Muxml.None,
                      File => Filename);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => "data/xinclude_incdir.xml"),
                 Message   => "Reference mismatch (incdir): " & Filename);

         Ada.Directories.Delete_File (Name => Filename);
         Ada.Directories.Delete_File (Name => "obj/xinclude_1.xml");
      end Multiple_Inc_Dirs_Precedence;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         XML_Doc  : Muxml.XML_Data_Type;
         Filename : constant String := "obj/xinclude.xml";
      begin
         Muxml.Parse (Data => XML_Doc,
                      Kind => Muxml.None,
                      File => "data/xinclude.xml");
         Merge_XIncludes (Policy       => XML_Doc,
                          Include_Dirs => (1 => U ("data")));

         Muxml.Write (Data => XML_Doc,
                      Kind => Muxml.None,
                      File => Filename);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => "data/xinclude_resolved.xml"),
                 Message   => "Reference mismatch: " & Filename);

         Ada.Directories.Delete_File (Name => Filename);
      end Positive_Test;
   begin
      Multiple_Inc_Dirs_Precedence;
      Positive_Test;
--  begin read only
   end Test_Merge_XIncludes;
--  end read only


--  begin read only
   procedure Test_Get_Image_Size (Gnattest_T : in out Test);
   procedure Test_Get_Image_Size_046aa6 (Gnattest_T : in out Test) renames Test_Get_Image_Size;
--  id:2.2/046aa68f3a717a5c/Get_Image_Size/1/0/
   procedure Test_Get_Image_Size (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Add_Memory_Region
        (Policy      => Data,
         Name        => "linux|bin",
         Address     => "16#0011_4000#",
         Size        => "16#0001_3000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_binary",
         File_Name   => "obj1.o",
         File_Offset => "none");

      Assert (Condition => Get_Image_Size (Policy => Data) = 16#127000#,
              Message   => "Image size mismatch");
--  begin read only
   end Test_Get_Image_Size;
--  end read only


--  begin read only
   procedure Test_Calculate_PCI_Cfg_Address (Gnattest_T : in out Test);
   procedure Test_Calculate_PCI_Cfg_Address_d974d1 (Gnattest_T : in out Test) renames Test_Calculate_PCI_Cfg_Address;
--  id:2.2/d974d1c05b67ac28/Calculate_PCI_Cfg_Address/1/0/
   procedure Test_Calculate_PCI_Cfg_Address (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Dom_Impl : DOM.Core.DOM_Implementation;
      Policy   : Muxml.XML_Data_Type;
      PCI      : DOM.Core.Node;
   begin
      Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);

      PCI := DOM.Core.Documents.Create_Element
        (Doc      => Policy.Doc,
         Tag_Name => "pci");
      DOM.Core.Elements.Set_Attribute (Elem  => PCI,
                                       Name  => "bus",
                                       Value => "16#05#");
      DOM.Core.Elements.Set_Attribute (Elem  => PCI,
                                       Name  => "device",
                                       Value => "16#1f#");
      DOM.Core.Elements.Set_Attribute (Elem  => PCI,
                                       Name  => "function",
                                       Value => "6");

      Assert (Condition => Calculate_PCI_Cfg_Address
              (Base_Address => 16#5000_0000#,
               PCI_Node     => PCI) = 16#505fe000#,
              Message   => "Address mismatch");
--  begin read only
   end Test_Calculate_PCI_Cfg_Address;
--  end read only


--  begin read only
   procedure Test_Is_Physical_Mmconf_Region (Gnattest_T : in out Test);
   procedure Test_Is_Physical_Mmconf_Region_a81275 (Gnattest_T : in out Test) renames Test_Is_Physical_Mmconf_Region;
--  id:2.2/a812758853ed6f2d/Is_Physical_Mmconf_Region/1/0/
   procedure Test_Is_Physical_Mmconf_Region (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Dom_Impl : DOM.Core.DOM_Implementation;
      Policy   : Muxml.XML_Data_Type;
      Devices  : DOM.Core.Node;
   begin
      Assert (Condition     => not Is_Physical_Mmconf_Region
              (Devices_Node => null,
               Addr         => 16#0500_2000#),
              Message       => "Mmconf without PCI base");

      Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);

      Devices := DOM.Core.Documents.Create_Element
        (Doc      => Policy.Doc,
         Tag_Name => "devices");
      DOM.Core.Elements.Set_Attribute (Elem  => Devices,
                                       Name  => "pciConfigAddress",
                                       Value => "16#0500_0000#");
      DOM.Core.Elements.Set_Attribute (Elem  => Devices,
                                       Name  => "pciConfigSize",
                                       Value => "16#3000#");

      Assert (Condition => Is_Physical_Mmconf_Region
              (Devices_Node => Devices,
               Addr         => 16#0500_0000#),
              Message   => "Unable to detect Mmconf region (1)");
      Assert (Condition => Is_Physical_Mmconf_Region
              (Devices_Node => Devices,
               Addr         => 16#0500_2000#),
              Message   => "Unable to detect Mmconf region (2)");
      Assert (Condition => not Is_Physical_Mmconf_Region
              (Devices_Node => Devices,
               Addr         => 16#0500_3000#),
              Message   => "Incorrect Mmconf region (1)");
      Assert (Condition => not Is_Physical_Mmconf_Region
              (Devices_Node => Devices,
               Addr         => 16#14ff_f000#),
              Message   => "Incorrect Mmconf region (2)");
      Assert (Condition => not Is_Physical_Mmconf_Region
              (Devices_Node => Devices,
               Addr         => 16#1500_0000#),
              Message   => "Incorrect Mmconf region (3)");
      Assert (Condition => not Is_Physical_Mmconf_Region
              (Devices_Node => Devices,
               Addr         => 16#04ff_f000#),
              Message   => "Incorrect Mmconf region (4)");
--  begin read only
   end Test_Is_Physical_Mmconf_Region;
--  end read only


--  begin read only
   procedure Test_To_APIC_ID (Gnattest_T : in out Test);
   procedure Test_To_APIC_ID_13d079 (Gnattest_T : in out Test) renames Test_To_APIC_ID;
--  id:2.2/13d079c1c944d680/To_APIC_ID/1/0/
   procedure Test_To_APIC_ID (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/hardware/processor/cpu[@apicId='0']",
         Name  => "cpuId",
         Value => "12");

      Assert (Condition => To_APIC_ID
              (Policy => Policy,
               CPU_ID => 12) = 0,
              Message   => "APIC ID mismatch");

      declare
         Dummy : Natural;
      begin
         Dummy := To_APIC_ID
           (Policy => Policy,
            CPU_ID => 22);
         Assert (Condition => True,
                 Message   => "Exception expected");

      exception
         when E : APIC_ID_Not_Found =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "APIC ID for CPU ID 22 not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_To_APIC_ID;
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
end Mutools.XML_Utils.Test_Data.Tests;
