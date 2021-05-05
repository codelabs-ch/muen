--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Kernel.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Mucfgcheck.Validation_Errors;
--  begin read only
--  end read only
package body Mucfgcheck.Kernel.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_CPU_Local_Data_Address_Equality (Gnattest_T : in out Test);
   procedure Test_CPU_Local_Data_Address_Equality_e6a851 (Gnattest_T : in out Test) renames Test_CPU_Local_Data_Address_Equality;
--  id:2.2/e6a85142aff94dec/CPU_Local_Data_Address_Equality/1/0/
   procedure Test_CPU_Local_Data_Address_Equality (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      CPU_Local_Data_Address_Equality (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu[@id='0']/memory"
         & "[@physical='kernel_data_0']",
         Name  => "virtualAddress",
         Value => "16#0021_9000#");

      CPU_Local_Data_Address_Equality (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Attribute 'virtualAddress => 16#0011_0000#' of "
               & "'kernel_data_1' kernel CPU-local data element "
               & "differs"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_CPU_Local_Data_Address_Equality;
--  end read only


--  begin read only
   procedure Test_CPU_Local_BSS_Address_Equality (Gnattest_T : in out Test);
   procedure Test_CPU_Local_BSS_Address_Equality_457535 (Gnattest_T : in out Test) renames Test_CPU_Local_BSS_Address_Equality;
--  id:2.2/457535f84c28475a/CPU_Local_BSS_Address_Equality/1/0/
   procedure Test_CPU_Local_BSS_Address_Equality (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      CPU_Local_BSS_Address_Equality (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu[@id='0']/memory"
         & "[@physical='kernel_bss_0']",
         Name  => "virtualAddress",
         Value => "16#0021_9000#");

      CPU_Local_BSS_Address_Equality (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Attribute 'virtualAddress => 16#0011_1000#' of "
               & "'kernel_bss_1' kernel CPU-local BSS element "
               & "differs"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_CPU_Local_BSS_Address_Equality;
--  end read only


--  begin read only
   procedure Test_Global_Data_Address_Equality (Gnattest_T : in out Test);
   procedure Test_Global_Data_Address_Equality_ea7e5e (Gnattest_T : in out Test) renames Test_Global_Data_Address_Equality;
--  id:2.2/ea7e5e33cb5d69f2/Global_Data_Address_Equality/1/0/
   procedure Test_Global_Data_Address_Equality (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Global_Data_Address_Equality (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu[@id='0']/memory"
         & "[@physical='kernel_global_data']",
         Name  => "virtualAddress",
         Value => "16#0021_9000#");

      Global_Data_Address_Equality (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Attribute 'virtualAddress => 16#0011_9000#' of "
               & "'kernel_global_data' kernel global data element "
               & "differs"),
              Message   => "Exception mismatch (1)");

       Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu[@id='0']/memory"
         & "[@physical='kernel_global_data']",
         Name  => "physical",
         Value => "foo");

      Global_Data_Address_Equality (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Required kernel global data mappings not present "
               & "(expected 4, found 3)"),
              Message   => "Exception mismatch (2)");
--  begin read only
   end Test_Global_Data_Address_Equality;
--  end read only


--  begin read only
   procedure Test_Stack_Address_Equality (Gnattest_T : in out Test);
   procedure Test_Stack_Address_Equality_61fb48 (Gnattest_T : in out Test) renames Test_Stack_Address_Equality;
--  id:2.2/61fb4824388cbd39/Stack_Address_Equality/1/0/
   procedure Test_Stack_Address_Equality (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Stack_Address_Equality (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory"
         & "[@physical='kernel_stack_1']",
         Name  => "virtualAddress",
         Value => "16#0031_0000#");

      Stack_Address_Equality (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Attribute 'virtualAddress => 16#0031_0000#' of "
               & "'kernel_stack_1' kernel stack memory element differs"),
              Message   => "Exception mismatch (1)");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory"
         & "[@physical='kernel_stack_1']",
         Name  => "virtualAddress",
         Value => "16#0011_3000#");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory"
         & "[@physical='kernel_interrupt_stack_1']",
         Name  => "virtualAddress",
         Value => "16#0051_0000#");

      Stack_Address_Equality (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Attribute 'virtualAddress => 16#0051_0000#' of "
               & "'kernel_interrupt_stack_1' kernel interrupt stack "
               & "memory element differs"),
              Message   => "Exception mismatch (2)");
--  begin read only
   end Test_Stack_Address_Equality;
--  end read only


--  begin read only
   procedure Test_Crash_Audit_Address_Equality (Gnattest_T : in out Test);
   procedure Test_Crash_Audit_Address_Equality_9755fa (Gnattest_T : in out Test) renames Test_Crash_Audit_Address_Equality;
--  id:2.2/9755fa372ca7d37b/Crash_Audit_Address_Equality/1/0/
   procedure Test_Crash_Audit_Address_Equality (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Crash_Audit_Address_Equality (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory"
         & "[@physical='crash_audit'][1]",
         Name  => "virtualAddress",
         Value => "16#0021_0000#");

      Crash_Audit_Address_Equality (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Attribute 'virtualAddress => 16#000e_0001_9000#' of"
               & " 'crash_audit' crash audit region element differs"),
              Message   => "Exception mismatch (1)");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory"
         & "[@physical='crash_audit'][1]",
         Name  => "physical",
         Value => "something");

      Crash_Audit_Address_Equality (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Required crash audit mappings not present (expected 4, "
               & "found 3)"),
              Message   => "Exception mismatch (2)");
--  begin read only
   end Test_Crash_Audit_Address_Equality;
--  end read only


--  begin read only
   procedure Test_Stack_Layout (Gnattest_T : in out Test);
   procedure Test_Stack_Layout_61b627 (Gnattest_T : in out Test) renames Test_Stack_Layout;
--  id:2.2/61b6272803731039/Stack_Layout/1/0/
   procedure Test_Stack_Layout (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Stack_Layout (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  No stack region on CPU 2.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu[@id='2']/memory"
         & "[@logical='stack']",
         Name  => "logical",
         Value => "X");
      Stack_Layout (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "CPU 2 has no stack region mapping"),
              Message   => "Exception mismatch (1)");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu[@id='2']/memory"
         & "[@logical='X']",
         Name  => "logical",
         Value => "stack");

      --  No interrupt stack region on CPU 2.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu[@id='2']/memory"
         & "[@logical='interrupt_stack']",
         Name  => "logical",
         Value => "X");
      Stack_Layout (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "CPU 2 has no interrupt stack region mapping"),
              Message   => "Exception mismatch (2)");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu[@id='2']/memory"
         & "[@logical='X']",
         Name  => "logical",
         Value => "interrupt_stack");

      --  No guard page below stack on CPU 3.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu[@id='3']/memory"
         & "[@logical='bss']",
         Name  => "virtualAddress",
         Value => "16#0011_2000#");
      Stack_Layout (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Expected unmapped page on CPU 3 below kernel stack @ "
               & "16#0011_3000#, found 'bss'"),
              Message   => "Exception mismatch (3)");

      --  No guard page above stack on CPU 3.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu[@id='3']/memory"
         & "[@logical='bss']",
         Name  => "virtualAddress",
         Value => "16#0011_4000#");
      Stack_Layout (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Expected unmapped page on CPU 3 above kernel stack @ "
               & "16#0011_3fff#, found 'bss'"),
               Message   => "Exception mismatch (4)");

      --  No guard page below interrupt stack on CPU 3.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu[@id='3']/memory"
         & "[@logical='bss']",
         Name  => "virtualAddress",
         Value => "16#0011_5000#");
      Stack_Layout (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Expected unmapped page on CPU 3 below kernel stack @ "
               & "16#0011_6000#, found 'bss'"),
              Message   => "Exception mismatch (5)");

      --  No guard page above interrupt stack on CPU 3.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu[@id='3']/memory"
         & "[@logical='bss']",
         Name  => "virtualAddress",
         Value => "16#0011_7000#");
      Stack_Layout (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Expected unmapped page on CPU 3 above kernel stack @ "
               & "16#0011_6fff#, found 'bss'"),
              Message   => "Exception mismatch (6)");
--  begin read only
   end Test_Stack_Layout;
--  end read only


--  begin read only
   procedure Test_IOMMU_Consecutiveness (Gnattest_T : in out Test);
   procedure Test_IOMMU_Consecutiveness_fc88d4 (Gnattest_T : in out Test) renames Test_IOMMU_Consecutiveness;
--  id:2.2/fc88d4365ce63af7/IOMMU_Consecutiveness/1/0/
   procedure Test_IOMMU_Consecutiveness (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/devices/device[@physical='iommu_2']/memory",
         Name  => "virtualAddress",
         Value => "16#0021_0000#");

      IOMMU_Consecutiveness (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Mapping of MMIO region of IOMMU 'iommu_1' not adjacent "
               & "to other IOMMU regions"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_IOMMU_Consecutiveness;
--  end read only


--  begin read only
   procedure Test_CPU_Memory_Section_Count (Gnattest_T : in out Test);
   procedure Test_CPU_Memory_Section_Count_14dd51 (Gnattest_T : in out Test) renames Test_CPU_Memory_Section_Count;
--  id:2.2/14dd51df8988d4a1/CPU_Memory_Section_Count/1/0/
   procedure Test_CPU_Memory_Section_Count (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Mem_Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/kernel/memory");
      begin
         Muxml.Utils.Remove_Child (Node       => Mem_Node,
                                   Child_Name => "cpu");

         CPU_Memory_Section_Count (XML_Data => Data);
         Assert (Condition => Validation_Errors.Contains
                 (Msg => "Invalid number of kernel memory CPU section(s), 3 "
                  & "present but 4 required"),
                 Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_CPU_Memory_Section_Count;
--  end read only


--  begin read only
   procedure Test_Virtual_Memory_Overlap (Gnattest_T : in out Test);
   procedure Test_Virtual_Memory_Overlap_7973e4 (Gnattest_T : in out Test) renames Test_Virtual_Memory_Overlap;
--  id:2.2/7973e4663e077f6d/Virtual_Memory_Overlap/1/0/
   procedure Test_Virtual_Memory_Overlap (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Virtual_Memory_Overlap (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu[@id='1']/"
         & "memory[@physical='kernel_data_1']",
         Name  => "virtualAddress",
         Value => "16#0010_0000#");

      Virtual_Memory_Overlap (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Overlap of virtual memory region 'text' and 'data' "
               & "of kernel running on CPU 1"),
              Message   => "Exception mismatch");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu[@id='0']/"
         & "memory[@logical='tau0_interface']",
         Name  => "virtualAddress",
         Value => "16#001f_c000#");

      Virtual_Memory_Overlap (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Overlap of virtual memory region 'tau0_interface' "
               & "and 'ioapic->mmio' of kernel running on CPU 0"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Virtual_Memory_Overlap;
--  end read only


--  begin read only
   procedure Test_System_Board_Reference (Gnattest_T : in out Test);
   procedure Test_System_Board_Reference_9057a6 (Gnattest_T : in out Test) renames Test_System_Board_Reference;
--  id:2.2/9057a6b12a851d5f/System_Board_Reference/1/0/
   procedure Test_System_Board_Reference (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data        : Muxml.XML_Data_Type;
      Board, Node : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Board := Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/system/kernel/devices/device[@logical='system_board']");

      --  Positive test, must not raise an exception.

      System_Board_Reference (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      Node := DOM.Core.Nodes.Remove_Child
        (N         => Board,
         Old_Child => Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/kernel/devices/device/ioPort"
            & "[@logical='reset_port']"));

      System_Board_Reference (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Kernel system board reference does not provide logical"
               & " reset port"),
              Message   => "Exception mismatch (1)");

      Node := DOM.Core.Nodes.Append_Child
        (N         => Board,
         New_Child => Node);
      Node := DOM.Core.Nodes.Remove_Child
        (N         => Board,
         Old_Child => Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/kernel/devices/device/ioPort"
            & "[@logical='poweroff_port']"));

      System_Board_Reference (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Kernel system board reference does not provide logical"
               & " poweroff port"),
              Message   => "Exception mismatch (2)");

      Node := DOM.Core.Nodes.Remove_Child
        (N         => Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/kernel/devices"),
         Old_Child => Board);

      System_Board_Reference (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Kernel system board reference not present"),
              Message   => "Exception mismatch (3)");
--  begin read only
   end Test_System_Board_Reference;
--  end read only


--  begin read only
   procedure Test_Diagnostics_Device_Reference (Gnattest_T : in out Test);
   procedure Test_Diagnostics_Device_Reference_91f2d6 (Gnattest_T : in out Test) renames Test_Diagnostics_Device_Reference;
--  id:2.2/91f2d6f41421c208/Diagnostics_Device_Reference/1/0/
   procedure Test_Diagnostics_Device_Reference (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Diagnostics_Device_Reference (XML_Data => Data);
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");

      --  Resource name mismatch

      Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/kernel/devices/device[@physical='debugconsole']"
            & "/ioPort",
            Name  => "physical",
            Value => "nonexistent");

      begin
         Diagnostics_Device_Reference (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
            when Validation_Errors.Validation_Error =>
            Assert (Condition => Validation_Errors.Contains
                    (Msg => "Kernel debug console reference to physical Ioport "
                     & "'nonexistent' not specified by platform kernel "
                     & "diagnostics device"),
                    Message   => "Exception mismatch (Resource name)");
      end;

      --  Resource type mismatch

      declare
         Dev_Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/kernel/devices/device"
            & "[@physical='debugconsole']");
         New_Node : constant DOM.Core.Node
           := DOM.Core.Documents.Create_Element
             (Doc      => Data.Doc,
              Tag_Name => "memory");
      begin
         Muxml.Utils.Remove_Child (Node       => Dev_Node,
                                   Child_Name => "ioPort");
         DOM.Core.Elements.Set_Attribute (Elem  => New_Node,
                                          Name  => "physical",
                                          Value => "port");
         Muxml.Utils.Append_Child (Node      => Dev_Node,
                                   New_Child => New_Node);

         Diagnostics_Device_Reference (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Validation_Errors.Validation_Error =>
            Assert (Condition => Validation_Errors.Contains
                    (Msg => "Kernel debug console reference to physical Memory "
                     & "'port' not specified by platform kernel "
                     & "diagnostics device"),
                    Message   => "Exception mismatch (Resource type)");
      end;

      --  Resource count mismatch

      Muxml.Utils.Remove_Elements
        (Doc   => Data.Doc,
         XPath => "/system/kernel/devices/device"
         & "[@physical='debugconsole']/*");

      Diagnostics_Device_Reference (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Kernel debug console and platform diagnostics device "
               & "resource count mismatch: 0 /= 1"),
              Message   => "Exception mismatch (Resource count)");

      --  Device name mismatch

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/devices/device[@physical='debugconsole']",
         Name  => "physical",
         Value => "nonexistent");

      Diagnostics_Device_Reference (XML_Data => Data);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Kernel debug console and platform diagnostics device "
               & "physical reference mismatch: nonexistent /= "
               & "debugconsole"),
              Message   => "Exception mismatch (Resource count)");
--  begin read only
   end Test_Diagnostics_Device_Reference;
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
end Mucfgcheck.Kernel.Test_Data.Tests;
