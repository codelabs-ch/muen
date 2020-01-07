--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Platform.Test_Data.

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
package body Mucfgcheck.Platform.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Alias_Physical_Device_References (Gnattest_T : in out Test);
   procedure Test_Alias_Physical_Device_References_8eb8d9 (Gnattest_T : in out Test) renames Test_Alias_Physical_Device_References;
--  id:2.2/8eb8d957391a24d8/Alias_Physical_Device_References/1/0/
   procedure Test_Alias_Physical_Device_References (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive tests, must no raise an exception.

      Alias_Physical_Device_References (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/platform/mappings/aliases/alias[@name='nic']",
         Name  => "physical",
         Value => "nonexistent");

      begin
         Alias_Physical_Device_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical device 'nonexistent' referenced by device "
                    & "alias 'nic' not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Alias_Physical_Device_References;
--  end read only


--  begin read only
   procedure Test_Alias_Physical_Device_Resource_References (Gnattest_T : in out Test);
   procedure Test_Alias_Physical_Device_Resource_References_7db453 (Gnattest_T : in out Test) renames Test_Alias_Physical_Device_Resource_References;
--  id:2.2/7db453008dc7a438/Alias_Physical_Device_Resource_References/1/0/
   procedure Test_Alias_Physical_Device_Resource_References (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive tests, must no raise an exception.

      Alias_Physical_Device_Resource_References (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/platform/mappings/aliases/alias/"
         & "resource[@name='mem1']",
         Name  => "physical",
         Value => "nonexistent");

      begin
         Alias_Physical_Device_Resource_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical device resource 'nonexistent' referenced by "
                    & "alias resource 'mem1' of device alias 'nic' not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Alias_Physical_Device_Resource_References;
--  end read only


--  begin read only
   procedure Test_Class_Physical_Device_References (Gnattest_T : in out Test);
   procedure Test_Class_Physical_Device_References_5e6f8d (Gnattest_T : in out Test) renames Test_Class_Physical_Device_References;
--  id:2.2/5e6f8d2d67e06d2f/Class_Physical_Device_References/1/0/
   procedure Test_Class_Physical_Device_References (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive tests, must no raise an exception.

      Class_Physical_Device_References (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/platform/mappings/classes/class/"
         & "device[@physical='wireless']",
         Name  => "physical",
         Value => "nonexistent");

      begin
         Class_Physical_Device_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical device 'nonexistent' referenced by device "
                    & "class 'network_devices' not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Class_Physical_Device_References;
--  end read only


--  begin read only
   procedure Test_Subject_Alias_Resource_References (Gnattest_T : in out Test);
   procedure Test_Subject_Alias_Resource_References_643b2b (Gnattest_T : in out Test) renames Test_Subject_Alias_Resource_References;
--  id:2.2/643b2b83e1c6c4a9/Subject_Alias_Resource_References/1/0/
   procedure Test_Subject_Alias_Resource_References (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data     : Muxml.XML_Data_Type;
      Dev_Node : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Setup_Nic_Alias:
      declare
         Dummy : DOM.Core.Node;
      begin
         Dev_Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/devices/device"
            & "[@physical='ethernet']");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Dev_Node,
            Name  => "physical",
            Value => "nic");
         Muxml.Utils.Set_Attribute
           (Doc   => Dev_Node,
            XPath => "irq[@physical='irq']",
            Name  => "physical",
            Value => "interrupt");
         Muxml.Utils.Set_Attribute
           (Doc   => Dev_Node,
            XPath => "memory[@physical='mmio1']",
            Name  => "physical",
            Value => "mem1");
         Muxml.Utils.Set_Attribute
           (Doc   => Dev_Node,
            XPath => "memory[@physical='mmio2']",
            Name  => "physical",
            Value => "mem2");
         Dummy := DOM.Core.Nodes.Remove_Child
           (N         => Dev_Node,
            Old_Child => Muxml.Utils.Get_Element
              (Doc   => Dev_Node,
               XPath => "memory[@physical='mmconf']"));
      end Setup_Nic_Alias;

      --  Positive tests, must no raise an exception.

      Subject_Alias_Resource_References (XML_Data => Data);

      Nonexistent_IO_Port:
      declare
         IO_Port : DOM.Core.Node;
      begin
         IO_Port := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "ioPort");
         DOM.Core.Elements.Set_Attribute
           (Elem  => IO_Port,
            Name  => "logical",
            Value => "port");
         DOM.Core.Elements.Set_Attribute
           (Elem  => IO_Port,
            Name  => "physical",
            Value => "nonexistent_ioport");
         Muxml.Utils.Append_Child
           (Node      => Dev_Node,
            New_Child => IO_Port);

         Subject_Alias_Resource_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Logical device 'eth0' of subject 'linux' references "
                    & "resource 'nonexistent_ioport' that is not provided by "
                    & "device alias 'nic'",
                    Message   => "Exception mismatch");
      end Nonexistent_IO_Port;

      Nonexistent_Memory:
      begin
         Muxml.Utils.Set_Attribute
           (Doc   => Dev_Node,
            XPath => "memory[@physical='mem2']",
            Name  => "physical",
            Value => "nonexistent_mem");

         Subject_Alias_Resource_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Logical device 'eth0' of subject 'linux' references "
                    & "resource 'nonexistent_mem' that is not provided by "
                    & "device alias 'nic'",
                    Message   => "Exception mismatch");
      end Nonexistent_Memory;

      Nonexistent_Irq:
      begin
         Muxml.Utils.Set_Attribute
           (Doc   => Dev_Node,
            XPath => "irq[@physical='interrupt']",
            Name  => "physical",
            Value => "nonexistent_irq");

         Subject_Alias_Resource_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Logical device 'eth0' of subject 'linux' references "
                    & "resource 'nonexistent_irq' that is not provided by "
                    & "device alias 'nic'",
                    Message   => "Exception mismatch");
      end Nonexistent_Irq;
--  begin read only
   end Test_Subject_Alias_Resource_References;
--  end read only


--  begin read only
   procedure Test_Kernel_Diagnostics_Device_Reference (Gnattest_T : in out Test);
   procedure Test_Kernel_Diagnostics_Device_Reference_d0d3aa (Gnattest_T : in out Test) renames Test_Kernel_Diagnostics_Device_Reference;
--  id:2.2/d0d3aa99d21b5bf6/Kernel_Diagnostics_Device_Reference/1/0/
   procedure Test_Kernel_Diagnostics_Device_Reference (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Invalid_Reference
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/platform/kernelDiagnostics/device",
            Name  => "physical",
            Value => "nonexistent");

         begin
            Kernel_Diagnostics_Device_Reference (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Physical device 'nonexistent' designated as kernel "
                       & "diagnostics device not found",
                       Message   => "Exception mismatch");
         end;
      end Invalid_Reference;

      ----------------------------------------------------------------------

      procedure No_Diagnostics
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/test_policy_src.xml");

         --  Positive tests for type 'none', must no raise an exception.

         Kernel_Diagnostics_Device_Reference (XML_Data => Data);
      end No_Diagnostics;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         --  Positive tests, must not raise an exception.

         Kernel_Diagnostics_Device_Reference (XML_Data => Data);
      end Positive_Test;
   begin
      Positive_Test;
      No_Diagnostics;
      Invalid_Reference;
--  begin read only
   end Test_Kernel_Diagnostics_Device_Reference;
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
end Mucfgcheck.Platform.Test_Data.Tests;
