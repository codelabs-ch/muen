--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Spec.Utils.Test_Data.

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
package body Spec.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Get_IRQ_Count (Gnattest_T : in out Test);
   procedure Test_Get_IRQ_Count_6a25e1 (Gnattest_T : in out Test) renames Test_Get_IRQ_Count;
--  id:2.2/6a25e10d4be9c4b3/Get_IRQ_Count/1/0/
   procedure Test_Get_IRQ_Count (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Physical_IRQs : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Policy.Doc,
              XPath => "/system/hardware/devices/device/irq");
      begin
         Assert (Condition => Get_IRQ_Count
                 (IRQs     => Physical_IRQs,
                  IRQ_Kind => Mutools.XML_Utils.IRQ_ISA) = 2,
                 Message   => "ISA IRQ count mismatch");
         Assert (Condition => Get_IRQ_Count
                 (IRQs     => Physical_IRQs,
                  IRQ_Kind => Mutools.XML_Utils.IRQ_PCI_LSI) = 1,
                 Message   => "PCI LSI IRQ count mismatch");
         Assert (Condition => Get_IRQ_Count
                 (IRQs     => Physical_IRQs,
                  IRQ_Kind => Mutools.XML_Utils.IRQ_PCI_MSI) = 1,
                 Message   => "PCI MSI IRQ count mismatch");
      end;
--  begin read only
   end Test_Get_IRQ_Count;
--  end read only


--  begin read only
   procedure Test_Get_APIC_CPU_ID_Map (Gnattest_T : in out Test);
   procedure Test_Get_APIC_CPU_ID_Map_ec024a (Gnattest_T : in out Test) renames Test_Get_APIC_CPU_ID_Map;
--  id:2.2/ec024aad4bc486f6/Get_APIC_CPU_ID_Map/1/0/
   procedure Test_Get_APIC_CPU_ID_Map (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl   : DOM.Core.DOM_Implementation;
      Node   : DOM.Core.Node;
      Nodes  : DOM.Core.Node_List;
      Policy : Muxml.XML_Data_Type;
      Ref    : Utils.APIC_To_CPU_ID_Array
        := (0, 1, 0, 0, 0, 2);
   begin
      Policy.Doc := DOM.Core.Create_Document (Implementation => Impl);
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Policy.Doc,
         Tag_Name => "cpu");

      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "apicId",
         Value => "0");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "cpuId",
         Value => "0");
      DOM.Core.Append_Node (List => Nodes,
                            N    => Node);

      Node := DOM.Core.Nodes.Clone_Node (N => Node, Deep => False);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "apicId",
         Value => "2");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "cpuId",
         Value => "1");
      DOM.Core.Append_Node (List => Nodes,
                            N    => Node);

      Node := DOM.Core.Nodes.Clone_Node (N => Node, Deep => False);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "apicId",
         Value => "10");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "cpuId",
         Value => "2");
      DOM.Core.Append_Node (List => Nodes,
                            N    => Node);

      Assert (Condition => Get_APIC_CPU_ID_Map (CPU_Nodes => Nodes) = Ref,
              Message   => "CPU ID Map mismatch");
--  begin read only
   end Test_Get_APIC_CPU_ID_Map;
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
end Spec.Utils.Test_Data.Tests;
