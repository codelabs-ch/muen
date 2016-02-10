--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into VTd.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body VTd.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Get_BDF (Gnattest_T : in out Test);
   procedure Test_Get_BDF_a2731a (Gnattest_T : in out Test) renames Test_Get_BDF;
--  id:2.2/a2731a173049c762/Get_BDF/1/0/
   procedure Test_Get_BDF (Gnattest_T : in out Test) is
   --  vtd-utils.ads:33:4:Get_BDF
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Mutools.PCI.BDF_Type;

      Impl      : DOM.Core.DOM_Implementation;
      Data      : Muxml.XML_Data_Type;
      Node, Dev : DOM.Core.Node;
      BDF       : Mutools.PCI.BDF_Type;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Dev := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "device");
      Muxml.Utils.Append_Child
        (Node      => Data.Doc,
         New_Child => Dev);

      BDF := Get_BDF (Dev => Dev);
      Assert (Condition => BDF = Mutools.PCI.Null_BDF,
              Message   => "BDF not nil");

      Node := DOM.Core.Nodes.Append_Child
        (N         => Dev,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "pci"));
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "bus",
         Value => "12");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "device",
         Value => "20");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "function",
         Value => "7");

      BDF := Get_BDF (Dev => Dev);
      Assert (Condition => BDF = Mutools.PCI.Create
              (Bus    => 12,
               Device => 20,
               Func   => 7),
              Message   => "BDF mismatch");
--  begin read only
   end Test_Get_BDF;
--  end read only


--  begin read only
   procedure Test_Get_IR_TM_SID (Gnattest_T : in out Test);
   procedure Test_Get_IR_TM_SID_db1b39 (Gnattest_T : in out Test) renames Test_Get_IR_TM_SID;
--  id:2.2/db1b3998c0f5a7dc/Get_IR_TM_SID/1/0/
   procedure Test_Get_IR_TM_SID (Gnattest_T : in out Test) is
   --  vtd-utils.ads:37:4:Get_IR_TM_SID
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_16;
      use type VTd.Tables.Bit_Type;

      TM  : Tables.Bit_Type;
      SID : Interfaces.Unsigned_16;
      BDF : Mutools.PCI.BDF_Type := Mutools.PCI.Null_BDF;
   begin
      Get_IR_TM_SID (Kind => Mutools.XML_Utils.IRQ_ISA,
                     BDF  => BDF,
                     TM   => TM,
                     SID  => SID);
      Assert (Condition => TM = 0,
              Message   => "ISA: TM not 0");
      Assert (Condition => SID = Mutools.PCI.IOAPIC_Bus_Dev_Func,
              Message   => "ISA: SID mismatch");

      Get_IR_TM_SID (Kind => Mutools.XML_Utils.IRQ_PCI_LSI,
                     BDF  => BDF,
                     TM   => TM,
                     SID  => SID);
      Assert (Condition => TM = 1,
              Message   => "LSI: TM not 1");
      Assert (Condition => SID = Mutools.PCI.IOAPIC_Bus_Dev_Func,
              Message   => "LSI: SID mismatch");

      BDF := Mutools.PCI.Create (Bus    => 12,
                                 Device => 16,
                                 Func   => 5);
      Get_IR_TM_SID (Kind => Mutools.XML_Utils.IRQ_PCI_MSI,
                     BDF  => BDF,
                     TM   => TM,
                     SID  => SID);
      Assert (Condition => TM = 0,
              Message   => "MSI: TM not 0");
      Assert (Condition => SID = 3205,
              Message   => "MSI: SID mismatch");
--  begin read only
   end Test_Get_IR_TM_SID;
--  end read only

end VTd.Utils.Test_Data.Tests;
