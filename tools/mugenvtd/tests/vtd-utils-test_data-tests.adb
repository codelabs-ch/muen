--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into VTd.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body VTd.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Get_IR_TM_SID (Gnattest_T : in out Test);
   procedure Test_Get_IR_TM_SID_ca4b39 (Gnattest_T : in out Test) renames Test_Get_IR_TM_SID;
--  id:2.2/ca4b399f8c877443/Get_IR_TM_SID/1/0/
   procedure Test_Get_IR_TM_SID (Gnattest_T : in out Test) is
   --  vtd-utils.ads:29:4:Get_IR_TM_SID
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_16;
      use type VTd.Tables.Bit_Type;

      TM  : Tables.Bit_Type;
      SID : Interfaces.Unsigned_16;
   begin
      Get_IR_TM_SID (Kind => Mutools.XML_Utils.IRQ_ISA,
                     TM   => TM,
                     SID  => SID);
      Assert (Condition => TM = 0,
              Message   => "ISA: TM not 0");
      Assert (Condition => SID = IOAPIC_Bus_Dev_Func,
              Message   => "ISA: SID mismatch");

      Get_IR_TM_SID (Kind => Mutools.XML_Utils.IRQ_PCI_LSI,
                     TM   => TM,
                     SID  => SID);
      Assert (Condition => TM = 1,
              Message   => "LSI: TM not 1");
      Assert (Condition => SID = IOAPIC_Bus_Dev_Func,
              Message   => "LSI: SID mismatch");

      Get_IR_TM_SID (Kind => Mutools.XML_Utils.IRQ_PCI_MSI,
                     TM   => TM,
                     SID  => SID);
      Assert (Condition => TM = 1,
              Message   => "MSI: TM not 1");
      Assert (Condition => SID = IOAPIC_Bus_Dev_Func,
              Message   => "MSI: SID mismatch");
--  begin read only
   end Test_Get_IR_TM_SID;
--  end read only

end VTd.Utils.Test_Data.Tests;
