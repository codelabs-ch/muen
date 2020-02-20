--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into VTd.Utils.Test_Data.

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
package body VTd.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Get_IR_TM_SID (Gnattest_T : in out Test);
   procedure Test_Get_IR_TM_SID_201ae5 (Gnattest_T : in out Test) renames Test_Get_IR_TM_SID;
--  id:2.2/201ae557a111f1d1/Get_IR_TM_SID/1/0/
   procedure Test_Get_IR_TM_SID (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_16;
      use type VTd.Tables.Bit_Type;

      IOAPIC_SID : constant String := "16#f0f8#";

      TM  : Tables.Bit_Type;
      SID : Interfaces.Unsigned_16;
      BDF : Mutools.PCI.BDF_Type := Mutools.PCI.Null_BDF;
   begin
      Get_IR_TM_SID (Kind       => Mutools.XML_Utils.IRQ_ISA,
                     BDF        => BDF,
                     IOAPIC_SID => IOAPIC_SID,
                     TM         => TM,
                     SID        => SID);
      Assert (Condition => TM = 0,
              Message   => "ISA: TM not 0");
      Assert (Condition => SID = Interfaces.Unsigned_16'Value (IOAPIC_SID),
              Message   => "ISA: SID mismatch");

      Get_IR_TM_SID (Kind       => Mutools.XML_Utils.IRQ_PCI_LSI,
                     BDF        => BDF,
                     IOAPIC_SID => IOAPIC_SID,
                     TM         => TM,
                     SID        => SID);
      Assert (Condition => TM = 1,
              Message   => "LSI: TM not 1");
      Assert (Condition => SID = Interfaces.Unsigned_16'Value (IOAPIC_SID),
              Message   => "LSI: SID mismatch");

      BDF := Mutools.PCI.Create (Bus    => 12,
                                 Device => 16,
                                 Func   => 5);
      Get_IR_TM_SID (Kind       => Mutools.XML_Utils.IRQ_PCI_MSI,
                     BDF        => BDF,
                     IOAPIC_SID => IOAPIC_SID,
                     TM         => TM,
                     SID        => SID);
      Assert (Condition => TM = 0,
              Message   => "MSI: TM not 0");
      Assert (Condition => SID = 3205,
              Message   => "MSI: SID mismatch");
--  begin read only
   end Test_Get_IR_TM_SID;
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
end VTd.Utils.Test_Data.Tests;
