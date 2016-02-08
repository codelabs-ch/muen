--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into VTd.PCI.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body VTd.PCI.Test_Data.Tests is


--  begin read only
   procedure Test_Create (Gnattest_T : in out Test);
   procedure Test_Create_62ce3a (Gnattest_T : in out Test) renames Test_Create;
--  id:2.2/62ce3a264e244093/Create/1/0/
   procedure Test_Create (Gnattest_T : in out Test) is
   --  vtd-pci.ads:34:4:Create
--  end read only

      pragma Unreferenced (Gnattest_T);

      BDF : BDF_Type := Create
        (Bus    => 56,
         Device => 23,
         Func   => 4);
   begin
      Assert (Condition => BDF.Bus = 56,
              Message   => "Bus mismatch");
      Assert (Condition => BDF.Device = 23,
              Message   => "Device mismatch");
      Assert (Condition => BDF.Func = 4,
              Message   => "Function mismatch");
--  begin read only
   end Test_Create;
--  end read only


--  begin read only
   procedure Test_To_SID (Gnattest_T : in out Test);
   procedure Test_To_SID_557930 (Gnattest_T : in out Test) renames Test_To_SID;
--  id:2.2/5579300fa08b424d/To_SID/1/0/
   procedure Test_To_SID (Gnattest_T : in out Test) is
   --  vtd-pci.ads:44:4:To_SID
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_16;
   begin
      Assert (Condition => To_SID
              (BDF =>
                 (Bus    => 16#f0#,
                  Device => 16#1f#,
                  Func   => 0)) = IOAPIC_Bus_Dev_Func,
              Message   => "SID mismatch");
--  begin read only
   end Test_To_SID;
--  end read only

end VTd.PCI.Test_Data.Tests;
