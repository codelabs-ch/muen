--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Acpi.Asl.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Acpi.Asl.Test_Data.Tests is


--  begin read only
   procedure Test_DWordMemory (Gnattest_T : in out Test);
   procedure Test_DWordMemory_2778f7 (Gnattest_T : in out Test) renames Test_DWordMemory;
--  id:2.2/2778f78290b0efd1/DWordMemory/1/0/
   procedure Test_DWordMemory (Gnattest_T : in out Test) is
   --  acpi-asl.ads:24:4:DWordMemory
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref : constant String := "DWordMemory (ResourceProducer, PosDecode, "
        & "MinFixed, MaxFixed, Cacheable, ReadWrite, 0x0, 0xd2520000, "
        & "0xd252ffff, 0x0, 0x10000,,,, AddressRangeMemory, TypeStatic)";
   begin
      Assert (Condition => DWordMemory
              (Base_Address => 16#d252_0000#,
               Size         => 16#0001_0000#) = Ref,
              Message   => "DWordMemory string mismatch");
--  begin read only
   end Test_DWordMemory;
--  end read only

end Acpi.Asl.Test_Data.Tests;
