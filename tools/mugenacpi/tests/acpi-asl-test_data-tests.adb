--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Acpi.Asl.Test_Data.

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
package body Acpi.Asl.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_DWordMemory (Gnattest_T : in out Test);
   procedure Test_DWordMemory_9a4014 (Gnattest_T : in out Test) renames Test_DWordMemory;
--  id:2.2/9a4014f56644c4de/DWordMemory/1/0/
   procedure Test_DWordMemory (Gnattest_T : in out Test) is
   --  acpi-asl.ads:24:4:DWordMemory
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_1 : constant String := "DWordMemory (ResourceProducer, PosDecode, "
        & "MinFixed, MaxFixed, Cacheable, ReadWrite, 0x0, 0xd2520000, "
        & "0xd252ffff, 0x0, 0x10000,,,, AddressRangeMemory, TypeStatic)";
      Ref_2 : constant String := "DWordMemory (ResourceProducer, PosDecode, "
        & "MinFixed, MaxFixed, NonCacheable, ReadWrite, 0x0, 0xf8000000, "
        & "0xf8ffffff, 0x0, 0x1000000,,,, AddressRangeMemory, TypeStatic)";
   begin
      Assert (Condition => DWordMemory
              (Base_Address => 16#d252_0000#,
               Size         => 16#0001_0000#,
               Cacheable    => True) = Ref_1,
              Message   => "DWordMemory string mismatch (1)");
      Assert (Condition => DWordMemory
              (Base_Address => 16#f800_0000#,
               Size         => 16#0100_0000#,
               Cacheable    => False) = Ref_2,
              Message   => "DWordMemory string mismatch (2)");
--  begin read only
   end Test_DWordMemory;
--  end read only


--  begin read only
   procedure Test_IO (Gnattest_T : in out Test);
   procedure Test_IO_93fe76 (Gnattest_T : in out Test) renames Test_IO;
--  id:2.2/93fe76d2e3938c77/IO/1/0/
   procedure Test_IO (Gnattest_T : in out Test) is
   --  acpi-asl.ads:32:4:IO
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_1 : constant String := "IO (Decode16, 0x3f8, 0x3f8, 0x08, 0x8,)";
      Ref_2 : constant String := "IO (Decode16, 0x50b0, 0x50b0, 0x08, 0x8,)";
   begin
      Assert (Condition => IO
              (Start_Port => 16#03f8#,
               Port_Range => 8) = Ref_1,
              Message   => "IO string mismatch (1)");
      Assert (Condition => IO
              (Start_Port => 16#50b0#,
               Port_Range => 8) = Ref_2,
              Message   => "IO string mismatch (2)");
--  begin read only
   end Test_IO;
--  end read only


--  begin read only
   procedure Test_IRQNoFlags (Gnattest_T : in out Test);
   procedure Test_IRQNoFlags_e0e5e6 (Gnattest_T : in out Test) renames Test_IRQNoFlags;
--  id:2.2/e0e5e6d17d473863/IRQNoFlags/1/0/
   procedure Test_IRQNoFlags (Gnattest_T : in out Test) is
   --  acpi-asl.ads:40:4:IRQNoFlags
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_1 : constant String := "IRQNoFlags () { 5 }";
      Ref_2 : constant String := "IRQNoFlags () { 254 }";
   begin
      Assert (Condition => IRQNoFlags (Number => 5) = Ref_1,
              Message   => "IRQNoFlags string mismatch (1)");
      Assert (Condition => IRQNoFlags (Number => 254) = Ref_2,
              Message   => "IRQNoFlags string mismatch (2)");
--  begin read only
   end Test_IRQNoFlags;
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
end Acpi.Asl.Test_Data.Tests;
