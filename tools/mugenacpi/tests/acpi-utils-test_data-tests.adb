--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Acpi.Utils.Test_Data.

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
package body Acpi.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Indent (Gnattest_T : in out Test);
   procedure Test_Indent_399a7a (Gnattest_T : in out Test) renames Test_Indent;
--  id:2.2/399a7ad24f629c80/Indent/1/0/
   procedure Test_Indent (Gnattest_T : in out Test) is
   --  acpi-utils.ads:28:4:Indent
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Already tested in Libmutools");
--  begin read only
   end Test_Indent;
--  end read only


--  begin read only
   procedure Test_Add_Dev_IRQ_Resource (Gnattest_T : in out Test);
   procedure Test_Add_Dev_IRQ_Resource_386422 (Gnattest_T : in out Test) renames Test_Add_Dev_IRQ_Resource;
--  id:2.2/386422a502039cf0/Add_Dev_IRQ_Resource/1/0/
   procedure Test_Add_Dev_IRQ_Resource (Gnattest_T : in out Test) is
   --  acpi-utils.ads:35:4:Add_Dev_IRQ_Resource
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_IRQ_Resource : constant String := Indent (N => 5)
        & "Package (4) { 0x010cffff, 2, Zero, 0x5 }," & ASCII.LF;

      Buf : Unbounded_String;
   begin
      Add_Dev_IRQ_Resource (Buffer  => Buf,
                            Bus_Nr  => 1,
                            Dev_Nr  => 12,
                            Irq_Nr  => 5,
                            Int_Pin => 2);
      Assert (Condition => To_String (Buf) = Ref_IRQ_Resource,
              Message   => "Reference IRQ resource mismatch");
--  begin read only
   end Test_Add_Dev_IRQ_Resource;
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
end Acpi.Utils.Test_Data.Tests;
