--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Acpi.DSDT.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Acpi.DSDT.Test_Data.Tests is


--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_a7b092 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/a7b092122beb7bb7/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  acpi-dsdt.ads:28:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Subj : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/subjects/subject[@name='linux']");
      begin
         Write (Policy   => Policy,
                Subject  => Subj,
                Filename => "obj/linux_dsdt.dsl");

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/linux_dsdt.dsl.ref",
                  Filename2 => "obj/linux_dsdt.dsl"),
                 Message   => "DSDT table mismatch");
      end;
--  begin read only
   end Test_Write;
--  end read only

end Acpi.DSDT.Test_Data.Tests;
