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

      ----------------------------------------------------------------------

      procedure DSDT_Generation
      is
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
                    Message   => "DSDT table source mismatch");
            Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => "data/linux_dsdt.aml.ref",
                     Filename2 => "obj/linux_dsdt.aml"),
                    Message   => "DSDT table mismatch");
         end;

         Ada.Directories.Delete_File (Name => "obj/linux_dsdt.dsl");
         Ada.Directories.Delete_File (Name => "obj/linux_dsdt.aml");
      end DSDT_Generation;

      ----------------------------------------------------------------------

      procedure Single_PRT_Entry
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         declare
            Subj : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system/subjects/subject[@name='linux']");
            Dev  : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Subj,
               XPath => "devices/device[@physical='ethernet']");
         begin

            --  Remove second IRQ resource.

            Muxml.Utils.Remove_Child (Node       => Dev,
                                      Child_Name => "irq");

            Write (Policy   => Policy,
                   Subject  => Subj,
                   Filename => "obj/linux_dsdt.dsl");

            --  The iasl compilation step must not raise an exception.

         end;

         Ada.Directories.Delete_File (Name => "obj/linux_dsdt.dsl");
         Ada.Directories.Delete_File (Name => "obj/linux_dsdt.aml");
      end Single_PRT_Entry;

      ----------------------------------------------------------------------

      procedure Empty_PRT
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         declare
            Subj : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system/subjects/subject[@name='linux']");
            Dev  : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Subj,
               XPath => "devices/device[@physical='ethernet']");
         begin

            --  Remove all IRQ resources.

            Muxml.Utils.Remove_Child (Node       => Dev,
                                      Child_Name => "irq");
            Muxml.Utils.Remove_Child (Node       => Dev,
                                      Child_Name => "irq");

            Write (Policy   => Policy,
                   Subject  => Subj,
                   Filename => "obj/linux_dsdt.dsl");

            --  The iasl compilation step must not raise an exception.

         end;

         Ada.Directories.Delete_File (Name => "obj/linux_dsdt.dsl");
         Ada.Directories.Delete_File (Name => "obj/linux_dsdt.aml");
      end Empty_PRT;

      ----------------------------------------------------------------------

      procedure Single_Serial_Port
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         declare
            Subj : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system/subjects/subject[@name='linux']");
            Dev  : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Subj,
               XPath => "devices/device[@physical='serial_1']");
         begin

            --  Remove second I/O port resource.

            Muxml.Utils.Remove_Child (Node       => Dev,
                                      Child_Name => "ioPort");

            Write (Policy   => Policy,
                   Subject  => Subj,
                   Filename => "obj/linux_dsdt.dsl");


            Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => "data/linux_dsdt_one_port.dsl.ref",
                     Filename2 => "obj/linux_dsdt.dsl"),
                    Message   => "DSDT table source mismatch");
         end;

         Ada.Directories.Delete_File (Name => "obj/linux_dsdt.dsl");
         Ada.Directories.Delete_File (Name => "obj/linux_dsdt.aml");
      end Single_Serial_Port;
   begin
      DSDT_Generation;
      Single_PRT_Entry;
      Empty_PRT;
      Single_Serial_Port;
--  begin read only
   end Test_Write;
--  end read only

end Acpi.DSDT.Test_Data.Tests;
