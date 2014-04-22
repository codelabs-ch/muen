--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Exceptions;

with DOM.Core.Elements;
with DOM.Core.Documents;

with Muxml.Utils;

with Mucfgcheck.Device;

package body Device_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Device validator tests");
      T.Add_Test_Routine
        (Routine => Validate_Physdev_Refs'Access,
         Name    => "Validate physical device references");
      T.Add_Test_Routine
        (Routine => Validate_Physdev_Name_Uniqueness'Access,
         Name    => "Validate physical device name uniqueness");
      T.Add_Test_Routine
        (Routine => Validate_Physirq_Uniqueness'Access,
         Name    => "Validate physical IRQ uniqueness");
      T.Add_Test_Routine
        (Routine => Validate_Physirq_Refs'Access,
         Name    => "Validate physical IRQ references");
      T.Add_Test_Routine
        (Routine => Validate_Physirq_Name_Uniqueness'Access,
         Name    => "Validate per-device IRQ name uniqueness");
      T.Add_Test_Routine
        (Routine => Validate_IO_Port_Start_Smaller_End'Access,
         Name    => "Validate I/O port start less or equal end");
      T.Add_Test_Routine
        (Routine => Validate_IO_Port_Refs'Access,
         Name    => "Validate I/O port references");
      T.Add_Test_Routine
        (Routine => Validate_IO_Port_Name_Uniqueness'Access,
         Name    => "Validate per-device I/O port name uniqueness");
      T.Add_Test_Routine
        (Routine => Validate_Devmem_Name_Uniqueness'Access,
         Name    => "Validate device memory name uniqueness");
      T.Add_Test_Routine
        (Routine => Validate_Devmem_Refs'Access,
         Name    => "Validate device memory references");
      T.Add_Test_Routine
        (Routine => Validate_Device_Shareability'Access,
         Name    => "Validate device shareability");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Validate_Device_Shareability
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/devices/"
            & "device[@physical='port80']");
      begin
         DOM.Core.Elements.Set_Attribute (Elem  => Node,
                                          Name  => "physical",
                                          Value => "cmos_rtc");

         Mucfgcheck.Device.Device_Sharing (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Non-shareable device 'cmos_rtc' is referenced by "
                    & "multiple logical devices 'time->port80', "
                    & "'linux->cmos_rtc'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Device_Shareability;

   -------------------------------------------------------------------------

   procedure Validate_Devmem_Name_Uniqueness
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Dev  : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/platform/device[@name='vga']");
         Node : constant DOM.Core.Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "memory");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "name",
            Value => "buffer");
         Muxml.Utils.Append_Child
           (Node      => Dev,
            New_Child => Node);

         Mucfgcheck.Device.Device_Memory_Name_Uniqueness (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Device 'vga' has multiple memory regions with name"
                    & " 'buffer'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Devmem_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Validate_Devmem_Refs
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/kernel/devices/device/memory");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "physical",
            Value => "nonexistent");

         Mucfgcheck.Device.Device_Memory_References (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical device memory 'nonexistent' referenced by"
                    & " logical device memory 'mmio' of logical device "
                    & "'ioapic' not found",
                    Message   => "Exception mismatch");
      end;
   end Validate_Devmem_Refs;

   -------------------------------------------------------------------------

   procedure Validate_IO_Port_Name_Uniqueness
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/platform/device/ioPort[@name='port_64']");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "name",
            Value => "port_60");

         Mucfgcheck.Device.Device_IO_Port_Name_Uniqueness (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Device 'keyboard' has multiple I/O ports with name "
                    & "'port_60'",
                    Message   => "Exception mismatch");
      end;
   end Validate_IO_Port_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Validate_IO_Port_Refs
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/devices/device[@logical='vga']"
            & "/ioPort");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "physical",
            Value => "nonexistent");

         Mucfgcheck.Device.IO_Port_References (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical I/O port 'nonexistent' referenced by logical"
                    & " I/O port 'ports' of logical device 'vga' not found",
                    Message   => "Exception mismatch");
      end;
   end Validate_IO_Port_Refs;

   -------------------------------------------------------------------------

   procedure Validate_IO_Port_Start_Smaller_End
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/platform/device/ioPort[@name='ports']");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "start",
            Value => "16#ffff#");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "end",
            Value => "16#50b8#");

         Mucfgcheck.Device.IO_Port_Start_Smaller_End (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "I/O port 'ports' start 16#ffff# larger than "
                    & "end 16#50b8#",
                    Message   => "Exception mismatch");
      end;
   end Validate_IO_Port_Start_Smaller_End;

   -------------------------------------------------------------------------

   procedure Validate_Physdev_Name_Uniqueness
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/platform/device[@name='serial']");
      begin
         DOM.Core.Elements.Set_Attribute (Elem  => Node,
                                          Name  => "name",
                                          Value => "vga");

         Mucfgcheck.Device.Physical_Device_Name_Uniqueness (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Multiple physical devices with name 'vga'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Physdev_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Validate_Physdev_Refs
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/kernel/devices/device[@physical='ioapic']");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "physical",
            Value => "nonexistent");

         Mucfgcheck.Device.Physical_Device_References (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical device 'nonexistent' referenced by logical"
                    & " device 'ioapic' not found",
                    Message   => "Exception mismatch");
      end;
   end Validate_Physdev_Refs;

   -------------------------------------------------------------------------

   procedure Validate_Physirq_Name_Uniqueness
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Kbd  : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/platform/device[@name='keyboard']");
         Node : constant DOM.Core.Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "irq");
      begin
         DOM.Core.Elements.Set_Attribute (Elem  => Node,
                                          Name  => "name",
                                          Value => "kbd_irq");
         Muxml.Utils.Append_Child (Node      => Kbd,
                                   New_Child => Node);

         Mucfgcheck.Device.Device_IRQ_Name_Uniqueness (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Device 'keyboard' has multiple IRQs with name "
                    & "'kbd_irq'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Physirq_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Validate_Physirq_Refs
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");

      declare
         Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/devices/device/irq"
            & "[@physical='cmd']");
      begin

         --  Set invalid IRQ number.

         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "physical",
            Value => "nonexistent");

         Mucfgcheck.Device.Physical_IRQ_References (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical IRQ 'nonexistent' referenced by logical IRQ"
                    & " 'irq' of logical device 'console' not found",
                    Message   => "Exception mismatch");
      end;
   end Validate_Physirq_Refs;

   -------------------------------------------------------------------------

   procedure Validate_Physirq_Uniqueness
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");

      begin
         Mucfgcheck.Device.Physical_IRQ_Uniqueness (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Devices 'serial' and 'keyboard' share IRQ 1",
                    Message   => "Exception mismatch");
      end;
   end Validate_Physirq_Uniqueness;

end Device_Tests;
