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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Muxml;

with Validators.Device;

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
        (Routine => Validate_IRQ_Number_Eq'Access,
         Name    => "Validate IRQ number equality");
      T.Add_Test_Routine
        (Routine => Validate_IO_Port_Start_Smaller_End'Access,
         Name    => "Validate I/O ports start <= end");
      T.Add_Test_Routine
        (Routine => Validate_IO_Port_Refs'Access,
         Name    => "Validate I/O port references");
      T.Add_Test_Routine
        (Routine => Validate_IO_Port_Name_Uniqueness'Access,
         Name    => "Validate per-device I/O port name uniqueness");
      T.Add_Test_Routine
        (Routine => Validate_IO_Port_Range_Eq'Access,
         Name    => "Validate I/O port range equality");
      T.Add_Test_Routine
        (Routine => Validate_Devmem_Name_Uniqueness'Access,
         Name    => "Validate device memory name uniqueness");
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
                   File => "data/validators.xml");

      begin
         Validators.Device.Device_Sharing (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Non-shareable device 'serial' is referenced by multiple"
                    & " logical devices 'kernel->log', 'linux->console'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Device_Shareability;

   -------------------------------------------------------------------------

   procedure Validate_Devmem_Name_Uniqueness
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Device.Device_Memory_Name_Uniqueness (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Device 'vga' has multiple memory regions with name"
                    & " 'buffer'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Devmem_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Validate_IO_Port_Name_Uniqueness
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Device.Device_IO_Port_Name_Uniqueness (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Device 'vga' has multiple I/O ports with name 'port'",
                    Message   => "Exception mismatch");
      end;
   end Validate_IO_Port_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Validate_IO_Port_Range_Eq
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Device.IO_Port_Range_Equality (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "I/O port ranges of physical 'serial' and logical I/O "
                    & "port 'ports' of logical device 'log' differ",
                    Message   => "Exception mismatch");
      end;
   end Validate_IO_Port_Range_Eq;

   -------------------------------------------------------------------------

   procedure Validate_IO_Port_Refs
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      declare
         Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
           (List  => McKae.XML.XPath.XIA.XPath_Query
              (N     => Data.Doc,
               XPath => "/system/kernel/devices/device/ioPort"),
            Index => 0);
      begin

         --  Set invalid I/O port reference.

         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "physical",
            Value => "nonexistent");

         Validators.Device.IO_Port_References (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical I/O port 'nonexistent' referenced by logical"
                    & " I/O port 'ports' of logical device 'log' not found",
                    Message   => "Exception mismatch");
      end;
   end Validate_IO_Port_Refs;

   -------------------------------------------------------------------------

   procedure Validate_IO_Port_Start_Smaller_End
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      declare
         Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
           (List  => McKae.XML.XPath.XIA.XPath_Query
              (N     => Data.Doc,
               XPath => "/system/kernel/devices/device/ioPort"),
            Index => 0);
      begin

         --  Set invalid port range.

         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "start",
            Value => "16#ffff#");

         Validators.Device.IO_Port_Start_Smaller_End (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "I/O port 'ports' start 16#ffff# larger than "
                    & "end 16#50b8#",
                    Message   => "Exception mismatch");
      end;
   end Validate_IO_Port_Start_Smaller_End;

   -------------------------------------------------------------------------

   procedure Validate_IRQ_Number_Eq
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/validators.xml");

      begin
         Validators.Device.IRQ_Number_Equality (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical IRQ 'cmd' and logical IRQ 'irq' of logical "
                    & "device 'console' differ",
                    Message   => "Exception mismatch");
      end;
   end Validate_IRQ_Number_Eq;

   -------------------------------------------------------------------------

   procedure Validate_Physdev_Name_Uniqueness
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Device.Physical_Device_Name_Uniqueness (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
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
                   File => "data/validators.xml");

      declare
         Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
           (List  => McKae.XML.XPath.XIA.XPath_Query
              (N     => Data.Doc,
               XPath => "/system/kernel/devices/device"),
            Index => 0);
      begin

         --  Set invalid device reference.

         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "physical",
            Value => "nonexistent");

         Validators.Device.Physical_Device_References (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical device 'nonexistent' referenced by logical"
                    & " device 'log' not found",
                    Message   => "Exception mismatch");
      end;
   end Validate_Physdev_Refs;

   -------------------------------------------------------------------------

   procedure Validate_Physirq_Name_Uniqueness
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Device.Device_IRQ_Name_Uniqueness (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Device 'vga' has multiple IRQs with name 'bar'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Physirq_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Validate_Physirq_Refs
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      declare
         Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
           (List  => McKae.XML.XPath.XIA.XPath_Query
              (N     => Data.Doc,
               XPath => "/system/subjects/subject/devices/device/irq"
               & "[@physical='cmd']"),
            Index => 0);
      begin

         --  Set invalid IRQ number.

         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "physical",
            Value => "nonexistent");

         Validators.Device.Physical_IRQ_References (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
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
                   File => "data/validators.xml");

      begin
         Validators.Device.Physical_IRQ_Uniqueness (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Devices 'serial' and 'keyboard' share IRQ 1",
                    Message   => "Exception mismatch");
      end;
   end Validate_Physirq_Uniqueness;

end Device_Tests;
