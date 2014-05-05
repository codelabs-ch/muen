--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Device.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mucfgcheck.Device.Test_Data.Tests is


--  begin read only
   procedure Test_Physical_Device_References (Gnattest_T : in out Test);
   procedure Test_Physical_Device_References_b4cc94 (Gnattest_T : in out Test) renames Test_Physical_Device_References;
--  id:2.2/b4cc947cfd4d6ff0/Physical_Device_References/1/0/
   procedure Test_Physical_Device_References (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:25:4:Physical_Device_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/devices/device[@physical='ioapic']",
         Name  => "physical",
         Value => "nonexistent");

      begin
         Physical_Device_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical device 'nonexistent' referenced by logical"
                    & " device 'ioapic' not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Physical_Device_References;
--  end read only


--  begin read only
   procedure Test_Physical_Device_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Physical_Device_Name_Uniqueness_fa4110 (Gnattest_T : in out Test) renames Test_Physical_Device_Name_Uniqueness;
--  id:2.2/fa4110c6a29dd204/Physical_Device_Name_Uniqueness/1/0/
   procedure Test_Physical_Device_Name_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:28:4:Physical_Device_Name_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/platform/device[@name='serial']",
         Name  => "name",
         Value => "vga");

      begin
         Physical_Device_Name_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Multiple physical devices with name 'vga'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Physical_Device_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Physical_IRQ_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Physical_IRQ_Uniqueness_11c442 (Gnattest_T : in out Test) renames Test_Physical_IRQ_Uniqueness;
--  id:2.2/11c442b92552adf4/Physical_IRQ_Uniqueness/1/0/
   procedure Test_Physical_IRQ_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:31:4:Physical_IRQ_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Physical_IRQ_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Physical_IRQ_References (Gnattest_T : in out Test);
   procedure Test_Physical_IRQ_References_b54993 (Gnattest_T : in out Test) renames Test_Physical_IRQ_References;
--  id:2.2/b5499347878df1ba/Physical_IRQ_References/1/0/
   procedure Test_Physical_IRQ_References (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:34:4:Physical_IRQ_References
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Physical_IRQ_References;
--  end read only


--  begin read only
   procedure Test_Device_IRQ_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Device_IRQ_Name_Uniqueness_0150bf (Gnattest_T : in out Test) renames Test_Device_IRQ_Name_Uniqueness;
--  id:2.2/0150bf5273c9a2cb/Device_IRQ_Name_Uniqueness/1/0/
   procedure Test_Device_IRQ_Name_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:37:4:Device_IRQ_Name_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Device_IRQ_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_IO_Port_Start_Smaller_End (Gnattest_T : in out Test);
   procedure Test_IO_Port_Start_Smaller_End_c12eaa (Gnattest_T : in out Test) renames Test_IO_Port_Start_Smaller_End;
--  id:2.2/c12eaa9dd1b2f74e/IO_Port_Start_Smaller_End/1/0/
   procedure Test_IO_Port_Start_Smaller_End (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:40:4:IO_Port_Start_Smaller_End
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_IO_Port_Start_Smaller_End;
--  end read only


--  begin read only
   procedure Test_IO_Port_References (Gnattest_T : in out Test);
   procedure Test_IO_Port_References_5e0653 (Gnattest_T : in out Test) renames Test_IO_Port_References;
--  id:2.2/5e0653dce539594f/IO_Port_References/1/0/
   procedure Test_IO_Port_References (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:43:4:IO_Port_References
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_IO_Port_References;
--  end read only


--  begin read only
   procedure Test_Device_IO_Port_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Device_IO_Port_Name_Uniqueness_3e600f (Gnattest_T : in out Test) renames Test_Device_IO_Port_Name_Uniqueness;
--  id:2.2/3e600f38d0777032/Device_IO_Port_Name_Uniqueness/1/0/
   procedure Test_Device_IO_Port_Name_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:46:4:Device_IO_Port_Name_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Device_IO_Port_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Device_Memory_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Device_Memory_Name_Uniqueness_6a4d02 (Gnattest_T : in out Test) renames Test_Device_Memory_Name_Uniqueness;
--  id:2.2/6a4d025abc9b72fc/Device_Memory_Name_Uniqueness/1/0/
   procedure Test_Device_Memory_Name_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:49:4:Device_Memory_Name_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Device_Memory_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Device_Memory_References (Gnattest_T : in out Test);
   procedure Test_Device_Memory_References_6481e3 (Gnattest_T : in out Test) renames Test_Device_Memory_References;
--  id:2.2/6481e34bd4cbc943/Device_Memory_References/1/0/
   procedure Test_Device_Memory_References (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:52:4:Device_Memory_References
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Device_Memory_References;
--  end read only


--  begin read only
   procedure Test_Device_Sharing (Gnattest_T : in out Test);
   procedure Test_Device_Sharing_288f44 (Gnattest_T : in out Test) renames Test_Device_Sharing;
--  id:2.2/288f44a12a8ccac8/Device_Sharing/1/0/
   procedure Test_Device_Sharing (Gnattest_T : in out Test) is
   --  mucfgcheck-device.ads:55:4:Device_Sharing
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Device_Sharing;
--  end read only

end Mucfgcheck.Device.Test_Data.Tests;
