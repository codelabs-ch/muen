--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Platform.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mucfgcheck.Platform.Test_Data.Tests is


--  begin read only
   procedure Test_Memory_Space (Gnattest_T : in out Test);
   procedure Test_Memory_Space_20d775 (Gnattest_T : in out Test) renames Test_Memory_Space;
--  id:2.2/20d775fbae27b871/Memory_Space/1/0/
   procedure Test_Memory_Space (Gnattest_T : in out Test) is
   --  mucfgcheck-platform.ads:25:4:Memory_Space
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/platform/memory/"
         & "memoryBlock[@name='extended_mem_1']",
         Name  => "size",
         Value => "16#1000#");

      begin
         Memory_Space (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Allocated 266039296 bytes of physical memory but only "
                    & "659456 bytes available by the platform",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Memory_Space;
--  end read only


--  begin read only
   procedure Test_Memory_Block_Overlap (Gnattest_T : in out Test);
   procedure Test_Memory_Block_Overlap_12597b (Gnattest_T : in out Test) renames Test_Memory_Block_Overlap;
--  id:2.2/12597bd947918ca6/Memory_Block_Overlap/1/0/
   procedure Test_Memory_Block_Overlap (Gnattest_T : in out Test) is
   --  mucfgcheck-platform.ads:28:4:Memory_Block_Overlap
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/platform/memory/memoryBlock[@name='base_mem']",
         Name  => "size",
         Value => "16#1000_0000#");

      begin
         Memory_Block_Overlap (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Overlap of platform memory block 'base_mem' and"
                    & " 'extended_mem_1'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Memory_Block_Overlap;
--  end read only


--  begin read only
   procedure Test_PCI_Config_Space_Address (Gnattest_T : in out Test);
   procedure Test_PCI_Config_Space_Address_4663d9 (Gnattest_T : in out Test) renames Test_PCI_Config_Space_Address;
--  id:2.2/4663d97b4d1f43a4/PCI_Config_Space_Address/1/0/
   procedure Test_PCI_Config_Space_Address (Gnattest_T : in out Test) is
   --  mucfgcheck-platform.ads:32:4:PCI_Config_Space_Address
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/platform/devices",
         Name  => "pciConfigAddress",
         Value => "");

      begin
         PCI_Config_Space_Address (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Missing PCI configuration space address",
                    Message   => "Exception mismatch");
      end;

--  begin read only
   end Test_PCI_Config_Space_Address;
--  end read only

end Mucfgcheck.Platform.Test_Data.Tests;
