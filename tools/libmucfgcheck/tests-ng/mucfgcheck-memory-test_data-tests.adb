--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Memory.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mucfgcheck.Memory.Test_Data.Tests is


--  begin read only
   procedure Test_VMXON_Region_Presence (Gnattest_T : in out Test);
   procedure Test_VMXON_Region_Presence_1b2bcc (Gnattest_T : in out Test) renames Test_VMXON_Region_Presence;
--  id:2.2/1b2bcc5675f4e46d/VMXON_Region_Presence/1/0/
   procedure Test_VMXON_Region_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:25:4:VMXON_Region_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_0|vmxon']",
         Name  => "name",
         Value => "foobar");

      begin
         VMXON_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "VMXON region 'kernel_0|vmxon' for logical CPU 0 not "
                    & "found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_VMXON_Region_Presence;
--  end read only


--  begin read only
   procedure Test_VMXON_Region_Size (Gnattest_T : in out Test);
   procedure Test_VMXON_Region_Size_265239 (Gnattest_T : in out Test) renames Test_VMXON_Region_Size;
--  id:2.2/2652390e3850ab98/VMXON_Region_Size/1/0/
   procedure Test_VMXON_Region_Size (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:28:4:VMXON_Region_Size
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_0|vmxon']",
         Name  => "size",
         Value => "16#0001_0012#");

      begin
         VMXON_Region_Size (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'size => 16#0001_0012#' of 'kernel_0|vmxon' "
                    & "VMXON memory element not 4K",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_VMXON_Region_Size;
--  end read only


--  begin read only
   procedure Test_VMXON_In_Lowmem (Gnattest_T : in out Test);
   procedure Test_VMXON_In_Lowmem_b08e99 (Gnattest_T : in out Test) renames Test_VMXON_In_Lowmem;
--  id:2.2/b08e99a886068c15/VMXON_In_Lowmem/1/0/
   procedure Test_VMXON_In_Lowmem (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:31:4:VMXON_In_Lowmem
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_1|vmxon']",
         Name  => "physicalAddress",
         Value => "16#0010_0000#");

      begin
         VMXON_In_Lowmem (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'physicalAddress => 16#0010_0000#' of "
                    & "'kernel_1|vmxon' VMXON memory element not below 1 MiB",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_VMXON_In_Lowmem;
--  end read only


--  begin read only
   procedure Test_VMXON_Consecutiveness (Gnattest_T : in out Test);
   procedure Test_VMXON_Consecutiveness_1b32f6 (Gnattest_T : in out Test) renames Test_VMXON_Consecutiveness;
--  id:2.2/1b32f6d793bebead/VMXON_Consecutiveness/1/0/
   procedure Test_VMXON_Consecutiveness (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:34:4:VMXON_Consecutiveness
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_VMXON_Consecutiveness;
--  end read only


--  begin read only
   procedure Test_VMCS_Region_Presence (Gnattest_T : in out Test);
   procedure Test_VMCS_Region_Presence_945465 (Gnattest_T : in out Test) renames Test_VMCS_Region_Presence;
--  id:2.2/94546573f481cca8/VMCS_Region_Presence/1/0/
   procedure Test_VMCS_Region_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:37:4:VMCS_Region_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_VMCS_Region_Presence;
--  end read only


--  begin read only
   procedure Test_VMCS_Region_Size (Gnattest_T : in out Test);
   procedure Test_VMCS_Region_Size_be694e (Gnattest_T : in out Test) renames Test_VMCS_Region_Size;
--  id:2.2/be694e541bcdcd01/VMCS_Region_Size/1/0/
   procedure Test_VMCS_Region_Size (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:40:4:VMCS_Region_Size
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_VMCS_Region_Size;
--  end read only


--  begin read only
   procedure Test_VMCS_In_Lowmem (Gnattest_T : in out Test);
   procedure Test_VMCS_In_Lowmem_fb4765 (Gnattest_T : in out Test) renames Test_VMCS_In_Lowmem;
--  id:2.2/fb4765ea1f97c688/VMCS_In_Lowmem/1/0/
   procedure Test_VMCS_In_Lowmem (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:43:4:VMCS_In_Lowmem
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_VMCS_In_Lowmem;
--  end read only


--  begin read only
   procedure Test_VMCS_Consecutiveness (Gnattest_T : in out Test);
   procedure Test_VMCS_Consecutiveness_8a8f58 (Gnattest_T : in out Test) renames Test_VMCS_Consecutiveness;
--  id:2.2/8a8f58d2d992eec8/VMCS_Consecutiveness/1/0/
   procedure Test_VMCS_Consecutiveness (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:46:4:VMCS_Consecutiveness
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_VMCS_Consecutiveness;
--  end read only


--  begin read only
   procedure Test_Physical_Memory_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Physical_Memory_Name_Uniqueness_460dc7 (Gnattest_T : in out Test) renames Test_Physical_Memory_Name_Uniqueness;
--  id:2.2/460dc72313ab5aa8/Physical_Memory_Name_Uniqueness/1/0/
   procedure Test_Physical_Memory_Name_Uniqueness (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:49:4:Physical_Memory_Name_Uniqueness
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Physical_Memory_Name_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Physical_Memory_References (Gnattest_T : in out Test);
   procedure Test_Physical_Memory_References_639788 (Gnattest_T : in out Test) renames Test_Physical_Memory_References;
--  id:2.2/6397882c731f89ee/Physical_Memory_References/1/0/
   procedure Test_Physical_Memory_References (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:52:4:Physical_Memory_References
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Physical_Memory_References;
--  end read only


--  begin read only
   procedure Test_Physical_Address_Alignment (Gnattest_T : in out Test);
   procedure Test_Physical_Address_Alignment_7431e2 (Gnattest_T : in out Test) renames Test_Physical_Address_Alignment;
--  id:2.2/7431e2b33dc8a6ac/Physical_Address_Alignment/1/0/
   procedure Test_Physical_Address_Alignment (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:55:4:Physical_Address_Alignment
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Physical_Address_Alignment;
--  end read only


--  begin read only
   procedure Test_Virtual_Address_Alignment (Gnattest_T : in out Test);
   procedure Test_Virtual_Address_Alignment_569258 (Gnattest_T : in out Test) renames Test_Virtual_Address_Alignment;
--  id:2.2/56925858ff6c81bd/Virtual_Address_Alignment/1/0/
   procedure Test_Virtual_Address_Alignment (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:58:4:Virtual_Address_Alignment
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Virtual_Address_Alignment;
--  end read only


--  begin read only
   procedure Test_Region_Size (Gnattest_T : in out Test);
   procedure Test_Region_Size_827af8 (Gnattest_T : in out Test) renames Test_Region_Size;
--  id:2.2/827af8f6ce3d362c/Region_Size/1/0/
   procedure Test_Region_Size (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:61:4:Region_Size
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Region_Size;
--  end read only


--  begin read only
   procedure Test_Entity_Name_Encoding (Gnattest_T : in out Test);
   procedure Test_Entity_Name_Encoding_01db9a (Gnattest_T : in out Test) renames Test_Entity_Name_Encoding;
--  id:2.2/01db9ac250ac37d0/Entity_Name_Encoding/1/0/
   procedure Test_Entity_Name_Encoding (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:65:4:Entity_Name_Encoding
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Entity_Name_Encoding;
--  end read only


--  begin read only
   procedure Test_Physical_Memory_Overlap (Gnattest_T : in out Test);
   procedure Test_Physical_Memory_Overlap_ac191e (Gnattest_T : in out Test) renames Test_Physical_Memory_Overlap;
--  id:2.2/ac191e848059014d/Physical_Memory_Overlap/1/0/
   procedure Test_Physical_Memory_Overlap (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:68:4:Physical_Memory_Overlap
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Physical_Memory_Overlap;
--  end read only


--  begin read only
   procedure Test_Virtual_Memory_Overlap (Gnattest_T : in out Test);
   procedure Test_Virtual_Memory_Overlap_7973e4 (Gnattest_T : in out Test) renames Test_Virtual_Memory_Overlap;
--  id:2.2/7973e4663e077f6d/Virtual_Memory_Overlap/1/0/
   procedure Test_Virtual_Memory_Overlap (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:71:4:Virtual_Memory_Overlap
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Virtual_Memory_Overlap;
--  end read only


--  begin read only
   procedure Test_Kernel_PT_Consecutiveness (Gnattest_T : in out Test);
   procedure Test_Kernel_PT_Consecutiveness_fcfd60 (Gnattest_T : in out Test) renames Test_Kernel_PT_Consecutiveness;
--  id:2.2/fcfd60df7a11c2d7/Kernel_PT_Consecutiveness/1/0/
   procedure Test_Kernel_PT_Consecutiveness (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:74:4:Kernel_PT_Consecutiveness
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Kernel_PT_Consecutiveness;
--  end read only


--  begin read only
   procedure Test_Kernel_Stack_Region_Presence (Gnattest_T : in out Test);
   procedure Test_Kernel_Stack_Region_Presence_e9e355 (Gnattest_T : in out Test) renames Test_Kernel_Stack_Region_Presence;
--  id:2.2/e9e355ee728b2e37/Kernel_Stack_Region_Presence/1/0/
   procedure Test_Kernel_Stack_Region_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:77:4:Kernel_Stack_Region_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Kernel_Stack_Region_Presence;
--  end read only


--  begin read only
   procedure Test_Kernel_Store_Region_Presence (Gnattest_T : in out Test);
   procedure Test_Kernel_Store_Region_Presence_3872a1 (Gnattest_T : in out Test) renames Test_Kernel_Store_Region_Presence;
--  id:2.2/3872a19a8e2482ec/Kernel_Store_Region_Presence/1/0/
   procedure Test_Kernel_Store_Region_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:80:4:Kernel_Store_Region_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Kernel_Store_Region_Presence;
--  end read only


--  begin read only
   procedure Test_Kernel_PT_Region_Presence (Gnattest_T : in out Test);
   procedure Test_Kernel_PT_Region_Presence_851d89 (Gnattest_T : in out Test) renames Test_Kernel_PT_Region_Presence;
--  id:2.2/851d896c926ea31a/Kernel_PT_Region_Presence/1/0/
   procedure Test_Kernel_PT_Region_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:83:4:Kernel_PT_Region_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Kernel_PT_Region_Presence;
--  end read only


--  begin read only
   procedure Test_Kernel_Memory_Mappings (Gnattest_T : in out Test);
   procedure Test_Kernel_Memory_Mappings_fe36fc (Gnattest_T : in out Test) renames Test_Kernel_Memory_Mappings;
--  id:2.2/fe36fc1c47e6055f/Kernel_Memory_Mappings/1/0/
   procedure Test_Kernel_Memory_Mappings (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:86:4:Kernel_Memory_Mappings
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Kernel_Memory_Mappings;
--  end read only


--  begin read only
   procedure Test_System_Memory_Mappings (Gnattest_T : in out Test);
   procedure Test_System_Memory_Mappings_6ca6be (Gnattest_T : in out Test) renames Test_System_Memory_Mappings;
--  id:2.2/6ca6befcb4661223/System_Memory_Mappings/1/0/
   procedure Test_System_Memory_Mappings (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:89:4:System_Memory_Mappings
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_System_Memory_Mappings;
--  end read only

end Mucfgcheck.Memory.Test_Data.Tests;
