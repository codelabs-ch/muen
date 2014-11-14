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

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_1|vmxon']",
         Name  => "physicalAddress",
         Value => "16#a000#");

      begin
         VMXON_Consecutiveness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Memory region 'kernel_0|vmxon' not adjacent to other"
                    & " VMXON regions",
                    Message   => "Exception mismatch");
      end;
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

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='linux|vmcs']",
         Name  => "name",
         Value => "foobar");

      begin
         VMCS_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "VMCS region 'linux|vmcs' for subject linux not found",
                    Message   => "Exception mismatch");
      end;
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

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='sm|vmcs']",
         Name  => "size",
         Value => "16#0001_0013#");

      begin
         VMCS_Region_Size (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'size => 16#0001_0013#' of 'sm|vmcs' "
                    & "VMCS memory element not 4K",
                    Message   => "Exception mismatch");
      end;
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

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='time|vmcs']",
         Name  => "physicalAddress",
         Value => "16#0010_0000#");

      begin
         VMCS_In_Lowmem (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'physicalAddress => 16#0010_0000#' of "
                    & "'time|vmcs' VMCS memory element not below 1 MiB",
                    Message   => "Exception mismatch");
      end;
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

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='linux|vmcs']",
         Name  => "physicalAddress",
         Value => "16#a000#");

      begin
         VMCS_Consecutiveness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Memory region 'linux|vmcs' not adjacent to other"
                    & " VMCS regions",
                    Message   => "Exception mismatch");
      end;
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

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_0|vmxon']",
         Name  => "name",
         Value => "kernel_1|vmxon");

      begin
         Physical_Memory_Name_Uniqueness (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Multiple physical memory regions with name"
                    & " 'kernel_1|vmxon'",
                    Message   => "Exception mismatch");
      end;
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

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='linux|ram']",
         Name  => "name",
         Value => "foobar");

      begin
         Physical_Memory_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical memory 'linux|ram' referenced by logical "
                    & "memory 'linux|ram' not found",
                    Message   => "Exception mismatch");
      end;
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

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_text']",
         Name  => "physicalAddress",
         Value => "16#0010_0023#");

      begin
         Physical_Address_Alignment (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical address of memory region 'kernel_text' does "
                    & "not honor alignment 16#1000#",
                    Message   => "Exception mismatch");
      end;
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

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/memory/memory[@physical='vt|bin']",
         Name  => "virtualAddress",
         Value => "16#000e_0500#");

      begin
         Virtual_Address_Alignment (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'virtualAddress => 16#000e_0500#' of 'binary'"
                    & " logical memory element not page aligned",
                    Message   => "Exception mismatch");
      end;
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

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Region_Size (XML_Data => Data);

      --  Set invalid memory region size.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_text']",
         Name  => "size",
         Value => "16#0042#");

      begin
         Region_Size (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'size => 16#0042#' of 'kernel_text' physical "
                    & "memory element not multiple of page size (4K)",
                    Message   => "Exception mismatch");
      end;
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

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_0|vmxon']",
         Name  => "name",
         Value => "kernel_5|vmxon");

      begin
         Entity_Name_Encoding (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Entity 'kernel_5' encoded in memory region "
                    & "'kernel_5|vmxon' does not exist or is invalid",
                    Message   => "Exception mismatch");
      end;
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
      declare
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/memory/memory[@name='kernel_text']",
            Name  => "size",
            Value => "16#1000_0000#");

         begin
            Physical_Memory_Overlap (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Overlap of physical or device memory region "
                       & "'linux|ram' and 'kernel_text'",
                       Message   => "Exception mismatch");
         end;
      end;

      declare
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/memory/memory[@name='kernel_text']",
            Name  => "physicalAddress",
            Value => "16#000b_7000#");

         begin
            Physical_Memory_Overlap (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Overlap of physical or device memory region "
                       & "'kernel_text' and 'buffer'",
                       Message   => "Exception mismatch");
         end;
      end;
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

      ----------------------------------------------------------------------

      procedure Virtmem_Overlap_Kernel
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/kernel/memory/cpu[@id='0']/"
            & "memory[@physical='kernel_data']",
            Name  => "virtualAddress",
            Value => "16#0010_0000#");

         begin
            Virtual_Memory_Overlap (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Overlap of virtual memory region 'text' and 'data' "
                       & "of kernel running on CPU 0",
                       Message   => "Exception mismatch");
         end;
      end Virtmem_Overlap_Kernel;

      ----------------------------------------------------------------------

      procedure Virtmem_Overlap_Subject
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/memory/"
            & "memory[@physical='linux|bin']",
            Name  => "virtualAddress",
            Value => "16#0000#");

         begin
            Virtual_Memory_Overlap (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Overlap of virtual memory region 'binary' and "
                       & "'zero_page' of subject 'linux'",
                       Message   => "Exception mismatch");
         end;
      end Virtmem_Overlap_Subject;

      ----------------------------------------------------------------------

      procedure Virtmem_Overlap_Device_Kernel
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/kernel/memory/cpu[@id='0']/"
            & "memory[@logical='tau0_interface']",
            Name  => "virtualAddress",
            Value => "16#001f_c000#");

         begin
            Virtual_Memory_Overlap (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Overlap of virtual memory region 'tau0_interface' "
                       & "and 'mmio' of kernel running on CPU 0",
                       Message   => "Exception mismatch");
         end;
      end Virtmem_Overlap_Device_Kernel;

      ----------------------------------------------------------------------

      procedure Virtmem_Overlap_Device_Subject
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject/memory/"
            & "memory[@physical='vt|bin']",
            Name  => "virtualAddress",
            Value => "16#000b_7000#");

         begin
            Virtual_Memory_Overlap (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Overlap of virtual memory region 'binary' and "
                       & "'buffer' of subject 'vt'",
                       Message   => "Exception mismatch");
         end;
      end Virtmem_Overlap_Device_Subject;
   begin
      Virtmem_Overlap_Kernel;
      Virtmem_Overlap_Subject;
      Virtmem_Overlap_Device_Kernel;
      Virtmem_Overlap_Device_Subject;
--  begin read only
   end Test_Virtual_Memory_Overlap;
--  end read only


--  begin read only
   procedure Test_Kernel_Stack_Region_Presence (Gnattest_T : in out Test);
   procedure Test_Kernel_Stack_Region_Presence_e9e355 (Gnattest_T : in out Test) renames Test_Kernel_Stack_Region_Presence;
--  id:2.2/e9e355ee728b2e37/Kernel_Stack_Region_Presence/1/0/
   procedure Test_Kernel_Stack_Region_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:74:4:Kernel_Stack_Region_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_stack_0']",
         Name  => "name",
         Value => "foobar");

      begin
         Kernel_Stack_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Kernel stack region 'kernel_stack_0' for logical CPU 0"
                    & " not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Kernel_Stack_Region_Presence;
--  end read only


--  begin read only
   procedure Test_Kernel_Store_Region_Presence (Gnattest_T : in out Test);
   procedure Test_Kernel_Store_Region_Presence_3872a1 (Gnattest_T : in out Test) renames Test_Kernel_Store_Region_Presence;
--  id:2.2/3872a19a8e2482ec/Kernel_Store_Region_Presence/1/0/
   procedure Test_Kernel_Store_Region_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:77:4:Kernel_Store_Region_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_store_0']",
         Name  => "name",
         Value => "foobar");

      begin
         Kernel_Store_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Kernel store region 'kernel_store_0' for logical CPU 0"
                    & " not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Kernel_Store_Region_Presence;
--  end read only


--  begin read only
   procedure Test_Kernel_PT_Region_Presence (Gnattest_T : in out Test);
   procedure Test_Kernel_PT_Region_Presence_851d89 (Gnattest_T : in out Test) renames Test_Kernel_PT_Region_Presence;
--  id:2.2/851d896c926ea31a/Kernel_PT_Region_Presence/1/0/
   procedure Test_Kernel_PT_Region_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:80:4:Kernel_PT_Region_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_0|pt']",
         Name  => "name",
         Value => "foobar");

      begin
         Kernel_PT_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Kernel PT region 'kernel_0|pt' for logical CPU 0"
                    & " not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Kernel_PT_Region_Presence;
--  end read only


--  begin read only
   procedure Test_Kernel_PT_Below_4G (Gnattest_T : in out Test);
   procedure Test_Kernel_PT_Below_4G_976943 (Gnattest_T : in out Test) renames Test_Kernel_PT_Below_4G;
--  id:2.2/97694323e4a9cd62/Kernel_PT_Below_4G/1/0/
   procedure Test_Kernel_PT_Below_4G (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:83:4:Kernel_PT_Below_4G
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_0|pt']",
         Name  => "physicalAddress",
         Value => "16#0001_0000_0000#");

      begin
         Kernel_PT_Below_4G (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Kernel PT region 'kernel_0|pt' for logical CPU 0 not "
                    & "below 4G",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Kernel_PT_Below_4G;
--  end read only


--  begin read only
   procedure Test_Kernel_Memory_Mappings (Gnattest_T : in out Test);
   procedure Test_Kernel_Memory_Mappings_fe36fc (Gnattest_T : in out Test) renames Test_Kernel_Memory_Mappings;
--  id:2.2/fe36fc1c47e6055f/Kernel_Memory_Mappings/1/0/
   procedure Test_Kernel_Memory_Mappings (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:86:4:Kernel_Memory_Mappings
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='vt|bin']",
         Name  => "type",
         Value => "kernel");

      begin
         Kernel_Memory_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Kernel memory region 'vt|bin' mapped by logical memory "
                    & "region 'binary' of subject 'vt'",
                    Message   => "Exception mismatch");
      end;
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

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='vt|bin']",
         Name  => "type",
         Value => "system");

      begin
         System_Memory_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "System memory region 'vt|bin' is mapped by logical "
                    & "memory region 'binary'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_System_Memory_Mappings;
--  end read only


--  begin read only
   procedure Test_VTd_Root_Region_Size (Gnattest_T : in out Test);
   procedure Test_VTd_Root_Region_Size_bc3a31 (Gnattest_T : in out Test) renames Test_VTd_Root_Region_Size;
--  id:2.2/bc3a31ac2395433f/VTd_Root_Region_Size/1/0/
   procedure Test_VTd_Root_Region_Size (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:92:4:VTd_Root_Region_Size
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@type='system_vtd_root']",
         Name  => "size",
         Value => "16#0012#");

      begin
         VTd_Root_Region_Size (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'size => 16#0012#' of 'vtd_root' VT-d root "
                    & "table element not 4K",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_VTd_Root_Region_Size;
--  end read only


--  begin read only
   procedure Test_VTd_Context_Region_Size (Gnattest_T : in out Test);
   procedure Test_VTd_Context_Region_Size_4d6204 (Gnattest_T : in out Test) renames Test_VTd_Context_Region_Size;
--  id:2.2/4d620465079ba6ad/VTd_Context_Region_Size/1/0/
   procedure Test_VTd_Context_Region_Size (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:95:4:VTd_Context_Region_Size
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='vtd_context_3']",
         Name  => "size",
         Value => "16#002a#");

      begin
         VTd_Context_Region_Size (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'size => 16#002a#' of 'vtd_context_3' VT-d "
                    & "context table element not 4K",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_VTd_Context_Region_Size;
--  end read only


--  begin read only
   procedure Test_VTd_Root_Region_Presence (Gnattest_T : in out Test);
   procedure Test_VTd_Root_Region_Presence_b744c5 (Gnattest_T : in out Test) renames Test_VTd_Root_Region_Presence;
--  id:2.2/b744c5d7d5100d62/VTd_Root_Region_Presence/1/0/
   procedure Test_VTd_Root_Region_Presence (Gnattest_T : in out Test) is
   --  mucfgcheck-memory.ads:98:4:VTd_Root_Region_Presence
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@type='system_vtd_root']",
         Name  => "type",
         Value => "subject");

      begin
         VTd_Root_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "VT-d root table memory region not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_VTd_Root_Region_Presence;
--  end read only

end Mucfgcheck.Memory.Test_Data.Tests;
