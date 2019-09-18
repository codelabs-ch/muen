--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Memory.Test_Data.

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
package body Mucfgcheck.Memory.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_VMXON_Region_Presence (Gnattest_T : in out Test);
   procedure Test_VMXON_Region_Presence_1b2bcc (Gnattest_T : in out Test) renames Test_VMXON_Region_Presence;
--  id:2.2/1b2bcc5675f4e46d/VMXON_Region_Presence/1/0/
   procedure Test_VMXON_Region_Presence (Gnattest_T : in out Test) is
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
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      VMCS_Region_Presence (XML_Data => Data);

      --  Missing VMCS region.

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
                    = "VMCS region 'linux|vmcs' for subject 'linux' not found",
                    Message   => "Exception mismatch");
      end;

      --  VMCS region with incorrect region type.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='vt|vmcs']",
         Name  => "type",
         Value => "system");

      begin
         VMCS_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "VMCS region 'vt|vmcs' for subject 'vt' not found",
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
   procedure Test_Physical_Memory_Name_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Physical_Memory_Name_Uniqueness_460dc7 (Gnattest_T : in out Test) renames Test_Physical_Memory_Name_Uniqueness;
--  id:2.2/460dc72313ab5aa8/Physical_Memory_Name_Uniqueness/1/0/
   procedure Test_Physical_Memory_Name_Uniqueness (Gnattest_T : in out Test) is
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
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Physical_Memory_References (XML_Data => Data);

      --  Invalid kernel memory reference.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_text']",
         Name  => "name",
         Value => "foobar");

      begin
         Physical_Memory_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical memory 'kernel_text' referenced by logical "
                    & "memory 'text' not found",
                    Message   => "Exception mismatch");
      end;

      --  Invalid subject memory reference.

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
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Entity_Name_Encoding (XML_Data => Data);

      begin
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/memory/memory[@name='kernel_0|vmxon']",
            Name  => "name",
            Value => "kernel_5|vmxon");
         Entity_Name_Encoding (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Entity 'kernel_5' encoded in memory region "
                    & "'kernel_5|vmxon' does not exist or is invalid",
                    Message   => "Exception mismatch (1)");
      end;

      Multidigit_Kernel_Entity :
      begin
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/memory/memory[@name='kernel_5|vmxon']",
            Name  => "name",
            Value => "kernel_31|vmxon");

         Entity_Name_Encoding (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Entity 'kernel_31' encoded in memory region "
                    & "'kernel_31|vmxon' does not exist or is invalid",
                    Message   => "Exception mismatch (2)");
      end Multidigit_Kernel_Entity;

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/hardware/processor",
         Name  => "cpuCores",
         Value => "32");

      --  Valid kernel CPU number encoding, must not raise an exception.

      Entity_Name_Encoding (XML_Data => Data);
--  begin read only
   end Test_Entity_Name_Encoding;
--  end read only


--  begin read only
   procedure Test_Physical_Memory_Overlap (Gnattest_T : in out Test);
   procedure Test_Physical_Memory_Overlap_ac191e (Gnattest_T : in out Test) renames Test_Physical_Memory_Overlap;
--  id:2.2/ac191e848059014d/Physical_Memory_Overlap/1/0/
   procedure Test_Physical_Memory_Overlap (Gnattest_T : in out Test) is
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
   procedure Test_Uncached_Crash_Audit_Presence (Gnattest_T : in out Test);
   procedure Test_Uncached_Crash_Audit_Presence_2147d5 (Gnattest_T : in out Test) renames Test_Uncached_Crash_Audit_Presence;
--  id:2.2/2147d55d21bf3812/Uncached_Crash_Audit_Presence/1/0/
   procedure Test_Uncached_Crash_Audit_Presence (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Uncached_Crash_Audit_Presence (XML_Data => Data);

      Node := Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@type='subject_crash_audit']");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "caching",
         Value => "WC");

      begin
         Uncached_Crash_Audit_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Crash audit region caching is WC instead of UC",
                    Message   => "Exception mismatch (1)");
      end;

      Node := DOM.Core.Nodes.Remove_Child
        (N         => Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/memory"),
         Old_Child => Node);
      begin
         Uncached_Crash_Audit_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "One crash audit region expected, found 0",
                    Message   => "Exception mismatch (2)");
      end;
--  begin read only
   end Test_Uncached_Crash_Audit_Presence;
--  end read only


--  begin read only
   procedure Test_Crash_Audit_After_Image (Gnattest_T : in out Test);
   procedure Test_Crash_Audit_After_Image_049f01 (Gnattest_T : in out Test) renames Test_Crash_Audit_After_Image;
--  id:2.2/049f01b4611c4049/Crash_Audit_After_Image/1/0/
   procedure Test_Crash_Audit_After_Image (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Crash_Audit_After_Image (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@type='subject_crash_audit']",
         Name  => "physicalAddress",
         Value => "16#2000#");

      begin
         Crash_Audit_After_Image (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Crash audit region @16#2000# within system image with "
                    & "end address 16#cafe_6000#",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Crash_Audit_After_Image;
--  end read only


--  begin read only
   procedure Test_Kernel_Data_Region_Presence (Gnattest_T : in out Test);
   procedure Test_Kernel_Data_Region_Presence_18a431 (Gnattest_T : in out Test) renames Test_Kernel_Data_Region_Presence;
--  id:2.2/18a4312991eaca69/Kernel_Data_Region_Presence/1/0/
   procedure Test_Kernel_Data_Region_Presence (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Kernel_Data_Region_Presence (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_data_0']",
         Name  => "name",
         Value => "foobar");

      begin
         Kernel_Data_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Kernel data region 'kernel_data_0' for logical CPU 0"
                    & " not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Kernel_Data_Region_Presence;
--  end read only


--  begin read only
   procedure Test_Kernel_BSS_Region_Presence (Gnattest_T : in out Test);
   procedure Test_Kernel_BSS_Region_Presence_c455ac (Gnattest_T : in out Test) renames Test_Kernel_BSS_Region_Presence;
--  id:2.2/c455ac827bb67d9f/Kernel_BSS_Region_Presence/1/0/
   procedure Test_Kernel_BSS_Region_Presence (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Kernel_BSS_Region_Presence (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_bss_0']",
         Name  => "name",
         Value => "foobar");

      begin
         Kernel_BSS_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Kernel BSS region 'kernel_bss_0' for logical CPU 0"
                    & " not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Kernel_BSS_Region_Presence;
--  end read only


--  begin read only
   procedure Test_Kernel_Stack_Region_Presence (Gnattest_T : in out Test);
   procedure Test_Kernel_Stack_Region_Presence_e9e355 (Gnattest_T : in out Test) renames Test_Kernel_Stack_Region_Presence;
--  id:2.2/e9e355ee728b2e37/Kernel_Stack_Region_Presence/1/0/
   procedure Test_Kernel_Stack_Region_Presence (Gnattest_T : in out Test) is
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
   procedure Test_Kernel_Intr_Stack_Region_Presence (Gnattest_T : in out Test);
   procedure Test_Kernel_Intr_Stack_Region_Presence_36ff28 (Gnattest_T : in out Test) renames Test_Kernel_Intr_Stack_Region_Presence;
--  id:2.2/36ff28b32231d59f/Kernel_Intr_Stack_Region_Presence/1/0/
   procedure Test_Kernel_Intr_Stack_Region_Presence (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Kernel_Intr_Stack_Region_Presence (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_interrupt_stack_0']",
         Name  => "name",
         Value => "foobar");

      begin
         Kernel_Intr_Stack_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Kernel interrupt stack region "
                    & "'kernel_interrupt_stack_0' for logical CPU 0 not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Kernel_Intr_Stack_Region_Presence;
--  end read only


--  begin read only
   procedure Test_Kernel_PT_Region_Presence (Gnattest_T : in out Test);
   procedure Test_Kernel_PT_Region_Presence_851d89 (Gnattest_T : in out Test) renames Test_Kernel_PT_Region_Presence;
--  id:2.2/851d896c926ea31a/Kernel_PT_Region_Presence/1/0/
   procedure Test_Kernel_PT_Region_Presence (Gnattest_T : in out Test) is
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
                    = "Kernel pagetable region 'kernel_0|pt' for logical CPU 0"
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
   procedure Test_Kernel_Sched_Group_Info_Mappings (Gnattest_T : in out Test);
   procedure Test_Kernel_Sched_Group_Info_Mappings_6f1b3f (Gnattest_T : in out Test) renames Test_Kernel_Sched_Group_Info_Mappings;
--  id:2.2/6f1b3fbaf1fe3483/Kernel_Sched_Group_Info_Mappings/1/0/
   procedure Test_Kernel_Sched_Group_Info_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Kernel_Sched_Group_Info_Mappings (XML_Data => Data);

      --  Kernel scheduling group info mapping with wrong virtual base
      --  address.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory"
         & "[@physical='scheduling_group_info_4']",
         Name  => "virtualAddress",
         Value => "16#beef_0000#");
      begin
         Kernel_Sched_Group_Info_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Kernel mapping for info region of scheduling group 4 at"
                    & " unexpected kernel virtual address 16#beef_0000#, "
                    & "should be 16#00a0_3000#",
                    Message   => "Exception mismatch (1)");
      end;

      --  Kernel and scheduling group of different CPU.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='tau0']",
         Name  => "cpu",
         Value => "0");
      begin
         Kernel_Sched_Group_Info_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Info region of scheduling group 4 mapped by kernel "
                    & "running on CPU 3, should be CPU 0",
                    Message   => "Exception mismatch (2)");
      end;

      --  Multiple kernel scheduling group info mappings.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@physical='vt|state']",
         Name  => "physical",
         Value => "scheduling_group_info_4");
      begin
         Kernel_Sched_Group_Info_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (3)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Info region of scheduling group 4 has multiple "
                    & "kernel mappings: 2",
                    Message   => "Exception mismatch (3)");
      end;

      --  Missing kernel scheduling group info mapping.

      declare
         Node : DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/kernel/memory/cpu[@id='0']/memory"
            & "[@physical='scheduling_group_info_1']");
      begin
         Node := DOM.Core.Nodes.Remove_Child
           (N         => DOM.Core.Nodes.Parent_Node (N => Node),
            Old_Child => Node);

         Kernel_Sched_Group_Info_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (4)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "No kernel mapping for info region of scheduling group"
                    & " 1",
                    Message   => "Exception mismatch (4)");
      end;
--  begin read only
   end Test_Kernel_Sched_Group_Info_Mappings;
--  end read only


--  begin read only
   procedure Test_Subject_State_Region_Presence (Gnattest_T : in out Test);
   procedure Test_Subject_State_Region_Presence_33b778 (Gnattest_T : in out Test) renames Test_Subject_State_Region_Presence;
--  id:2.2/33b77883901d3c36/Subject_State_Region_Presence/1/0/
   procedure Test_Subject_State_Region_Presence (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Subject_State_Region_Presence (XML_Data => Data);

      --  Missing subject state region.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='vt|state']",
         Name  => "name",
         Value => "foobar");

      begin
         Subject_State_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject state region 'vt|state' for subject 'vt' not"
                    & " found",
                    Message   => "Exception mismatch");
      end;

      --  Subject state region with incorrect region type.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='tau0|state']",
         Name  => "type",
         Value => "subject");

      begin
         Subject_State_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject state region 'tau0|state' for subject 'tau0'"
                    & " not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Subject_State_Region_Presence;
--  end read only


--  begin read only
   procedure Test_Subject_Interrupts_Region_Presence (Gnattest_T : in out Test);
   procedure Test_Subject_Interrupts_Region_Presence_912d8d (Gnattest_T : in out Test) renames Test_Subject_Interrupts_Region_Presence;
--  id:2.2/912d8ddf3faa9ed6/Subject_Interrupts_Region_Presence/1/0/
   procedure Test_Subject_Interrupts_Region_Presence (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Subject_Interrupts_Region_Presence (XML_Data => Data);

      --  Missing subject interrupts region.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='linux|interrupts']",
         Name  => "name",
         Value => "foobar");

      begin
         Subject_Interrupts_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject interrupts region 'linux|interrupts' for "
                    & "subject 'linux' not found",
                    Message   => "Exception mismatch (1)");
      end;

      --  Subject interrupts region with incorrect region type.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='tau0|interrupts']",
         Name  => "type",
         Value => "subject");

      begin
         Subject_Interrupts_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject interrupts region 'tau0|interrupts' for subject"
                    & " 'tau0' not found",
                    Message   => "Exception mismatch (2)");
      end;
--  begin read only
   end Test_Subject_Interrupts_Region_Presence;
--  end read only


--  begin read only
   procedure Test_Kernel_Memory_Mappings (Gnattest_T : in out Test);
   procedure Test_Kernel_Memory_Mappings_fe36fc (Gnattest_T : in out Test) renames Test_Kernel_Memory_Mappings;
--  id:2.2/fe36fc1c47e6055f/Kernel_Memory_Mappings/1/0/
   procedure Test_Kernel_Memory_Mappings (Gnattest_T : in out Test) is
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
   procedure Test_Device_Memory_Mappings (Gnattest_T : in out Test);
   procedure Test_Device_Memory_Mappings_11f9fd (Gnattest_T : in out Test) renames Test_Device_Memory_Mappings;
--  id:2.2/11f9fd8fa420d8d1/Device_Memory_Mappings/1/0/
   procedure Test_Device_Memory_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must no raise an exception.

      Device_Memory_Mappings (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='vt|bin']",
         Name  => "type",
         Value => "device_rmrr");

      begin
         Device_Memory_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Device memory region 'vt|bin' is mapped by logical "
                    & "memory region 'binary' (Owner name: 'subjects')",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Device_Memory_Mappings;
--  end read only


--  begin read only
   procedure Test_Subject_State_Mappings (Gnattest_T : in out Test);
   procedure Test_Subject_State_Mappings_5f6e13 (Gnattest_T : in out Test) renames Test_Subject_State_Mappings;
--  id:2.2/5f6e1350f2cd2bf6/Subject_State_Mappings/1/0/
   procedure Test_Subject_State_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Subject_State_Mappings (XML_Data => Data);

      --  Kernel subject state mappings with differnt virtual base addresses.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@logical='linux|state']",
         Name  => "virtualAddress",
         Value => "16#cafe_0000#");
      begin
         Subject_State_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject state memory region 'linux|state' mapped at "
                    & "unexpected kernel virtual address 16#cafe_0000#, should"
                    & " be 16#001e_4000#",
                    Message   => "Exception mismatch");
      end;

      --  Kernel and subject with different CPU.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']",
         Name  => "cpu",
         Value => "0");
      begin
         Subject_State_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject state memory region 'linux|state' mapped by "
                    & "kernel and subject 'linux' with different CPU ID: "
                    & "1 /= 0",
                    Message   => "Exception mismatch");
      end;

      --  Multiple kernel subject state mappings.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@physical="
         & "'sm|timed_event']",
         Name  => "physical",
         Value => "vt|state");
      begin
         Subject_State_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject state memory region 'vt|state' has multiple "
                    & "kernel mappings: 2",
                    Message   => "Exception mismatch");
      end;

      --  No kernel subject state mapping.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@logical="
         & "'sm|timed_event']",
         Name  => "physical",
         Value => "nonexistent");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@logical='vt|state']",
         Name  => "physical",
         Value => "nonexistent");
      begin
         Subject_State_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject state memory region 'vt|state' is not mapped by"
                    & " any kernel",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Subject_State_Mappings;
--  end read only


--  begin read only
   procedure Test_Subject_Interrupts_Mappings (Gnattest_T : in out Test);
   procedure Test_Subject_Interrupts_Mappings_a36835 (Gnattest_T : in out Test) renames Test_Subject_Interrupts_Mappings;
--  id:2.2/a36835dba6b45279/Subject_Interrupts_Mappings/1/0/
   procedure Test_Subject_Interrupts_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Subject_Interrupts_Mappings (XML_Data => Data);

      --  Kernel subject interrupts mapping with wrong virtual base addresses.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu[@id='1']/memory"
         & "[@logical='sm|interrupts']",
         Name  => "virtualAddress",
         Value => "16#beef_0000#");
      begin
         Subject_Interrupts_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject interrupts memory region 'sm|interrupts' mapped"
                    & " at unexpected kernel virtual address 16#beef_0000#,"
                    & " should be 16#0060_3000#",
                    Message   => "Exception mismatch (1)");
      end;


      --  Kernel and subject with different CPU.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='sm']",
         Name  => "cpu",
         Value => "0");
      begin
         Subject_Interrupts_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject interrupts memory region 'sm|interrupts' "
                    & "mapped by kernel and subject 'sm' with different "
                    & "CPU ID: 1 /= 0",
                    Message   => "Exception mismatch (2)");
      end;

      --  Multiple kernel subject interrupt mappings.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@physical='vt|state']",
         Name  => "physical",
         Value => "sm|interrupts");
      begin
         Subject_Interrupts_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (3)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject interrupts memory region 'sm|interrupts' has "
                    & "multiple kernel mappings: 2",
                    Message   => "Exception mismatch (3)");
      end;

      --  Missing kernel subject interrupts mapping.

      declare
         Node : DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/kernel/memory/cpu[@id='0']/memory"
            & "[@logical='vt|interrupts']");
      begin
         Node := DOM.Core.Nodes.Remove_Child
           (N         => DOM.Core.Nodes.Parent_Node (N => Node),
            Old_Child => Node);

         Subject_Interrupts_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (4)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject interrupts memory region 'vt|interrupts' is "
                    & "not mapped by any kernel",
                    Message   => "Exception mismatch (4)");
      end;
--  begin read only
   end Test_Subject_Interrupts_Mappings;
--  end read only


--  begin read only
   procedure Test_Subject_MSR_Store_Mappings (Gnattest_T : in out Test);
   procedure Test_Subject_MSR_Store_Mappings_30a561 (Gnattest_T : in out Test) renames Test_Subject_MSR_Store_Mappings;
--  id:2.2/30a561743f5de4a0/Subject_MSR_Store_Mappings/1/0/
   procedure Test_Subject_MSR_Store_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Subject_MSR_Store_Mappings (XML_Data => Data);

      --  Kernel subject MSR store mapping with wrong virtual base addresses.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu[@id='1']/memory"
         & "[@logical='linux|msrstore']",
         Name  => "virtualAddress",
         Value => "16#beef_0000#");
      begin
         Subject_MSR_Store_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject MSR store memory region 'linux|msrstore' mapped"
                    & " at unexpected kernel virtual address 16#beef_0000#,"
                    & " should be 16#0080_4000#",
                    Message   => "Exception mismatch (1)");
      end;


      --  Kernel and subject with different CPU.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']",
         Name  => "cpu",
         Value => "0");
      begin
         Subject_MSR_Store_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject MSR store memory region 'linux|msrstore' "
                    & "mapped by kernel and subject 'linux' with different "
                    & "CPU ID: 1 /= 0",
                    Message   => "Exception mismatch (2)");
      end;

      --  Multiple kernel subject MSR store mappings.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@physical='vt|state']",
         Name  => "physical",
         Value => "linux|msrstore");
      begin
         Subject_MSR_Store_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (3)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject MSR store memory region 'linux|msrstore' has "
                    & "multiple kernel mappings: 2",
                    Message   => "Exception mismatch (3)");
      end;

      --  Missing kernel subject MSR store mapping.

      declare
         Node : DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/kernel/memory/cpu/memory"
            & "[@logical='time|msrstore']");
      begin
         Node := DOM.Core.Nodes.Remove_Child
           (N         => DOM.Core.Nodes.Parent_Node (N => Node),
            Old_Child => Node);

         Subject_MSR_Store_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (4)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject MSR store memory region 'time|msrstore' is "
                    & "not mapped by any kernel",
                    Message   => "Exception mismatch (4)");
      end;
--  begin read only
   end Test_Subject_MSR_Store_Mappings;
--  end read only


--  begin read only
   procedure Test_Subject_Timed_Event_Mappings (Gnattest_T : in out Test);
   procedure Test_Subject_Timed_Event_Mappings_fa82de (Gnattest_T : in out Test) renames Test_Subject_Timed_Event_Mappings;
--  id:2.2/fa82dee7e4ecaf6f/Subject_Timed_Event_Mappings/1/0/
   procedure Test_Subject_Timed_Event_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Subject_Timed_Event_Mappings (XML_Data => Data);

      --  Kernel timed event mappings with differnt virtual base addresses.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@logical='"
         & "linux|timed_event']",
         Name  => "virtualAddress",
         Value => "16#ffff_f000#");
      begin
         Subject_Timed_Event_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Timed event memory region 'linux|timed_event' mapped at"
                    & " unexpected kernel virtual address 16#ffff_f000#,"
                    & " should be 16#0040_4000#",
                    Message   => "Exception mismatch (1)");
      end;

      --  Kernel and subject with different CPU.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']",
         Name  => "cpu",
         Value => "0");
      begin
         Subject_Timed_Event_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Timed event memory region 'linux|timed_event' mapped by"
                    & " kernel and subject 'linux' with different CPU ID:"
                    & " 1 /= 0",
                    Message   => "Exception mismatch (2)");
      end;

      --  Multiple kernel timed event mappings.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@physical='vt|state']",
         Name  => "physical",
         Value => "sm|timed_event");
      begin
         Subject_Timed_Event_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (3)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Timed event memory region 'sm|timed_event' has multiple"
                    & " kernel mappings: 2",
                    Message   => "Exception mismatch (3)");
      end;

      --  No kernel timed event mapping.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@logical="
         & "'sm|timed_event']",
         Name  => "physical",
         Value => "nonexistent");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@logical='vt|state']",
         Name  => "physical",
         Value => "nonexistent");
      begin
         Subject_Timed_Event_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (4)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Timed event memory region 'sm|timed_event' is not"
                    & " mapped by any kernel",
                    Message   => "Exception mismatch (4)");
      end;
--  begin read only
   end Test_Subject_Timed_Event_Mappings;
--  end read only


--  begin read only
   procedure Test_Subject_VMCS_Mappings (Gnattest_T : in out Test);
   procedure Test_Subject_VMCS_Mappings_6436de (Gnattest_T : in out Test) renames Test_Subject_VMCS_Mappings;
--  id:2.2/6436de186de4308e/Subject_VMCS_Mappings/1/0/
   procedure Test_Subject_VMCS_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Subject_VMCS_Mappings (XML_Data => Data);

      --  Kernel subject VMCS mappings with different virtual base addresses.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@logical='linux|vmcs']",
         Name  => "virtualAddress",
         Value => "16#cafe_0000#");
      begin
         Subject_VMCS_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "VMCS memory region 'linux|vmcs' mapped at unexpected "
                    & "kernel virtual address 16#cafe_0000#, should be "
                    & "16#002e_4000#",
                    Message   => "Exception mismatch (1)");
      end;

      --  Kernel and subject with different CPU.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']",
         Name  => "cpu",
         Value => "0");
      begin
         Subject_VMCS_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

    exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "VMCS memory region 'linux|vmcs' mapped by "
                    & "kernel and subject 'linux' with different CPU ID: "
                    & "1 /= 0",
                    Message   => "Exception mismatch (2)");
      end;

      --  Multiple kernel subject VMCS mappings.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@physical="
         & "'sm|timed_event']",
         Name  => "physical",
         Value => "vt|vmcs");
      begin
         Subject_VMCS_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (3)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "VMCS memory region 'vt|vmcs' has multiple kernel "
                    & "mappings: 2",
                    Message   => "Exception mismatch (3)");
      end;

      --  No kernel subject VMCS mapping.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@logical="
         & "'sm|timed_event']",
         Name  => "physical",
         Value => "nonexistent");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@logical='vt|vmcs']",
         Name  => "physical",
         Value => "nonexistent");
      begin
         Subject_VMCS_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (4)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "VMCS memory region 'vt|vmcs' is not mapped by"
                    & " any kernel",
                    Message   => "Exception mismatch (4)");
      end;
--  begin read only
   end Test_Subject_VMCS_Mappings;
--  end read only


--  begin read only
   procedure Test_Subject_FPU_State_Mappings (Gnattest_T : in out Test);
   procedure Test_Subject_FPU_State_Mappings_7f61b5 (Gnattest_T : in out Test) renames Test_Subject_FPU_State_Mappings;
--  id:2.2/7f61b58ebe2db8a3/Subject_FPU_State_Mappings/1/0/
   procedure Test_Subject_FPU_State_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Subject_FPU_State_Mappings (XML_Data => Data);

      --  Kernel subject FPU state mappings with different virtual base
      --  addresses.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@logical='linux|fpu']",
         Name  => "virtualAddress",
         Value => "16#cafe_0000#");
      begin
         Subject_FPU_State_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject FPU state memory region 'linux|fpu' mapped at "
                    & "unexpected kernel virtual address 16#cafe_0000#, should"
                    & " be 16#00b0_4000#",
                    Message   => "Exception mismatch (1)");
      end;

      --  Kernel and subject with different CPU.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='linux']",
         Name  => "cpu",
         Value => "0");
      begin
         Subject_FPU_State_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

    exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject FPU state memory region 'linux|fpu' mapped by "
                    & "kernel and subject 'linux' with different CPU ID: "
                    & "1 /= 0",
                    Message   => "Exception mismatch (2)");
      end;

      --  Multiple kernel subject FPU state mappings.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@physical="
         & "'sm|timed_event']",
         Name  => "physical",
         Value => "vt|fpu");
      begin
         Subject_FPU_State_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (3)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject FPU state memory region 'vt|fpu' has multiple "
                    & "kernel mappings: 2",
                    Message   => "Exception mismatch (3)");
      end;

      --  No kernel subject FPU state mapping.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@logical="
         & "'sm|timed_event']",
         Name  => "physical",
         Value => "nonexistent");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory[@logical='vt|fpu']",
         Name  => "physical",
         Value => "nonexistent");
      begin
         Subject_FPU_State_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (4)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject FPU state memory region 'vt|fpu' is not mapped "
                    & "by any kernel",
                    Message   => "Exception mismatch (4)");
      end;
--  begin read only
   end Test_Subject_FPU_State_Mappings;
--  end read only


--  begin read only
   procedure Test_Subject_FPU_State_Region_Presence (Gnattest_T : in out Test);
   procedure Test_Subject_FPU_State_Region_Presence_9fdd2f (Gnattest_T : in out Test) renames Test_Subject_FPU_State_Region_Presence;
--  id:2.2/9fdd2fb75302db20/Subject_FPU_State_Region_Presence/1/0/
   procedure Test_Subject_FPU_State_Region_Presence (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Subject_FPU_State_Region_Presence (XML_Data => Data);

      --  Missing subject FPU state region.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='linux|fpu']",
         Name  => "name",
         Value => "foobar");

      begin
         Subject_FPU_State_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject fpu region 'linux|fpu' for subject 'linux' not"
                    & " found",
                    Message   => "Exception mismatch (1)");
      end;

      --  Subject FPU state region with incorrect region type.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='tau0|fpu']",
         Name  => "type",
         Value => "subject");

      begin
         Subject_FPU_State_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject fpu region 'tau0|fpu' for subject 'tau0' not"
                    & " found",
                    Message   => "Exception mismatch (2)");
      end;
--  begin read only
   end Test_Subject_FPU_State_Region_Presence;
--  end read only


--  begin read only
   procedure Test_Subject_Timed_Event_Region_Presence (Gnattest_T : in out Test);
   procedure Test_Subject_Timed_Event_Region_Presence_8a0459 (Gnattest_T : in out Test) renames Test_Subject_Timed_Event_Region_Presence;
--  id:2.2/8a045933feb3eda4/Subject_Timed_Event_Region_Presence/1/0/
   procedure Test_Subject_Timed_Event_Region_Presence (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Subject_Timed_Event_Region_Presence (XML_Data => Data);

      --  Missing subject timed event region.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='vt|timed_event']",
         Name  => "name",
         Value => "foobar");

      begin
         Subject_Timed_Event_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject timed_event region 'vt|timed_event' for subject"
                    & " 'vt' not found",
                    Message   => "Exception mismatch (1)");
      end;

      --  Subject timed event region with incorrect region type.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='tau0|timed_event']",
         Name  => "type",
         Value => "subject");

      begin
         Subject_Timed_Event_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject timed_event region 'tau0|timed_event' for "
                    & "subject 'tau0' not found",
                    Message   => "Exception mismatch (2)");
      end;
--  begin read only
   end Test_Subject_Timed_Event_Region_Presence;
--  end read only


--  begin read only
   procedure Test_Subject_MSR_Store_Region_Presence (Gnattest_T : in out Test);
   procedure Test_Subject_MSR_Store_Region_Presence_ef7581 (Gnattest_T : in out Test) renames Test_Subject_MSR_Store_Region_Presence;
--  id:2.2/ef758149cf6041df/Subject_MSR_Store_Region_Presence/1/0/
   procedure Test_Subject_MSR_Store_Region_Presence (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Subject_MSR_Store_Region_Presence (XML_Data => Data);

      --  Missing subject MSR store region.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='linux|msrstore']",
         Name  => "name",
         Value => "foobar");

      begin
         Subject_MSR_Store_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject MSR store region 'linux|msrstore' for subject "
                    & "'linux' not found",
                    Message   => "Exception mismatch");
      end;

      --  Subject MSR store region with incorrect region type.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='foobar']",
         Name  => "name",
         Value => "linux|msrstore");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='linux|msrstore']",
         Name  => "type",
         Value => "subject");

      begin
         Subject_MSR_Store_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject MSR store region 'linux|msrstore' for subject "
                    & "'linux' not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Subject_MSR_Store_Region_Presence;
--  end read only


--  begin read only
   procedure Test_Scheduling_Group_Info_Region_Presence (Gnattest_T : in out Test);
   procedure Test_Scheduling_Group_Info_Region_Presence_54e535 (Gnattest_T : in out Test) renames Test_Scheduling_Group_Info_Region_Presence;
--  id:2.2/54e5352eb4c027ff/Scheduling_Group_Info_Region_Presence/1/0/
   procedure Test_Scheduling_Group_Info_Region_Presence (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Scheduling_Group_Info_Region_Presence (XML_Data => Data);

      --  Missing sched group info region.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='scheduling_group_info_1']",
         Name  => "name",
         Value => "foobar");

      begin
         Scheduling_Group_Info_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Scheduling group info region of scheduling group 1 not"
                    & " found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Scheduling_Group_Info_Region_Presence;
--  end read only


--  begin read only
   procedure Test_Subject_Sched_Group_Info_Mappings (Gnattest_T : in out Test);
   procedure Test_Subject_Sched_Group_Info_Mappings_97d94f (Gnattest_T : in out Test) renames Test_Subject_Sched_Group_Info_Mappings;
--  id:2.2/97d94f92347c2094/Subject_Sched_Group_Info_Mappings/1/0/
   procedure Test_Subject_Sched_Group_Info_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Subject_Sched_Group_Info_Mappings (XML_Data => Data);

      --  Missing scheduling group info region mapping.

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/memory/memory"
         & "[@physical='scheduling_group_info_1']",
         Name  => "physical",
         Value => "foobar");

      begin
         Subject_Sched_Group_Info_Mappings (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject 'vt' has no mapping for info region of "
                    & "scheduling group 1",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Subject_Sched_Group_Info_Mappings;
--  end read only


--  begin read only
   procedure Test_VTd_Root_Region_Size (Gnattest_T : in out Test);
   procedure Test_VTd_Root_Region_Size_bc3a31 (Gnattest_T : in out Test) renames Test_VTd_Root_Region_Size;
--  id:2.2/bc3a31ac2395433f/VTd_Root_Region_Size/1/0/
   procedure Test_VTd_Root_Region_Size (Gnattest_T : in out Test) is
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


--  begin read only
   procedure Test_VTd_IRT_Region_Presence (Gnattest_T : in out Test);
   procedure Test_VTd_IRT_Region_Presence_8b55f8 (Gnattest_T : in out Test) renames Test_VTd_IRT_Region_Presence;
--  id:2.2/8b55f8befd365161/VTd_IRT_Region_Presence/1/0/
   procedure Test_VTd_IRT_Region_Presence (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      VTd_IRT_Region_Presence (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@type='system_vtd_ir']",
         Name  => "type",
         Value => "subject");

      begin
         VTd_IRT_Region_Presence (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "VT-d interrupt remapping table memory region not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_VTd_IRT_Region_Presence;
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
end Mucfgcheck.Memory.Test_Data.Tests;
