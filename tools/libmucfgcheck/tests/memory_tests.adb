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

with Muxml.Utils;

with Mucfgcheck.Memory;

package body Memory_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Memory validator tests");
      T.Add_Test_Routine
        (Routine => Validate_Physmem_Refs'Access,
         Name    => "Validate physical memory references");
      T.Add_Test_Routine
        (Routine => Validate_Physmem_Name_Uniqueness'Access,
         Name    => "Validate physical memory name uniqueness");
      T.Add_Test_Routine
        (Routine => Validate_VMXON_Presence'Access,
         Name    => "Validate presence of VMXON regions");
      T.Add_Test_Routine
        (Routine => Validate_VMXON_Size'Access,
         Name    => "Validate size of VMXON regions");
      T.Add_Test_Routine
        (Routine => Validate_VMXON_In_Lowmem'Access,
         Name    => "Validate physical address of VMXON regions");
      T.Add_Test_Routine
        (Routine => Validate_VMXON_Consecutiveness'Access,
         Name    => "Validate consecutiveness of VMXON regions");
      T.Add_Test_Routine
        (Routine => Validate_VMCS_Presence'Access,
         Name    => "Validate presence of VMCS regions");
      T.Add_Test_Routine
        (Routine => Validate_VMCS_Size'Access,
         Name    => "Validate size of VMCS regions");
      T.Add_Test_Routine
        (Routine => Validate_VMCS_In_Lowmem'Access,
         Name    => "Validate physical address of VMCS regions");
      T.Add_Test_Routine
        (Routine => Validate_VMCS_Consecutiveness'Access,
         Name    => "Validate consecutiveness of VMCS regions");
      T.Add_Test_Routine
        (Routine => Validate_Physaddr_Alignment'Access,
         Name    => "Validate physical memory address alignment");
      T.Add_Test_Routine
        (Routine => Validate_Virtaddr_Alignment'Access,
         Name    => "Validate virtual memory address alignment");
      T.Add_Test_Routine
        (Routine => Validate_Region_Size'Access,
         Name    => "Validate memory region size");
      T.Add_Test_Routine
        (Routine => Validate_Entity_Name_Encoding'Access,
         Name    => "Validate entity name encoding");
      T.Add_Test_Routine
        (Routine => Validate_CPU_Entity_Name_Encoding'Access,
         Name    => "Validate CPU entity name encoding");
      T.Add_Test_Routine
        (Routine => Validate_Physmem_Overlap'Access,
         Name    => "Validate physical memory region overlap");
      T.Add_Test_Routine
        (Routine => Validate_Physmem_Overlap_Device'Access,
         Name    => "Validate physical device memory region overlap");
      T.Add_Test_Routine
        (Routine => Validate_Virtmem_Overlap_Kernel'Access,
         Name    => "Validate kernel virtual memory region overlap");
      T.Add_Test_Routine
        (Routine => Validate_Virtmem_Overlap_Subject'Access,
         Name    => "Validate subject virtual memory region overlap");
      T.Add_Test_Routine
        (Routine => Validate_Virtmem_Overlap_Device_Kernel'Access,
         Name    => "Validate kernel device memory overlap");
      T.Add_Test_Routine
        (Routine => Validate_Virtmem_Overlap_Device_Subject'Access,
         Name    => "Validate subject device memory overlap");
      T.Add_Test_Routine
        (Routine => Validate_Kernel_PT_Consecutiveness'Access,
         Name    => "Validate consecutiveness of kernel PT regions");
      T.Add_Test_Routine
        (Routine => Validate_Kernel_Stack_Region_Presence'Access,
         Name    => "Validate kernel stack region presence");
      T.Add_Test_Routine
        (Routine => Validate_Kernel_Store_Region_Presence'Access,
         Name    => "Validate kernel store region presence");
      T.Add_Test_Routine
        (Routine => Validate_Kernel_PT_Region_Presence'Access,
         Name    => "Validate kernel PT region presence");
      T.Add_Test_Routine
        (Routine => Validate_Kernel_Memory_Mappings'Access,
         Name    => "Validate kernel memory mappings");
      T.Add_Test_Routine
        (Routine => Validate_System_Memory_Mappings'Access,
         Name    => "Validate system memory mappings");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Validate_CPU_Entity_Name_Encoding
   is
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
         Mucfgcheck.Memory.Entity_Name_Encoding (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Entity 'kernel_5' encoded in memory region "
                    & "'kernel_5|vmxon' does not exist or is invalid",
                    Message   => "Exception mismatch");
      end;
   end Validate_CPU_Entity_Name_Encoding;

   -------------------------------------------------------------------------

   procedure Validate_Entity_Name_Encoding
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_0|vmxon']",
         Name  => "name",
         Value => "invalid_0|vmxon");

      begin
         Mucfgcheck.Memory.Entity_Name_Encoding (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Entity 'invalid_0' encoded in memory region "
                    & "'invalid_0|vmxon' does not exist or is invalid",
                    Message   => "Exception mismatch");
      end;
   end Validate_Entity_Name_Encoding;

   -------------------------------------------------------------------------

   procedure Validate_Kernel_Memory_Mappings
   is
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
         Mucfgcheck.Memory.Kernel_Memory_Mappings (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Kernel memory region 'vt|bin' mapped by logical memory "
                    & "region 'binary' of subject 'vt'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Kernel_Memory_Mappings;

   -------------------------------------------------------------------------

   procedure Validate_Kernel_PT_Consecutiveness
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_0|pt']",
         Name  => "physicalAddress",
         Value => "16#0000#");

      begin
         Mucfgcheck.Memory.Kernel_PT_Consecutiveness (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Kernel PT memory region 'kernel_0|pt' not adjacent to "
                    & "other PT regions",
                    Message   => "Exception mismatch");
      end;
   end Validate_Kernel_PT_Consecutiveness;

   -------------------------------------------------------------------------

   procedure Validate_Kernel_PT_Region_Presence
   is
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
         Mucfgcheck.Memory.Kernel_PT_Region_Presence (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Kernel PT region 'kernel_0|pt' for logical CPU 0"
                    & " not found",
                    Message   => "Exception mismatch");
      end;
   end Validate_Kernel_PT_Region_Presence;

   -------------------------------------------------------------------------

   procedure Validate_Kernel_Stack_Region_Presence
   is
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
         Mucfgcheck.Memory.Kernel_Stack_Region_Presence (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Kernel stack region 'kernel_stack_0' for logical CPU 0"
                    & " not found",
                    Message   => "Exception mismatch");
      end;
   end Validate_Kernel_Stack_Region_Presence;

   -------------------------------------------------------------------------

   procedure Validate_Kernel_Store_Region_Presence
   is
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
         Mucfgcheck.Memory.Kernel_Store_Region_Presence (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Kernel store region 'kernel_store_0' for logical CPU 0"
                    & " not found",
                    Message   => "Exception mismatch");
      end;
   end Validate_Kernel_Store_Region_Presence;

   -------------------------------------------------------------------------

   procedure Validate_Physaddr_Alignment
   is
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
         Mucfgcheck.Memory.Physical_Address_Alignment (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'physicalAddress => 16#0010_0023#' of "
                    & "'kernel_text' physical memory element not page aligned",
                    Message   => "Exception mismatch");
      end;
   end Validate_Physaddr_Alignment;

   -------------------------------------------------------------------------

   procedure Validate_Physmem_Name_Uniqueness
   is
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
         Mucfgcheck.Memory.Physical_Memory_Name_Uniqueness (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Multiple physical memory regions with name"
                    & " 'kernel_1|vmxon'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Physmem_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Validate_Physmem_Overlap
   is
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
         Mucfgcheck.Memory.Physical_Memory_Overlap (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Overlap of physical or device memory region 'linux|ram'"
                    & " and 'kernel_text'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Physmem_Overlap;

   -------------------------------------------------------------------------

   procedure Validate_Physmem_Overlap_Device
   is
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
         Mucfgcheck.Memory.Physical_Memory_Overlap (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Overlap of physical or device memory region "
                    & "'kernel_text' and 'buffer'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Physmem_Overlap_Device;

   -------------------------------------------------------------------------

   procedure Validate_Physmem_Refs
   is
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
         Mucfgcheck.Memory.Physical_Memory_References (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical memory 'linux|ram' referenced by logical "
                    & "memory 'ram' not found",
                    Message   => "Exception mismatch");
      end;
   end Validate_Physmem_Refs;

   -------------------------------------------------------------------------

   procedure Validate_Region_Size
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/memory/memory[@name='kernel_text']",
         Name  => "size",
         Value => "16#0042#");

      begin
         Mucfgcheck.Memory.Region_Size (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'size => 16#0042#' of 'kernel_text' physical "
                    & "memory element not multiple of page size (4K)",
                    Message   => "Exception mismatch");
      end;
   end Validate_Region_Size;

   -------------------------------------------------------------------------

   procedure Validate_System_Memory_Mappings
   is
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
         Mucfgcheck.Memory.System_Memory_Mappings (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "System memory region 'vt|bin' is mapped by logical "
                    & "memory region 'binary'",
                    Message   => "Exception mismatch");
      end;
   end Validate_System_Memory_Mappings;

   -------------------------------------------------------------------------

   procedure Validate_Virtaddr_Alignment
   is
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
         Mucfgcheck.Memory.Virtual_Address_Alignment (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'virtualAddress => 16#000e_0500#' of 'binary'"
                    & " logical memory element not page aligned",
                    Message   => "Exception mismatch");
      end;
   end Validate_Virtaddr_Alignment;

   -------------------------------------------------------------------------

   procedure Validate_Virtmem_Overlap_Device_Kernel
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
         Mucfgcheck.Memory.Virtual_Memory_Overlap (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Overlap of virtual memory region 'tau0_interface' and "
                    & "'mmio' of kernel running on CPU 0",
                    Message   => "Exception mismatch");
      end;
   end Validate_Virtmem_Overlap_Device_Kernel;

   -------------------------------------------------------------------------

   procedure Validate_Virtmem_Overlap_Device_Subject
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
         Mucfgcheck.Memory.Virtual_Memory_Overlap (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Overlap of virtual memory region 'binary' and 'buffer'"
                    & " of subject 'vt'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Virtmem_Overlap_Device_Subject;

   -------------------------------------------------------------------------

   procedure Validate_Virtmem_Overlap_Kernel
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
         Mucfgcheck.Memory.Virtual_Memory_Overlap (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Overlap of virtual memory region 'text' and 'data' of"
                    & " kernel running on CPU 0",
                    Message   => "Exception mismatch");
      end;
   end Validate_Virtmem_Overlap_Kernel;

   -------------------------------------------------------------------------

   procedure Validate_Virtmem_Overlap_Subject
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
         Mucfgcheck.Memory.Virtual_Memory_Overlap (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Overlap of virtual memory region 'binary' and "
                    & "'zero_page' of subject 'linux'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Virtmem_Overlap_Subject;

   -------------------------------------------------------------------------

   procedure Validate_VMCS_Consecutiveness
   is
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
         Mucfgcheck.Memory.VMCS_Consecutiveness (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Memory region 'linux|vmcs' not adjacent to other"
                    & " VMCS regions",
                    Message   => "Exception mismatch");
      end;
   end Validate_VMCS_Consecutiveness;

   -------------------------------------------------------------------------

   procedure Validate_VMCS_In_Lowmem
   is
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
         Mucfgcheck.Memory.VMCS_In_Lowmem (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'physicalAddress => 16#0010_0000#' of "
                    & "'time|vmcs' VMCS memory element not below 1 MiB",
                    Message   => "Exception mismatch");
      end;
   end Validate_VMCS_In_Lowmem;

   -------------------------------------------------------------------------

   procedure Validate_VMCS_Presence
   is
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
         Mucfgcheck.Memory.VMCS_Region_Presence (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "VMCS region 'linux|vmcs' for subject linux not found",
                    Message   => "Exception mismatch");
      end;
   end Validate_VMCS_Presence;

   -------------------------------------------------------------------------

   procedure Validate_VMCS_Size
   is
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
         Mucfgcheck.Memory.VMCS_Region_Size (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'size => 16#0001_0013#' of 'sm|vmcs' "
                    & "VMCS memory element not 4K",
                    Message   => "Exception mismatch");
      end;
   end Validate_VMCS_Size;

   -------------------------------------------------------------------------

   procedure Validate_VMXON_Consecutiveness
   is
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
         Mucfgcheck.Memory.VMXON_Consecutiveness (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Memory region 'kernel_0|vmxon' not adjacent to other"
                    & " VMXON regions",
                    Message   => "Exception mismatch");
      end;
   end Validate_VMXON_Consecutiveness;

   -------------------------------------------------------------------------

   procedure Validate_VMXON_In_Lowmem
   is
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
         Mucfgcheck.Memory.VMXON_In_Lowmem (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'physicalAddress => 16#0010_0000#' of "
                    & "'kernel_1|vmxon' VMXON memory element not below 1 MiB",
                    Message   => "Exception mismatch");
      end;
   end Validate_VMXON_In_Lowmem;

   -------------------------------------------------------------------------

   procedure Validate_VMXON_Presence
   is
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
         Mucfgcheck.Memory.VMXON_Region_Presence (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "VMXON region 'kernel_0|vmxon' for logical CPU 0 not "
                    & "found",
                    Message   => "Exception mismatch");
      end;
   end Validate_VMXON_Presence;

   -------------------------------------------------------------------------

   procedure Validate_VMXON_Size
   is
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
         Mucfgcheck.Memory.VMXON_Region_Size (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Mucfgcheck.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'size => 16#0001_0012#' of 'kernel_0|vmxon' "
                    & "VMXON memory element not 4K",
                    Message   => "Exception mismatch");
      end;
   end Validate_VMXON_Size;

end Memory_Tests;
