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

with Validators.Memory;

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
        (Routine => Validate_VMXON_Presence'Access,
         Name    => "Validate presence of VMXON regions");
      T.Add_Test_Routine
        (Routine => Validate_VMXON_Size'Access,
         Name    => "Validate size of VMXON regions");
      T.Add_Test_Routine
        (Routine => Validate_VMXON_In_Lowmem'Access,
         Name    => "Validate physical address of VMXON regions");
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
   end Initialize;

   -------------------------------------------------------------------------

   procedure Validate_CPU_Entity_Name_Encoding
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      declare
         Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
           (List  => McKae.XML.XPath.XIA.XPath_Query
              (N     => Data.Doc,
               XPath => "/system/memory/memory[@name='invalid_0|vmxon']"),
            Index => 0);
      begin

         --  Set invalid CPU number in entity reference.

         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "name",
            Value => "kernel_1|vmxon");

         Validators.Memory.Entity_Name_Encoding (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Entity 'kernel_1' encoded in memory region "
                    & "'kernel_1|vmxon' does not exist or is invalid",
                    Message   => "Exception mismatch");
      end;
   end Validate_CPU_Entity_Name_Encoding;

   -------------------------------------------------------------------------

   procedure Validate_Entity_Name_Encoding
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Memory.Entity_Name_Encoding (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Entity 'invalid_0' encoded in memory region "
                    & "'invalid_0|vmxon' does not exist or is invalid",
                    Message   => "Exception mismatch");
      end;
   end Validate_Entity_Name_Encoding;

   -------------------------------------------------------------------------

   procedure Validate_Physaddr_Alignment
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Memory.Physical_Address_Alignment (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'physicalAddress => 16#0010_0023#' of "
                    & "'kernel_text' physical memory element not page aligned",
                    Message   => "Exception mismatch");
      end;
   end Validate_Physaddr_Alignment;

   -------------------------------------------------------------------------

   procedure Validate_Physmem_Overlap
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Memory.Physical_Memory_Overlap (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Overlap of physical memory region 'invalid_0|vmxon'"
                    & " and 'invalid|vmcs'",
                    Message   => "Exception mismatch");
      end;
   end Validate_Physmem_Overlap;

   -------------------------------------------------------------------------

   procedure Validate_Physmem_Refs
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Memory.Physical_Memory_References (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical memory 'lnx_mem' referenced by logical memory"
                    & " 'linux' not found",
                    Message   => "Exception mismatch");
      end;
   end Validate_Physmem_Refs;

   -------------------------------------------------------------------------

   procedure Validate_Region_Size
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Memory.Region_Size (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'size => 16#0042#' of 'ram_1' physical "
                    & "memory element not multiple of page size (4K)",
                    Message   => "Exception mismatch");
      end;
   end Validate_Region_Size;

   -------------------------------------------------------------------------

   procedure Validate_Virtaddr_Alignment
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Memory.Virtual_Address_Alignment (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'virtualAddress => 16#000e_0500#' of 'linux' "
                    & "logical memory element not page aligned",
                    Message   => "Exception mismatch");
      end;
   end Validate_Virtaddr_Alignment;

   -------------------------------------------------------------------------

   procedure Validate_VMCS_In_Lowmem
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Memory.VMCS_In_Lowmem (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'physicalAddress => 16#0010_0000#' of "
                    & "'invalid|vmcs' VMCS memory element not below 1 MiB",
                    Message   => "Exception mismatch");
      end;
   end Validate_VMCS_In_Lowmem;

   -------------------------------------------------------------------------

   procedure Validate_VMCS_Presence
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Memory.VMCS_Region_Presence (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
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
                   File => "data/validators.xml");

      begin
         Validators.Memory.VMCS_Region_Size (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'size => 16#0001_0013#' of 'invalid|vmcs' "
                    & "VMCS memory element not 4K",
                    Message   => "Exception mismatch");
      end;
   end Validate_VMCS_Size;

   -------------------------------------------------------------------------

   procedure Validate_VMXON_In_Lowmem
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Memory.VMXON_In_Lowmem (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'physicalAddress => 16#0010_0000#' of "
                    & "'invalid_0|vmxon' VMXON memory element not below 1 MiB",
                    Message   => "Exception mismatch");
      end;
   end Validate_VMXON_In_Lowmem;

   -------------------------------------------------------------------------

   procedure Validate_VMXON_Presence
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   File => "data/validators.xml");

      begin
         Validators.Memory.VMXON_Region_Presence (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
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
                   File => "data/validators.xml");

      begin
         Validators.Memory.VMXON_Region_Size (XML_Data => Data);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'size => 16#0001_0012#' of 'invalid_0|vmxon' "
                    & "VMXON memory element not 4K",
                    Message   => "Exception mismatch");
      end;
   end Validate_VMXON_Size;

end Memory_Tests;
