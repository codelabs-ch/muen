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
        (Routine => Validate_Physaddr_Alignment'Access,
         Name    => "Validate physical memory address alignment");
      T.Add_Test_Routine
        (Routine => Validate_Virtaddr_Alignment'Access,
         Name    => "Validate virtual memory address alignment");
      T.Add_Test_Routine
        (Routine => Validate_Region_Size'Access,
         Name    => "Validate memory region size");
   end Initialize;

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
                    = "Attribute physicalAddress 16#0010_0023# of physical "
                    & "memory region 'kernel_text' not page aligned",
                    Message   => "Exception mismatch");
      end;
   end Validate_Physaddr_Alignment;

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
                    = "Attribute size 16#8000_0042# of physical memory region "
                    & "'ram_1' not multiple of page size (4K)",
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
                    = "Virtual address 16#000e_0500# of 'linux' not"
                    & " page aligned",
                    Message   => "Exception mismatch");
      end;
   end Validate_Virtaddr_Alignment;

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
                    = "Attribute size 16#0001_0013# of VMCS memory region "
                    & "'invalid|vmcs' not 4K",
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
                    = "Attribute physicalAddress 16#0010_0000# of VMXON memory"
                    & " region 'invalid_0|vmxon' not below 1 MiB",
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
                    = "Attribute size 16#0001_0012# of VMXON memory region "
                    & "'invalid_0|vmxon' not 4K",
                    Message   => "Exception mismatch");
      end;
   end Validate_VMXON_Size;

end Memory_Tests;
