--
--  Copyright (C) 2013, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp;

with X86_64;

with SK.CPU_Info;
with SK.Crash_Audit;
with SK.Interrupt_Tables;

package SK.VMX
with
   Abstract_State =>
     (VMCS_State with External => (Async_Readers, Async_Writers))
is

   --  Physical memory address of VMX exit handler.
   Exit_Address : constant SK.Word64
   with
      Import,
      Convention => C,
      Link_Name  => "vmx_exit_handler_ptr";

   --  Enter VMX root operation.
   procedure Enter_Root_Mode
   with
      Global => (Input  => (CPU_Info.APIC_ID, CPU_Info.CPU_ID),
                 In_Out => (Crash_Audit.State, X86_64.State));

   --  Reset VMCS of subject specified by ID.
   procedure Reset
     (VMCS_Address : SK.Word64;
      Subject_ID   : Skp.Global_Subject_ID_Type)
   with
      Global => (Input  => CPU_Info.APIC_ID,
                 In_Out => (VMCS_State, Crash_Audit.State, X86_64.State));

   --  Load VMCS with given address.
   procedure Load (VMCS_Address : SK.Word64)
   with
      Global => (Input  => CPU_Info.APIC_ID,
                 In_Out => (Crash_Audit.State, X86_64.State));

   --  Read value from specified field of the current, active VMCS. If the
   --  operation fails, CPU.Panic is called.
   procedure VMCS_Read
     (Field :     SK.Word16;
      Value : out SK.Word64)
   with
      Global => (Input  => CPU_Info.APIC_ID,
                 In_Out => (Crash_Audit.State, X86_64.State));

   --  Write given value to the specified field of the current, active VMCS. If
   --  the operation fails, CPU.Panic is called.
   procedure VMCS_Write
     (Field : SK.Word16;
      Value : SK.Word64)
   with
      Global => (Input  => CPU_Info.APIC_ID,
                 In_Out => (Crash_Audit.State, X86_64.State));

   --  Setup control fields of the currently active VMCS.
   procedure VMCS_Setup_Control_Fields
     (IO_Bitmap_Address  : SK.Word64;
      MSR_Bitmap_Address : SK.Word64;
      MSR_Store_Address  : SK.Word64;
      MSR_Count          : SK.Word32;
      Ctls_Exec_Pin      : SK.Word32;
      Ctls_Exec_Proc     : SK.Word32;
      Ctls_Exec_Proc2    : SK.Word32;
      Ctls_Exit          : SK.Word32;
      Ctls_Entry         : SK.Word32;
      CR0_Mask           : SK.Word64;
      CR4_Mask           : SK.Word64;
      Exception_Bitmap   : SK.Word32)
   with
      Global => (Input  => CPU_Info.APIC_ID,
                 In_Out => (Crash_Audit.State, X86_64.State));

   --  Setup host fields of the currently active VMCS.
   procedure VMCS_Setup_Host_Fields
   with
      Global => (Input  => (Exit_Address, CPU_Info.APIC_ID,
                            Interrupt_Tables.State),
                 In_Out => (Crash_Audit.State, X86_64.State));

   --  Setup guest fields of the currently active VMCS.
   procedure VMCS_Setup_Guest_Fields
     (PML4_Address : SK.Word64;
      EPT_Pointer  : SK.Word64;
      RIP_Value    : SK.Word64;
      RSP_Value    : SK.Word64;
      CR0_Value    : SK.Word64;
      CR4_Value    : SK.Word64;
      CS_Access    : SK.Word32)
   with
      Global => (Input  => CPU_Info.APIC_ID,
                 In_Out => (Crash_Audit.State, X86_64.State));

   --  Enable/Disable interrupt-window exiting depending on the given value.
   procedure VMCS_Set_Interrupt_Window (Value : Boolean)
   with
      Global => (Input  => CPU_Info.APIC_ID,
                 In_Out => (Crash_Audit.State, X86_64.State));

   --  Report VMX launch/resume error and panic.
   procedure VMX_Error
   with
      Global     => (In_Out => X86_64.State),
      No_Return,
      Export,
      Convention => C,
      Link_Name  => "vmx_error";

end SK.VMX;
