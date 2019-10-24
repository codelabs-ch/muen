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

--D @Interface
--D This package implements subprograms corresponding to lowlevel Intel VT-x
--D instructions. They are required for the management of VMX data structures
--D such as VMXON and VMCS regions, see Intel SDM Vol. 3C, chapter 30
--D \cite{intelsdm}.
package SK.VMX
with
   Abstract_State =>
     (VMCS_State with External => (Async_Readers, Async_Writers))
is

   --  Physical memory address of VMX exit handler.
   Exit_Address : constant Word64
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
     (VMCS_Address : Word64;
      Subject_ID   : Skp.Global_Subject_ID_Type)
   with
      Global => (Input  => CPU_Info.APIC_ID,
                 In_Out => (VMCS_State, Crash_Audit.State, X86_64.State));

   --  Load VMCS with given address.
   procedure Load (VMCS_Address : Word64)
   with
      Global => (Input  => CPU_Info.APIC_ID,
                 In_Out => (Crash_Audit.State, X86_64.State));

   --  Read value from specified field of the current, active VMCS.
   procedure VMCS_Read
     (Field :     Word16;
      Value : out Word64)
   with
      Global => (Input  => CPU_Info.APIC_ID,
                 In_Out => (Crash_Audit.State, X86_64.State));

   --  Write given value to the specified field of the current, active VMCS.
   procedure VMCS_Write
     (Field : Word16;
      Value : Word64)
   with
      Global => (Input  => CPU_Info.APIC_ID,
                 In_Out => (Crash_Audit.State, X86_64.State));

   --D @Section Id => impl_vmcs, Label => VMCS Management, Parent => implementation, Priority => 0
   --D @Section Id => impl_vmcs_setup_ctrl, Label => VM-Control Fields Setup, Parent => impl_vmcs, Priority => 0
   --D @Text Section => impl_vmcs_setup_ctrl, Priority => 0
   --D Setup control fields of the currently active VMCS. These fields govern
   --D VMX non-root operation as well as VM Exit and Entry behavior.
   procedure VMCS_Setup_Control_Fields
     (IO_Bitmap_Address  : Word64;
      MSR_Bitmap_Address : Word64;
      MSR_Store_Address  : Word64;
      MSR_Count          : Word32;
      Ctls_Exec_Pin      : Word32;
      Ctls_Exec_Proc     : Word32;
      Ctls_Exec_Proc2    : Word32;
      Ctls_Exit          : Word32;
      Ctls_Entry         : Word32;
      CR0_Mask           : Word64;
      CR4_Mask           : Word64;
      Exception_Bitmap   : Word32)
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
     (PML4_Address : Word64;
      EPT_Pointer  : Word64)
   with
      Global => (Input  => CPU_Info.APIC_ID,
                 In_Out => (Crash_Audit.State, X86_64.State));

   --  Enable/Disable interrupt-window exiting depending on the given value.
   procedure VMCS_Set_Interrupt_Window (Value : Boolean)
   with
      Global => (Input  => CPU_Info.APIC_ID,
                 In_Out => (Crash_Audit.State, X86_64.State));

end SK.VMX;
