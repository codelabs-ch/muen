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
--D This package implements subprograms to enter VMX root operation as well as
--D for higher level access and management of VMX structures such as VMCS and
--D VMXON regions, see Intel SDM Vol. 3C, "Chapter 24 Virtual Machine Control
--D Structures" \cite{intelsdm}.
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

   --D @Section Id => impl_vmcs_enter_root_mode, Label => Entering VMX root mode, Parent => impl_vmcs
   --D @Text Section => impl_vmcs_enter_root_mode
   --D Bring CPU into VMX root operation.
   procedure Enter_Root_Mode
   with
      Global => (Input  => (CPU_Info.APIC_ID, CPU_Info.CPU_ID),
                 In_Out => (Crash_Audit.State, X86_64.State));

   --D @Section Id => impl_vmcs_reset, Label => VMCS Reset, Parent => impl_vmcs
   --D @Text Section => impl_vmcs_reset
   --D Resetting a VMCS located at a specific physical memory address associated
   --D with a specific subject means clearing all data and initializing the VMCS
   --D for (re)use.
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

   --D @Section Id => impl_vmcs, Label => VMCS Management, Parent => implementation
   --D @Section Id => impl_vmcs_setup_ctrl, Label => VM-Control Fields Setup, Parent => impl_vmcs
   --D @Text Section => impl_vmcs_setup_ctrl
   --D Setup control fields of the currently active VMCS. These fields govern
   --D VMX non-root operation as well as VM Exit and Entry behavior.
   procedure VMCS_Setup_Control_Fields
     (VPID               : Word64;
      IO_Bitmap_Address  : Word64;
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

   --D @Section Id => impl_vmcs_setup_host, Label => Host-State Fields Setup, Parent => impl_vmcs
   --D @Text Section => impl_vmcs_setup_host
   --D Setup host-state fields of the currently active VMCS. Processor state is
   --D loaded from these fields on every VM exit, see Intel SDM Vol. 3C,
   --D "27.5 Loading Host State".
   procedure VMCS_Setup_Host_Fields
   with
      Global => (Input  => (Exit_Address, CPU_Info.APIC_ID,
                            Interrupt_Tables.State),
                 In_Out => (Crash_Audit.State, X86_64.State));

   --D @Section Id => impl_vmcs_setup_guest, Label => Guest-State Fields Setup, Parent => impl_vmcs
   --D @Text Section => impl_vmcs_setup_guest
   --D Setup guest-state fields of the currently active VMCS. Processor state is
   --D loaded from these fields on every VM entry (Intel SDM Vol. 3C,
   --D "27.3 Saving Guest State") and stored on very VM exit (Intel SDM Vol. 3C,
   --D "26.3.2 Loading Guest State").
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
