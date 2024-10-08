--
--  Copyright (C) 2013-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.CPU;
with SK.KC;
with SK.Bitops;
with SK.Constants;

package body SK.System_State
is

   -------------------------------------------------------------------------

   procedure Enable_VMX_Feature
   is
      MSR_Feature_Control : Word64;
   begin
      --D @OL Id => impl_vmcs_enable_vmx_steps, Section => impl_vmcs_enable_vmx, Priority => 10
      --D @Item List => impl_vmcs_enable_vmx_steps
      --D Read the current value of the \texttt{IA32\_FEATURE\_CONTROL} MSR.
      MSR_Feature_Control := CPU.Get_MSR64
        (Register => Constants.IA32_FEATURE_CONTROL);

      if not Bitops.Bit_Test
        (Value => MSR_Feature_Control,
         Pos   => Constants.IA32_FCTRL_LOCKED_FLAG)
      then

         --D @Item List => impl_vmcs_enable_vmx_steps
         --D If the lock bit is not set, then explicitly disable
         --D 'VMX in SMX operation' by clearing bit 1.
         MSR_Feature_Control := Bitops.Bit_Clear
           (Value => MSR_Feature_Control,
            Pos   => Constants.IA32_FCTRL_VMX_IN_SMX_FLAG);

         --D @Item List => impl_vmcs_enable_vmx_steps
         --D Then, enable 'VMX outside SMX operation' by setting bit 2.
         MSR_Feature_Control := Bitops.Bit_Set
           (Value => MSR_Feature_Control,
            Pos   => Constants.IA32_FCTRL_VMX_FLAG);

         --D @Item List => impl_vmcs_enable_vmx_steps
         --D Finally, lock the \texttt{IA32\_FEATURE\_CONTROL} register by
         --D setting the locked flag and writing the value back to the  MSR.
         MSR_Feature_Control := Bitops.Bit_Set
           (Value => MSR_Feature_Control,
            Pos   => Constants.IA32_FCTRL_LOCKED_FLAG);

         CPU.Write_MSR64 (Register => Constants.IA32_FEATURE_CONTROL,
                          Value    => MSR_Feature_Control);
      end if;
   end Enable_VMX_Feature;

   -------------------------------------------------------------------------

   --  Check required VMX-fixed bits of given register, see Intel SDM Vol. 3D,
   --  "A.7 VMX-Fixed Bits in CR0" and Intel SDM Vol. 3D, "A.8 VMX-Fixed Bits
   --  in CR4".
   function Fixed_Valid
     (Register : SK.Word64;
      Fixed0   : SK.Word64;
      Fixed1   : SK.Word64)
      return Boolean
   is
   begin
      return
        (Fixed0 or  Register) = Register and
        (Fixed1 and Register) = Register;
   end Fixed_Valid;

   -------------------------------------------------------------------------

   --  Returns True if an invariant TSC is present, see Intel SDM Vol. 3B,
   --  "17.17.1 Invariant TSC".
   function Has_Invariant_TSC return Boolean
   with
      Volatile_Function,
      Global => (Input => X86_64.State)
   is
      Unused_EAX, Unused_EBX, Unused_ECX, EDX : SK.Word32;
   begin
      Unused_EAX := 16#8000_0007#;
      Unused_ECX := 0;

      CPU.CPUID
        (EAX => Unused_EAX,
         EBX => Unused_EBX,
         ECX => Unused_ECX,
         EDX => EDX);

      return Bitops.Bit_Test
        (Value => Word64 (EDX),
         Pos   => Constants.CPUID_FEATURE_INVARIANT_TSC);
   end Has_Invariant_TSC;

   -------------------------------------------------------------------------

   --  Returns True if local APIC is present and supports x2APIC mode, see
   --  Intel SDM Vol. 3A, "10.4.2 Presence of the Local APIC" and Intel SDM
   --  Vol. 3A, "10.12.1 Detecting and Enabling x2APIC Mode".
   function Has_X2_Apic return Boolean
   with
      Volatile_Function,
      Global => (Input => X86_64.State)
   is
      Unused_EAX, Unused_EBX, ECX, EDX : Word32;
   begin
      Unused_EAX := 1;
      ECX        := 0;

      CPU.CPUID
        (EAX => Unused_EAX,
         EBX => Unused_EBX,
         ECX => ECX,
         EDX => EDX);

      return Bitops.Bit_Test
        (Value => Word64 (EDX),
         Pos   => Constants.CPUID_FEATURE_LOCAL_APIC) and then
        Bitops.Bit_Test
          (Value => Word64 (ECX),
           Pos   => Constants.CPUID_FEATURE_X2APIC);
   end Has_X2_Apic;

   -------------------------------------------------------------------------

   --  Returns true if VMX is supported by the CPU.
   function Has_VMX_Support return Boolean
   with
      Volatile_Function,
      Global => (Input => X86_64.State)
   is
      Unused_EAX, Unused_EBX, ECX, Unused_EDX : Word32;
   begin
      Unused_EAX := 1;
      ECX        := 0;

      CPU.CPUID
        (EAX => Unused_EAX,
         EBX => Unused_EBX,
         ECX => ECX,
         EDX => Unused_EDX);

      return Bitops.Bit_Test
        (Value => Word64 (ECX),
         Pos   => Constants.CPUID_FEATURE_VMX_FLAG);
   end Has_VMX_Support;

   -------------------------------------------------------------------------

   --D @Section Id => impl_kernel_init_check_state, Label => System state checks, Parent => impl_kernel_init, Priority => 10
   --D @Text Section => impl_kernel_init_check_state
   --D Validate the system state to ensure correct execution of the kernel and
   --D VMX in particular, see Intel SDM Vol. 3C, "31.5 VMM Setup & Tear Down"
   --D and "30.3 VMX Instructions", \texttt{VMXON}.
   --D @OL Id => impl_kernel_init_check_state_steps, Section => impl_kernel_init_check_state
   procedure Check_State
     (Is_Valid : out Boolean;
      Ctx      : out Crash_Audit_Types.System_Init_Context_Type)
   is
      MSR_Feature_Control, Fixed0, Fixed1 : SK.Word64;

      CR0       : constant Word64 := CPU.Get_CR0;
      CR4       : constant Word64 := CPU.Get_CR4;
      RFLAGS    : constant Word64 := CPU.Get_RFLAGS;
      IA32_EFER : constant Word64 := CPU.Get_MSR64
        (Register => Constants.IA32_EFER);
   begin
      Ctx := Crash_Audit_Types.Null_System_Init_Context;

      --D @Item List => impl_kernel_init_check_state_steps
      --D Check that the processor has support for VMX, see Intel SDM Vol. 3C,
      --D "23.6 Discovering Support for VMX".
      Ctx.VMX_Support := Has_VMX_Support;
      pragma Debug
        (not Ctx.VMX_Support, KC.Put_Line (Item => "Init: VMX not supported"));

      --D @Item List => impl_kernel_init_check_state_steps
      --D Check that VMX was not disabled and locked by the BIOS, see Intel SDM
      --D Vol. 3C, "23.7 Enabling and Entering VMX Operation".
      MSR_Feature_Control := CPU.Get_MSR64
        (Register => Constants.IA32_FEATURE_CONTROL);
      Ctx.Not_VMX_Disabled_Locked := not
        (Bitops.Bit_Test
           (Value => MSR_Feature_Control,
            Pos   => Constants.IA32_FCTRL_LOCKED_FLAG)
         and then not Bitops.Bit_Test
           (Value => MSR_Feature_Control,
            Pos   => Constants.IA32_FCTRL_VMX_FLAG));
      pragma Debug
        (not Ctx.Not_VMX_Disabled_Locked,
         KC.Put_Line (Item => "Init: VMX disabled by BIOS"));

      --D @Item List => impl_kernel_init_check_state_steps
      --D Check that processor is in protected mode, i.e. \texttt{CR0.PE} is
      --D set.
      Ctx.Protected_Mode := Bitops.Bit_Test
        (Value => CR0,
         Pos   => Constants.CR0_PE_FLAG);
      pragma Debug
        (not Ctx.Protected_Mode,
         KC.Put_Line (Item => "Init: Protected mode not enabled"));

      --D @Item List => impl_kernel_init_check_state_steps
      --D Check that processor has Paging enabled, i.e. \texttt{CR0.PG} is set.
      Ctx.Paging := Bitops.Bit_Test
        (Value => CR0,
         Pos   => Constants.CR0_PG_FLAG);
      pragma Debug
        (not Ctx.Paging, KC.Put_Line (Item => "Init: Paging not enabled"));

      --D @Item List => impl_kernel_init_check_state_steps
      --D Check that processor is in IA-32e mode, i.e. \texttt{IA32\_EFER.LMA}
      --D is set.
      Ctx.IA_32e_Mode := Bitops.Bit_Test
        (Value => IA32_EFER,
         Pos   => Constants.IA32_EFER_LMA_FLAG);
      pragma Debug
        (not Ctx.IA_32e_Mode, KC.Put_Line
           (Item => "Init: IA-32e mode not enabled"));

      --D @Item List => impl_kernel_init_check_state_steps
      --D Check that virtual 8086 mode is not enabled, i.e. \texttt{RFLAGS.VM}
      --D is clear.
      Ctx.Not_Virtual_8086 := not Bitops.Bit_Test
        (Value => RFLAGS,
         Pos   => Constants.RFLAGS_VM_FLAG);
      pragma Debug
        (not Ctx.Not_Virtual_8086,
         KC.Put_Line (Item => "Init: Virtual-8086 mode enabled"));

      --D @Item List => impl_kernel_init_check_state_steps
      --D Check that the current processor operating mode meets the required CR0
      --D fixed bits, see Intel SDM Vol. 3D, "A.7 VMX-Fixed Bits in CR0".
      Fixed0 := CPU.Get_MSR64 (Register => Constants.IA32_VMX_CR0_FIXED0);
      Fixed1 := CPU.Get_MSR64 (Register => Constants.IA32_VMX_CR0_FIXED1);
      Ctx.CR0_Valid := Fixed_Valid
        (Register => CR0,
         Fixed0   => Fixed0,
         Fixed1   => Fixed1);
      pragma Debug
        (not Ctx.CR0_Valid, KC.Put_Line (Item => "Init: CR0 is invalid"));

      --D @Item List => impl_kernel_init_check_state_steps
      --D Check that the current processor operating mode meets the required CR4
      --D fixed bits, see Intel SDM Vol. 3D, "A.8 VMX-Fixed Bits in CR4".
      Fixed0 := CPU.Get_MSR64 (Register => Constants.IA32_VMX_CR4_FIXED0);
      Fixed1 := CPU.Get_MSR64 (Register => Constants.IA32_VMX_CR4_FIXED1);
      Ctx.CR4_Valid := Fixed_Valid
        (Register => Bitops.Bit_Set
           (Value => CR4,
            Pos   => Constants.CR4_VMXE_FLAG),
         Fixed0   => Fixed0,
         Fixed1   => Fixed1);
      pragma Debug (not Ctx.CR4_Valid, KC.Put_Line
                    (Item => "Init: CR4 is invalid"));

      --D @Item List => impl_kernel_init_check_state_steps
      --D Check that the current processor supports x2APIC.
      Ctx.Apic_Support := Has_X2_Apic;
      pragma Debug
        (not Ctx.Apic_Support,
         KC.Put_Line (Item => "Init: Local x2APIC not present"));

      --D @Item List => impl_kernel_init_check_state_steps
      --D Check that the current processor has Invariant TSC, see Intel SDM Vol.
      --D 3B, "17.17.1 Invariant TSC".
      Ctx.Invariant_TSC := Has_Invariant_TSC;
      pragma Debug
        (not Ctx.Invariant_TSC,
         KC.Put_Line (Item => "Init: Invariant TSC not present"));

      Is_Valid := Ctx.VMX_Support   and
        Ctx.Not_VMX_Disabled_Locked and
        Ctx.Protected_Mode          and
        Ctx.Paging                  and
        Ctx.IA_32e_Mode             and
        Ctx.Not_Virtual_8086        and
        Ctx.CR0_Valid               and
        Ctx.CR4_Valid               and
        Ctx.Apic_Support            and
        Ctx.Invariant_TSC;
   end Check_State;

end SK.System_State;
