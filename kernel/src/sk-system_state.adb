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
      MSR_Feature_Control := CPU.Get_MSR64
        (Register => Constants.IA32_FEATURE_CONTROL);

      if not Bitops.Bit_Test
        (Value => MSR_Feature_Control,
         Pos   => Constants.IA32_FCTRL_LOCKED_FLAG)
      then

         --  Explicitly disable 'VMX in SMX operation'.

         MSR_Feature_Control := Bitops.Bit_Clear
           (Value => MSR_Feature_Control,
            Pos   => Constants.IA32_FCTRL_VMX_IN_SMX_FLAG);

         --  Enable 'VMX outside SMX operation'.

         MSR_Feature_Control := Bitops.Bit_Set
           (Value => MSR_Feature_Control,
            Pos   => Constants.IA32_FCTRL_VMX_FLAG);

         --  Lock MSR.

         MSR_Feature_Control := Bitops.Bit_Set
           (Value => MSR_Feature_Control,
            Pos   => Constants.IA32_FCTRL_LOCKED_FLAG);

         CPU.Write_MSR64 (Register => Constants.IA32_FEATURE_CONTROL,
                          Value    => MSR_Feature_Control);
      end if;
   end Enable_VMX_Feature;

   -------------------------------------------------------------------------

   --  Check required VMX-fixed bits of given register, see Intel SDM 3C,
   --  appendix A.7 & A.8.
   function Fixed_Valid
     (Register : SK.Word64;
      Fixed0   : SK.Word64;
      Fixed1   : SK.Word64)
      return Boolean
   is
   begin
      return
        ((Fixed0 or  Register) = Register) and
        ((Fixed1 and Register) = Register);
   end Fixed_Valid;

   -------------------------------------------------------------------------

   --  Returns True if an invariant TSC is present, see Intel SDM Vol. 3B,
   --  chapter 17.13.1.
   function Has_Invariant_TSC return Boolean
   with
      Volatile_Function,
      Global => (Input => X86_64.State)
   is
      Unused_EAX, Unused_EBX, Unused_ECX, EDX : SK.Word32;
   begin
      Unused_EAX := 16#8000_0007#;
      Unused_ECX := 0;

      pragma Warnings
        (GNATprove, Off, "unused assignment to ""Unused_E*X""",
         Reason => "Only parts of the CPUID result is needed");
      CPU.CPUID
        (EAX => Unused_EAX,
         EBX => Unused_EBX,
         ECX => Unused_ECX,
         EDX => EDX);
      pragma Warnings (GNATprove, On, "unused assignment to ""Unused_E*X""");

      return Bitops.Bit_Test
        (Value => Word64 (EDX),
         Pos   => Constants.CPUID_FEATURE_INVARIANT_TSC);
   end Has_Invariant_TSC;

   -------------------------------------------------------------------------

   --  Returns True if local APIC is present and supports x2APIC mode, see
   --  Intel SDM 3A, chapters 10.4.2 and 10.12.1.
   function Has_X2_Apic return Boolean
   with
      Volatile_Function,
      Global => (Input => X86_64.State)
   is
      Unused_EAX, Unused_EBX, ECX, EDX : Word32;
   begin
      Unused_EAX := 1;
      ECX        := 0;

      pragma Warnings (GNATprove, Off, "unused assignment to ""Unused_E*X""",
         Reason => "Only parts of the CPUID result is needed");
      CPU.CPUID
        (EAX => Unused_EAX,
         EBX => Unused_EBX,
         ECX => ECX,
         EDX => EDX);
      pragma Warnings (GNATprove, On, "unused assignment to ""Unused_E*X""");

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

      pragma Warnings (GNATprove, Off, "unused assignment to ""Unused_E*X""",
         Reason => "Only parts of the CPUID result is needed");
      CPU.CPUID
        (EAX => Unused_EAX,
         EBX => Unused_EBX,
         ECX => ECX,
         EDX => Unused_EDX);
      pragma Warnings (GNATprove, On, "unused assignment to ""Unused_E*X""");

      return Bitops.Bit_Test
        (Value => Word64 (ECX),
         Pos   => Constants.CPUID_FEATURE_VMX_FLAG);
   end Has_VMX_Support;

   -------------------------------------------------------------------------

   procedure Check_State
     (Is_Valid : out Boolean;
      Ctx      : out Crash_Audit_Types.System_Init_Context_Type)
   is
      MSR_Feature_Control : SK.Word64;

      CR0       : constant Word64 := CPU.Get_CR0;
      CR4       : constant Word64 := CPU.Get_CR4;
      RFLAGS    : constant Word64 := CPU.Get_RFLAGS;
      IA32_EFER : constant Word64 := CPU.Get_MSR64
        (Register => Constants.IA32_EFER);
   begin
      Ctx := Crash_Audit_Types.Null_System_Init_Context;

      Ctx.VMX_Support := Has_VMX_Support;
      pragma Debug
        (not Ctx.VMX_Support, KC.Put_Line (Item => "Init: VMX not supported"));

      MSR_Feature_Control := CPU.Get_MSR64
        (Register => Constants.IA32_FEATURE_CONTROL);
      Ctx.VMX_Not_Dislocked := not
        (Bitops.Bit_Test
           (Value => MSR_Feature_Control,
            Pos   => Constants.IA32_FCTRL_LOCKED_FLAG)
         and then not Bitops.Bit_Test
           (Value => MSR_Feature_Control,
            Pos   => Constants.IA32_FCTRL_VMX_FLAG));
      pragma Debug
        (not Ctx.VMX_Not_Dislocked,
         KC.Put_Line (Item => "Init: VMX disabled by BIOS"));

      Ctx.Protected_Mode := Bitops.Bit_Test
        (Value => CR0,
         Pos   => Constants.CR0_PE_FLAG);
      pragma Debug
        (not Ctx.Protected_Mode,
         KC.Put_Line (Item => "Init: Protected mode not enabled"));

      Ctx.Paging := Bitops.Bit_Test
        (Value => CR0,
         Pos   => Constants.CR0_PG_FLAG);
      pragma Debug
        (not Ctx.Paging, KC.Put_Line (Item => "Init: Paging not enabled"));

      Ctx.IA_32e_Mode := Bitops.Bit_Test
        (Value => IA32_EFER,
         Pos   => Constants.IA32_EFER_LMA_FLAG);
      pragma Debug
        (not Ctx.IA_32e_Mode, KC.Put_Line
           (Item => "Init: IA-32e mode not enabled"));

      Ctx.Not_Virtual_8086 := not Bitops.Bit_Test
        (Value => RFLAGS,
         Pos   => Constants.RFLAGS_VM_FLAG);
      pragma Debug
        (not Ctx.Not_Virtual_8086,
         KC.Put_Line (Item => "Init: Virtual-8086 mode enabled"));

      Ctx.CR0_Valid := Fixed_Valid
        (Register => CR0,
         Fixed0   => CPU.Get_MSR64
           (Register => Constants.IA32_VMX_CR0_FIXED0),
         Fixed1   => CPU.Get_MSR64
           (Register => Constants.IA32_VMX_CR0_FIXED1));
      pragma Debug
        (not Ctx.CR0_Valid, KC.Put_Line (Item => "Init: CR0 is invalid"));

      Ctx.CR4_Valid := Fixed_Valid
        (Register => Bitops.Bit_Set
           (Value => CR4,
            Pos   => Constants.CR4_VMXE_FLAG),
         Fixed0   => CPU.Get_MSR64
           (Register => Constants.IA32_VMX_CR4_FIXED0),
         Fixed1   => CPU.Get_MSR64
           (Register => Constants.IA32_VMX_CR4_FIXED1));
      pragma Debug (not Ctx.CR4_Valid, KC.Put_Line
                    (Item => "Init: CR4 is invalid"));

      Ctx.Apic_Support := Has_X2_Apic;
      pragma Debug
        (not Ctx.Apic_Support,
         KC.Put_Line (Item => "Init: Local x2APIC not present"));

      Ctx.Invariant_TSC := Has_Invariant_TSC;
      pragma Debug
        (not Ctx.Invariant_TSC,
         KC.Put_Line (Item => "Init: Invariant TSC not present"));

      Is_Valid := Ctx.VMX_Support   and
        Ctx.VMX_Not_Dislocked       and
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
