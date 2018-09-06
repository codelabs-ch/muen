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

with SK.Constants;
with SK.Bitops;

with CPU_Values;

pragma $Release_Warnings
  (Off, "unit * is not referenced",
   Reason => "Only used for debug output");
with SK.Strings;
with Debug_Ops;
with Sm_Component.Config;
pragma $Release_Warnings (On, "unit * is not referenced");

package body Exit_Handlers.CPUID
is

   use Subject_Info;

   --  Return size of enabled features in XSAVE area. The Features parameter
   --  describes a state-component bitmap, see Intel SDM Vol. 1,
   --  "13.1 XSAVE-Supported Features and State-Component Bitmaps".
   function Get_XSAVE_Area_Size (Features : SK.Word64) return SK.Word32;

   --  Return state-component bitmap of enabled XSAVE-Supported features.
   --  Note: This should consider the current XCR0 value. However, since this is
   --        currently not accessible, we can assume, that Linux enables all
   --        XSAVE-Supported features which are reported by us.
   function Get_Enabled_XSAVE_Features return SK.Word64;

   -------------------------------------------------------------------------

   function Get_Enabled_XSAVE_Features return SK.Word64
   is
      use type SK.Word32;

      Feature_Bitmap : SK.Word64 := 0;
      CPU_Features   : CPU_Values.CPUID_Values_Type;
      Success        : Boolean;
   begin
      CPU_Values.Get_CPUID_Values
        (Leaf    => 16#d#,
         Subleaf => 0,
         Result  => CPU_Features,
         Success => Success);
      if Success then
         Feature_Bitmap := SK.Word64
           (CPU_Features.EAX and SK.Constants.XCR0_Supported_Features_Mask);
      end if;

      return Feature_Bitmap;
   end Get_Enabled_XSAVE_Features;

   -------------------------------------------------------------------------

   function Get_XSAVE_Area_Size (Features : SK.Word64) return SK.Word32
   is
      use type SK.Word32;

      --  Legacy region + xsave header size, see Intel SDM Vol. 1,
      --  "13.4 XSAVE Area".
      Base_Size : constant := 16#240#;

      Values : CPU_Values.CPUID_Values_Type;
      Calc   : SK.Word32 := 0;
      Res    : Boolean;
   begin
      for I in SK.Bitops.Word64_Pos range 2 .. SK.Bitops.Word64_Pos'Last loop
         if SK.Bitops.Bit_Test
           (Value => Features,
            Pos   => I)
         then
            CPU_Values.Get_CPUID_Values
              (Leaf    => 16#d#,
               Subleaf => SK.Byte (I),
               Result  => Values,
               Success => Res);
            if Res then
               Calc := Calc + Values.EAX;
            end if;
         end if;
      end loop;

      return Base_Size + Calc;
   end Get_XSAVE_Area_Size;

   -------------------------------------------------------------------------

   procedure Process (Action : out Types.Subject_Action_Type)
   is
      use type SK.Word64;

      --D @Lst Smcpuidbegin
      RAX : constant SK.Word64 := State.Regs.RAX;
      RCX : constant SK.Word64 := State.Regs.RCX;

      Values : CPU_Values.CPUID_Values_Type;
      Res    : Boolean;
   begin
      Action         := Types.Subject_Continue;
      State.Regs.RAX := 0;
      State.Regs.RBX := 0;
      State.Regs.RCX := 0;
      State.Regs.RDX := 0;

      CPU_Values.Get_CPUID_Values
        (Leaf    => SK.Word32'Mod (RAX),
         Subleaf => SK.Byte'Mod (RCX),
         Result  => Values,
         Success => Res);
      if not Res then
         pragma Debug (Sm_Component.Config.Debug_Cpuid,
                       Debug_Ops.Put_Line
                         (Item => "Ignoring unknown CPUID leaf "
                          & SK.Strings.Img (RAX)
                          & ", subleaf " & SK.Strings.Img (RCX)));
         return;
      end if;

      case RAX is
         when 0 =>

            --  Cap highest valid CPUID number.

            State.Regs.RAX := 16#d#;

            State.Regs.RBX := SK.Word64 (Values.EBX);
            State.Regs.RCX := SK.Word64 (Values.ECX);
            State.Regs.RDX := SK.Word64 (Values.EDX);
         when 1 =>
            --D @Lst Smcpuidend

            State.Regs.RAX := SK.Word64 (Values.EAX);

            -- Bits 07..00 - Brand Index
            -- Bits 15..08 - CLFLUSH line size
            State.Regs.RBX := SK.Word64 (Values.EBX) and 16#ffff#;

            --  Bit  0 - Streaming SIMD Extensions 3 (SSE3)
            --  Bit  1 - PCLMULQDQ
            --  Bit  9 - Supplemental Streaming SIMD Extensions 3 (SSSE3)
            --  Bit 12 - FMA
            --  Bit 13 - CMPXCHG16B
            --  Bit 17 - PCID
            --  Bit 19 - SSE4.1
            --  Bit 20 - SSE4.2
            --  Bit 22 - MOVBE
            --  Bit 23 - POPCNT Instruction
            --  Bit 25 - AESNI
            --  Bit 27 - OSXSAVE
            --  Bit 28 - AVX
            --  Bit 26 - XSAVE
            --  Bit 29 - F16C
            --  Bit 30 - RDRAND
            State.Regs.RCX := SK.Word64 (Values.ECX) and 16#7eda_3203#;

            --  Bit  0 -   FPU: x87 enabled
            --  Bit  1 -   VME: Virtual-8086 Mode Enhancement
            --  Bit  2 -    DE: Debugging Extensions
            --  Bit  3 -   PSE: Page Size Extensions
            --  Bit  4 -   TSC: Time Stamp Counter
            --  Bit  5 -   MSR: RD/WR MSR
            --  Bit  6 -   PAE: PAE and 64bit page tables
            --  Bit  8 -   CX8: CMPXCHG8B Instruction
            --  Bit 11 -   SEP: SYSENTER/SYSEXIT Instructions
            --  Bit 12 -  MTRR: Memory Type Range Registers
            --  Bit 13 -   PGE: Page Global Bit
            --  Bit 15 -  CMOV: Conditional Move Instructions
            --  Bit 17 - PSE36: 36-Bit Page Size Extension
            --  Bit 19 - CLFSH: CLFLUSH Instruction
            --  Bit 23 -   MMX: MMX support
            --  Bit 24 -  FXSR: FX SAVE/RESTORE
            --  Bit 25 -   SSE: SSE support
            --  Bit 26 -  SSE2: SSE2 support
            --  Bit 27 -    SS: Self Snoop
            State.Regs.RDX := SK.Word64 (Values.EDX) and 16#0f8a_b97f#;
         when 4 =>

            --  Mask out APIC ID information. Otherwise Linux deduces topology
            --  from this information.

            State.Regs.RAX := SK.Word64 (Values.EAX) and 16#3ff#;
            State.Regs.RBX := SK.Word64 (Values.EBX);
            State.Regs.RCX := SK.Word64 (Values.ECX);
            State.Regs.RCX := SK.Word64 (Values.ECX);
         when 7 =>

            --  Structured Extended Feature Flags.

            if RCX = 0 then
               --  Cap supported subleaves.
               State.Regs.RAX := 1;

               --  Bit  0 - FSGSBASE
               --  Bit  3 - BMI1
               --  Bit  5 - AVX2
               --  Bit  7 - SMEP
               --  Bit  8 - BMI2
               --  Bit  9 - REP MOVSB/STOSB
               --  Bit 10 - INVPCID
               --  Bit 16 - AVX512F
               --  Bit 17 - AVX512DQ
               --  Bit 18 - RDSEED
               --  Bit 19 - ADX
               --  Bit 20 - SMAP
               --  Bit 21 - AVX512_IFMA
               --  Bit 23 - CLFLUSHOPT
               --  Bit 28 - AVX512CD
               --  Bit 29 - SHA
               --  Bit 30 - AVX512BW
               --  Bit 31 - AVX512VL
               State.Regs.RBX := SK.Word64 (Values.EBX) and 16#f0bf_07a9#;

               --  Bit  1 - AVX512_VBMI
               --  Bit  6 - AVX512_VBMI2
               --  Bit  8 - GFNI
               --  Bit  9 - VAES
               --  Bit 10 - VPCLMULQDQ
               --  Bit 11 - AVX512_VNNI
               --  Bit 12 - AVX512_BITALG
               --  Bit 14 - AVX512_VPOPCNTDQ
               --  Bit 27 - MOVDIRI
               --  Bit 28 - MOVDIRI64B
               State.Regs.RCX := SK.Word64 (Values.ECX) and 16#1800_5f42#;

               --  Bit  4 - Fast Short REP MOV
               --  Bit  8 - AVX512_VP2INTERSECT
               --  Bit 14 - SERIALIZE
               --  Bit 23 - AVX512_FP16
               State.Regs.RDX := SK.Word64 (Values.EDX) and 16#0080_4110#;
            elsif RCX = 1 then

               --  Bit  4 - AVX-VNNI
               --  Bit  5 - AVX512_BF16
               --  Bit 10 - zero-length REB MOVSB
               --  Bit 11 - fast short REB STOSB
               --  Bit 12 - fast short REB CMPSB, REP SCASB
               State.Regs.RAX := SK.Word64 (Values.EAX) and 16#0000_1c30#;
               State.Regs.RBX := 0;
               State.Regs.RCX := 0;
               State.Regs.RDX := 0;
            else
               State.Regs.RAX := 0;
               State.Regs.RBX := 0;
               State.Regs.RCX := 0;
               State.Regs.RDX := 0;
            end if;
         when 16#d# =>
            if RCX = 0 then
               declare
                  use type SK.Word32;

                  Enabled   : constant SK.Word64 := SK.Word64
                    (Values.EAX and SK.Constants.XCR0_Supported_Features_Mask);
                  Area_Size : constant SK.Word32
                    := Get_XSAVE_Area_Size (Features => Enabled);
               begin
                  State.Regs.RAX := Enabled;
                  State.Regs.RBX := SK.Word64 (Area_Size);
                  State.Regs.RCX := SK.Word64 (Area_Size);
                  State.Regs.RDX := SK.Word64 (Values.EDX);
               end;
            elsif RCX = 1 then

               --  Bit  0 - XSAVEOPT
               --  Bit  1 - XSAVEC
               --  Bit  2 - XGETBV
               State.Regs.RAX := SK.Word64 (Values.EAX) and 16#0007#;
               State.Regs.RBX := SK.Word64
                 (Get_XSAVE_Area_Size (Features => Get_Enabled_XSAVE_Features));

               --  Mask out IA32_XSS related values as it is currently not
               --  supported.

               State.Regs.RCX := 0;
               State.Regs.RDX := 0;
            else
               if RCX <= SK.Bitops.Word64_Pos'Last then

                  --  Nesting of the conditional is necessary, since GNATprove
                  --  does not yet support "and then" and thus does not carry
                  --  over the RCX upper bound to the conversion.

                  if SK.Bitops.Bit_Test
                    (Value => Get_Enabled_XSAVE_Features,
                     Pos   => SK.Bitops.Word64_Pos (RCX))
                  then
                     State.Regs.RAX := SK.Word64 (Values.EAX);
                     State.Regs.RBX := SK.Word64 (Values.EBX);
                     State.Regs.RCX := SK.Word64 (Values.ECX);
                     State.Regs.RDX := SK.Word64 (Values.EDX);
                  end if;
               end if;
            end if;

         when 16#8000_0000# =>

            --  Get Highest Extended Function Supported.

            State.Regs.RAX := 16#8000_0008#;
            State.Regs.RBX := 0;
            State.Regs.RCX := 0;
            State.Regs.RDX := 0;
         when 16#8000_0001# =>
            State.Regs.RAX := SK.Word64 (Values.EAX);
            State.Regs.RBX := SK.Word64 (Values.EBX);
            State.Regs.RCX := SK.Word64 (Values.ECX);

            --  Mask out Bit 27 - RDTSCP
            State.Regs.RDX := SK.Word64 (Values.EDX) and 16#f7ff_ffff#;
         when 16#8000_0007# =>

            --  Clear to zero in order to not provide CPU Size/hierarchy
            --  information to Linux subjects.

            State.Regs.RAX := 0;
            State.Regs.RBX := 0;
            State.Regs.RCX := 0;
            State.Regs.RDX := 0;
         when 16#8000_0008# =>

            --  Physical Address and Linear Address Size
            State.Regs.RAX := SK.Word64 (Values.EAX);
            State.Regs.RBX := 0;
            State.Regs.RCX := 0;
            State.Regs.RDX := 0;
         when 2 | 16#8000_0002# .. 16#8000_0006# =>

            --  Passthrough values.

            State.Regs.RAX := SK.Word64 (Values.EAX);
            State.Regs.RBX := SK.Word64 (Values.EBX);
            State.Regs.RCX := SK.Word64 (Values.ECX);
            State.Regs.RDX := SK.Word64 (Values.EDX);
         when others =>
            pragma Debug (Sm_Component.Config.Debug_Cpuid,
                          Debug_Ops.Put_Line
                            (Item => "Ignoring unsupported CPUID leaf "
                             & SK.Strings.Img (RAX)
                             & ", subleaf " & SK.Strings.Img (RCX)));
      end case;
   end Process;

end Exit_Handlers.CPUID;
