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

with SK.Strings;
with SK.Constants;
with SK.Bitops;

with Debug_Ops;

with CPU_Values;

pragma $Release_Warnings
  (Off, "unit ""Sm_Component.Config"" is not referenced",
   Reason => "Only used to control debug output");
with Sm_Component.Config;
pragma $Release_Warnings
  (On, "unit ""Sm_Component.Config"" is not referenced");

package body Exit_Handlers.CPUID
is

   use Subject_Info;

   --  Return size of enabled features in XSAVE area.
   function Get_XSAVE_Area_Size (Features : SK.Byte) return SK.Word32;

   -------------------------------------------------------------------------

   function Get_XSAVE_Area_Size (Features : SK.Byte) return SK.Word32
   is
      use type SK.Word32;

      --  Legacy region + xsave header size, see Intel SDM Vol. 1,
      --  "13.4 XSAVE Area".
      Base_Size : constant := 16#240#;

      Values : CPU_Values.CPUID_Values_Type;
      Calc   : SK.Word32 := 0;
      Res    : Boolean;
   begin
      for I in SK.Word64 range 2 .. 7 loop
         if SK.Bitops.Bit_Test
           (Value => SK.Word64 (Features),
            Pos   => I)
         then
            CPU_Values.Get_CPU_CPUID_Values
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

      CPU_Values.Get_CPU_CPUID_Values
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

      if RAX = 0 then

         --  Cap highest valid CPUID number.

         State.Regs.RAX := 16#d#;

         State.Regs.RBX := SK.Word64 (Values.EBX);
         State.Regs.RCX := SK.Word64 (Values.ECX);
         State.Regs.RDX := SK.Word64 (Values.EDX);
      elsif RAX = 1 then
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
         --  Bit 19 - SSE4.1
         --  Bit 20 - SSE4.2
         --  Bit 22 - POPCNT Instruction
         --  Bit 25 - AESNI
         --  Bit 27 - OSXSAVE
         --  Bit 28 - AVX
         --  Bit 26 - XSAVE
         --  Bit 29 - F16C
         --  Bit 30 - RDRAND
         State.Regs.RCX := SK.Word64 (Values.ECX) and 16#7e98_3203#;

         --  Bit  0 -   FPU: x87 enabled
         --  Bit  3 -   PSE: Page Size Extensions
         --  Bit  4 -   TSC: Time Stamp Counter
         --  Bit  5 -   MSR: RD/WR MSR
         --  Bit  6 -   PAE: PAE and 64bit page tables
         --  Bit  8 -   CX8: CMPXCHG8B Instruction
         --  Bit 11 -   SEP: SYSENTER/SYSEXIT Instructions
         --  Bit 13 -   PGE: Page Global Bit
         --  Bit 15 -  CMOV: Conditional Move Instructions
         --  Bit 19 - CLFSH: CLFLUSH Instruction
         --  Bit 23 -   MMX: MMX support
         --  Bit 24 -  FXSR: FX SAVE/RESTORE
         --  Bit 25 -   SSE: SSE support
         --  Bit 26 -  SSE2: SSE2 support
         State.Regs.RDX := SK.Word64 (Values.EDX) and 16#0788_a979#;
      elsif RAX = 7 then

         --  Structured Extended Feature Flags.

         --  Cap supported subleafs.
         State.Regs.RAX := 0;

         if RCX = 0 then

            --  Sub-leaf 0.

            --  Bit  0 - FSGSBASE
            --  Bit  9 - REP MOVSB/STOSB
            State.Regs.RBX := SK.Word64 (Values.EBX) and 16#0000_0201#;
         else
            State.Regs.RBX := 0;
         end if;

         State.Regs.RCX := 0;
         State.Regs.RDX := 0;
      elsif RAX = 16#d# then
         if RCX = 0 then
            declare
               use type SK.Word32;

               Enabled   : constant SK.Byte
                 := SK.Byte
                   (Values.EAX and SK.Constants.XCR0_Supported_Features_Mask);
               Area_Size : constant SK.Word32
                 := Get_XSAVE_Area_Size (Features => Enabled);
            begin
               State.Regs.RAX := SK.Word64 (Enabled);
               State.Regs.RBX := SK.Word64 (Area_Size);
               State.Regs.RCX := SK.Word64 (Area_Size);
            end;
         elsif RCX = 1 then

            --  Bit  0 - XSAVEOPT
            --  Bit  1 - XSAVEC
            --  Bit  2 - XGETBV
            State.Regs.RAX := 16#0007#;
            State.Regs.RBX := SK.Word64 (Values.EBX);
            State.Regs.RCX := SK.Word64 (Values.ECX);
         else
            State.Regs.RAX := SK.Word64 (Values.EAX);
            State.Regs.RBX := SK.Word64 (Values.EBX);
            State.Regs.RCX := SK.Word64 (Values.ECX);
         end if;

         State.Regs.RDX := SK.Word64 (Values.EDX);
      elsif RAX = 16#8000_0000#  then

         --  Get Highest Extended Function Supported.

         State.Regs.RAX := 16#8000_0004#;
         State.Regs.RBX := 0;
         State.Regs.RCX := 0;
         State.Regs.RDX := 0;
      elsif
        RAX = 2
        or else RAX = 4
        or else RAX = 16#8000_0001#
        or else (RAX >= 16#8000_0002# and then RAX <= 16#8000_0004#)
      then

         --  Passthrough values.

         State.Regs.RAX := SK.Word64 (Values.EAX);
         State.Regs.RBX := SK.Word64 (Values.EBX);
         State.Regs.RCX := SK.Word64 (Values.ECX);
         State.Regs.RDX := SK.Word64 (Values.EDX);
      else
         pragma Debug (Sm_Component.Config.Debug_Cpuid,
                       Debug_Ops.Put_Line
                         (Item => "Ignoring unsupported CPUID leaf "
                          & SK.Strings.Img (RAX)
                          & ", subleaf " & SK.Strings.Img (RCX)));
      end if;
   end Process;

end Exit_Handlers.CPUID;
