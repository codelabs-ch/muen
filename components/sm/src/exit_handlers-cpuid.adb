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

with Debug_Ops;

with CPU_Values;
with Sm_Component.Config;

package body Exit_Handlers.CPUID
is

   use Subject_Info;

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
         --  Bit 13 - CMPXCHG16B
         --  Bit 19 - SSE4.1
         --  Bit 20 - SSE4.2
         --  Bit 22 - POPCNT Instruction
         --  Bit 25 - AESNI
         --  Bit 30 - RDRAND
         State.Regs.RCX := SK.Word64 (Values.ECX) and 16#4298_2203#;

         --  TODO: remove, announce if present
         pragma Warnings (Off);
         if Sm_Component.Config.Sm_Announce_Xsave then
            pragma Warnings (On);
            declare
               Cur_RCX : constant SK.Word64 := State.Regs.RCX;
            begin
               --  Bit 26 - XSAVE
               --  Bit 27 - OSXSAVE
               State.Regs.RCX := Cur_RCX or 16#0c00_0000#;
            end;
         end if;

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
      elsif RAX = 2 then
         State.Regs.RAX := SK.Word64 (Values.EAX);
         State.Regs.RBX := SK.Word64 (Values.EBX);
         State.Regs.RCX := SK.Word64 (Values.ECX);
         State.Regs.RDX := SK.Word64 (Values.EDX);
      elsif RAX = 7 then

         --  Structured Extended Feature Flags.

         --  Max supported subleaf is 0.
         State.Regs.RAX := 0;

         if RCX = 0 then

            --  Sub-leaf 0.

            --  Bit  0 - FSGSBASE
            --  Bit  9 - REP MOVSB/STOSB
            State.Regs.RBX := 16#0000_0201#;
         else
            State.Regs.RBX := 0;
         end if;

         State.Regs.RCX := 0;
         State.Regs.RDX := 0;
      elsif RAX = 16#d# then
         State.Regs.RAX := 0;
         State.Regs.RBX := 0;
         State.Regs.RCX := 0;
         State.Regs.RDX := 0;

         pragma Warnings (Off);
         if Sm_Component.Config.Sm_Announce_Xsave then
            pragma Warnings (On);
            if RCX = 0 then

               --  Bit  0 - x87 state
               --  Bit  1 - SSE state
               State.Regs.RAX := 16#0003#;
               State.Regs.RBX := 16#0240#;
               State.Regs.RCX := 16#0240#;
               State.Regs.RDX := 0;
            elsif RCX = 1 then

               --  Bit  0 - XSAVEOPT
               --  Bit  1 - XSAVEC
               --  Bit  2 - XGETBV
               State.Regs.RAX := 16#0007#;
               State.Regs.RBX := 16#0240#;
               State.Regs.RCX := 16#0000#;
               State.Regs.RDX := 0;
            end if;
         end if;
      elsif RAX = 16#8000_0000#  then

         --  Get Highest Extended Function Supported.

         State.Regs.RAX := 16#8000_0004#;
         State.Regs.RBX := 0;
         State.Regs.RCX := 0;
         State.Regs.RDX := 0;
      elsif RAX = 16#8000_0001# then

         --  Get Extended CPU Features

         State.Regs.RAX := 0;
         State.Regs.RBX := 0;
         State.Regs.RCX := 0;

         --  Bit 20 - NX: Execute Disable Bit available
         --  Bit 29 - LM: Long Mode
         State.Regs.RDX := 16#2010_0000#;
      elsif RAX >= 16#8000_0002# and then RAX <= 16#8000_0004# then
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
