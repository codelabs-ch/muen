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

pragma $Release_Warnings
  (Off, "unit ""Sm_Component.Config"" is not referenced",
   Reason => "Only used to control debug output");
with Sm_Component.Config;
pragma $Release_Warnings
  (On, "unit ""Sm_Component.Config"" is not referenced");

package body Exit_Handlers.CPUID
is

   use Subject_Info;

   -------------------------------------------------------------------------

   procedure Process (Action : out Types.Subject_Action_Type)
   is
      use type SK.Word64;

      RAX : constant SK.Word64 := State.Regs.RAX;
      RCX : constant SK.Word64 := State.Regs.RCX;
   begin
      Action := Types.Subject_Continue;

      case RAX is
         when 0 =>

            --  Get vendor ID.

            --  Return the vendor ID for a GenuineIntel processor and set
            --  the highest valid CPUID number to 2.

            State.Regs.RAX := 7;
            State.Regs.RBX := 16#756e_6547#;
            State.Regs.RCX := 16#6c65_746e#;
            State.Regs.RDX := 16#4965_6e69#;
         when 1 =>

            --  Processor Info and Feature Bits.
            --                            Model IVB
            --                      IVB  / Stepping 9
            --                      /-\ | /
            State.Regs.RAX := 16#0003_06A9#;

            --     CFLUSH size (in 8B)
            --                        \
            State.Regs.RBX := 16#0000_0800#; --  FIXME use real CPU's value

            --  Bit  0 - Streaming SIMD Extensions 3 (SSE3)
            --  Bit  1 - PCLMULQDQ
            --  Bit  9 - Supplemental Streaming SIMD Extensions 3 (SSSE3)
            --  Bit 13 - CMPXCHG16B
            --  Bit 19 - SSE4.1
            --  Bit 20 - SSE4.2
            --  Bit 22 - POPCNT Instruction
            --  Bit 25 - AESNI
            --  Bit 30 - RDRAND
            State.Regs.RCX := 16#4298_2203#;

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
            State.Regs.RDX := 16#0788_a979#;
         when 2 =>

            --  Return Cache and TLB Descriptor information of a Pentium 4
            --  processor (values taken from [1]).
            --
            --  [1] - http://x86.renejeschke.de/html/file_module_x86_id_45.html

            State.Regs.RAX := 16#665b_5001#;
            State.Regs.RBX := 0;
            State.Regs.RCX := 0;
            State.Regs.RDX := 16#007a_7000#;
         when 7 =>

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
         when 16#8000_0000# =>

            --  Get Highest Extended Function Supported.

            State.Regs.RAX := 16#8000_0001#;
            State.Regs.RBX := 0;
            State.Regs.RCX := 0;
            State.Regs.RDX := 0;
         when 16#8000_0001# =>

            --  Get Extended CPU Features

            State.Regs.RAX := 0;
            State.Regs.RBX := 0;
            State.Regs.RCX := 0;

            --  Bit 20 - NX: Execute Disable Bit available
            --  Bit 29 - LM: Long Mode
            State.Regs.RDX := 16#2010_0000#;
         when others =>
            pragma Debug (Sm_Component.Config.Debug_Cpuid,
                          Debug_Ops.Put_Line
                            (Item => "Ignoring unknown CPUID function "
                             & SK.Strings.Img (RAX)));
            State.Regs.RAX := 0;
            State.Regs.RBX := 0;
            State.Regs.RCX := 0;
            State.Regs.RDX := 0;
      end case;
   end Process;

end Exit_Handlers.CPUID;
