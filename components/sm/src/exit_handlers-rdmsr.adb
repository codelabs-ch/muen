--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Subject.Text_IO;

with Subject_Info;

package body Exit_Handlers.RDMSR
is

   use SK.Constants;
   use Subject_Info;

   -------------------------------------------------------------------------

   procedure Process (Halt : out Boolean)
   is
      use type SK.Word64;

      RAX : constant SK.Word64 := State.Regs.RAX;
      RCX : constant SK.Word64 := State.Regs.RCX;
      RDX : constant SK.Word64 := State.Regs.RDX;
      MSR : constant SK.Word32 := SK.Word32 (RCX);
   begin
      Halt := False;

      case MSR is
         when IA32_BIOS_SIGN_ID |
              IA32_PMC0         |
              IA32_PMC1         |
              IA32_PMC2         |
              IA32_PMC3         |
              IA32_PMC4         |
              IA32_PMC5         |
              IA32_PMC6         |
              IA32_PMC7         |
              IA32_PERFEVTSEL0  |
              IA32_PERFEVTSEL1  |
              IA32_PERFEVTSEL2  |
              IA32_PERFEVTSEL3  =>
            pragma Debug (Subject.Text_IO.Put_String (Item => "RDMSR 16#"));
            pragma Debug (Subject.Text_IO.Put_Word32 (Item => MSR));
            pragma Debug (Subject.Text_IO.Put_Line   (Item => "#"));
            State.Regs.RAX := RAX and not 16#ffff_ffff#;
            State.Regs.RDX := RDX and not 16#ffff_ffff#;
         when IA32_MISC_ENABLE =>

            --  Bit 11: Branch Trace Storage Unavailable
            --  Bit 12: Precise Event Based Sampling (PEBS) Unavailable

            State.Regs.RAX := 16#1800#;
            State.Regs.RDX := 0;
         when others =>
            pragma Debug (Subject.Text_IO.Put_String
                          (Item => "Unhandled read access to MSR 16#"));
            pragma Debug (Subject.Text_IO.Put_Word32 (Item => MSR));
            pragma Debug (Subject.Text_IO.Put_Line   (Item => "#"));
            Halt := True;
      end case;
   end Process;

end Exit_Handlers.RDMSR;
