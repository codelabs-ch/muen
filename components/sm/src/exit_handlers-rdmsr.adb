--
--  Copyright (C) 2013-2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Debug_Ops;

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
      MSR : constant SK.Word32 := SK.Word32'Mod (RCX);
   begin
      Halt := False;

      case MSR is
         when IA32_MISC_ENABLE =>

            --  Bit  0: Fast string operations
            --  Bit 11: Branch Trace Storage Unavailable
            --  Bit 12: Precise Event Based Sampling (PEBS) Unavailable

            State.Regs.RAX := 16#1801#;
            State.Regs.RDX := 0;
         when others =>
            pragma Debug (Debug_Ops.Put_Value32
                          (Message => "RDMSR",
                           Value   => MSR));
            State.Regs.RAX := RAX and not 16#ffff_ffff#;
            State.Regs.RDX := RDX and not 16#ffff_ffff#;
      end case;
   end Process;

end Exit_Handlers.RDMSR;
