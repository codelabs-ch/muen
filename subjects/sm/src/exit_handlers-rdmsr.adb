--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

   use Subject_Info;

   -------------------------------------------------------------------------

   procedure Process (Halt : out Boolean)
   is
      use type SK.Word64;
   begin
      Halt := False;

      case State.Regs.RCX and 16#ffff_ffff# is
         when  16#8b# |
              16#c1# |
              16#c2# |
              16#c3# |
              16#c4# |
              16#c5# |
              16#c6# |
              16#c7# |
              16#c8# |
              16#186# |
              16#187# |
              16#188# |
              16#189# =>
            Subject.Text_IO.Put_String (Item => "RDMSR 16#");
            Subject.Text_IO.Put_Word32
              (Item => SK.Word32 (State.Regs.RCX and 16#ffff_ffff#));
            Subject.Text_IO.Put_Line (Item => "#");
            State.Regs.RAX := State.Regs.RAX and not 16#ffff_ffff#;
            State.Regs.RDX := State.Regs.RDX and not 16#ffff_ffff#;
         when 16#1a0# =>
            Subject.Text_IO.Put_Line (Item => "RDMSR 16#1a0#");
            State.Regs.RAX := 16#1800#;
            State.Regs.RDX := 0;
         when SK.Constants.IA32_KERNEL_GSBASE =>
            State.Regs.RAX := State.Kernel_GS_BASE;
         when others =>
            Subject.Text_IO.Put_Line (Item => "RDMSR");
            Halt := True;
      end case;
   end Process;

end Exit_Handlers.RDMSR;
