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

package body Exit_Handlers.WRMSR
is

   use SK.Constants;
   use Subject_Info;

   -------------------------------------------------------------------------

   procedure Process (Halt : out Boolean)
   is
      use type SK.Word64;

      MSR : constant SK.Word32 := SK.Word32 (State.Regs.RCX);
   begin
      Halt := False;

      case MSR is
         when IA32_BIOS_SIGN_ID |
              IA32_PMC0         =>
            Subject.Text_IO.Put_String (Item => "WRMSR 16#");
            Subject.Text_IO.Put_Word32 (Item => MSR);
            Subject.Text_IO.Put_String (Item => "#: ");
            Subject.Text_IO.Put_Word32
              (Item => SK.Word32 (State.Regs.RDX and 16#ffff_ffff#));
            Subject.Text_IO.Put_String (Item => ":");
            Subject.Text_IO.Put_Word32
              (Item => SK.Word32 (State.Regs.RAX and 16#ffff_ffff#));
            Subject.Text_IO.New_Line;
         when SK.Constants.IA32_KERNEL_GSBASE =>
            State.Kernel_GS_BASE := State.Regs.RAX;
         when others =>
            Subject.Text_IO.Put_String
              (Item => "Unhandled write access to MSR 16#");
            Subject.Text_IO.Put_Word32 (Item => MSR);
            Subject.Text_IO.Put_Line (Item => "#");
            Halt := True;
      end case;
   end Process;

end Exit_Handlers.WRMSR;
