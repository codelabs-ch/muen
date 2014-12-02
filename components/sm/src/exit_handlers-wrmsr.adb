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

with Debug_Ops;

package body Exit_Handlers.WRMSR
is

   use SK.Constants;
   use Subject_Info;

   -------------------------------------------------------------------------

   procedure Process (Halt : out Boolean)
   is
      use type SK.Word64;

      RCX : constant SK.Word64 := State.Regs.RCX;
      MSR : constant SK.Word32 := SK.Word32'Mod (RCX);
   begin
      Halt := False;

      case MSR is
         when IA32_BIOS_SIGN_ID | IA32_PMC0 => null;
         when others =>
            pragma Debug (Debug_Ops.Put_Value32
                          (Message => "Unhandled write access to MSR",
                           Value   => MSR));
            Halt := True;
      end case;
   end Process;

end Exit_Handlers.WRMSR;
