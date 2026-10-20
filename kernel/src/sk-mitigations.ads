--
--  Copyright (C) 2026  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2026  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with X86_64;

--D @Interface
--D This package implements the transient-execution mitigation policy checks
--D of the kernel at boot (the MCU revision floor, and later the ARCH_CAP
--D check and the Ada/SPARK mitigation primitives). It is compiled in only
--D when the mitigation policy is enabled (<mitigations enable="...">); the
--D disabled variant is a null body.
package SK.Mitigations
is

   --  Verify the microcode revision floor required by the mitigation policy
   --  (<mcu minRevision>). Halts if the given loaded revision is below the
   --  floor; this check runs on every core after the MCU load.
   procedure Check_MCU_Revision (Revision : Interfaces.Unsigned_32)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State => X86_64.State,
                  null         => Revision);

end SK.Mitigations;
