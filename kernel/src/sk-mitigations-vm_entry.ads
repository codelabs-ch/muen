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

with X86_64;

--D @Interface
--D This package implements the VM-entry mitigation primitives. It is
--D invoked by the kernel on every scheduling-partition crossing and once
--D before the first subject entry at boot; the primitives compiled in are
--D selected at build time (kernel.gpr), so no configuration-dependent
--D runtime branches exist. Each primitive lives in a child package with
--D its own enabled/disabled body selection.
package SK.Mitigations.VM_Entry
is

   --  Execute the enabled VM-entry mitigation primitives (crossing-gated at
   --  the call site): IBPB branch predictor flush, L1D cache flush and VERW
   --  buffer clear.
   procedure Run
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ X86_64.State);

end SK.Mitigations.VM_Entry;
