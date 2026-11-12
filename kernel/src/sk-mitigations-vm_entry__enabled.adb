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

with SK.Mitigations.VM_Entry.IBPB;
with SK.Mitigations.VM_Entry.L1D_Flush;
with SK.Mitigations.VM_Entry.Verw;

package body SK.Mitigations.VM_Entry
is

   -------------------------------------------------------------------------

   --  Execute the enabled VM-entry mitigation primitives (crossing-gated at
   --  the call site): IBPB branch predictor flush, L1D cache flush and VERW
   --  buffer clear.
   procedure Run
   is
   begin
      IBPB.Flush;
      L1D_Flush.Flush;
      Verw.Clear;
   end Run;

end SK.Mitigations.VM_Entry;
