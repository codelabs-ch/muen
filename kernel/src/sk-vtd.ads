--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.CPU_Registry;

package SK.VTd
with
   Abstract_State =>
     (State with External => (Async_Writers, Async_Readers, Effective_Writes)),
   Initializes    => State
is

   --  Initialize VT-d device isolation.
   procedure Initialize
   with
      Global  => (Input  => CPU_Registry.State,
                  In_Out => (X86_64.State, State)),
      Depends => (X86_64.State =>+ State,
                  State        =>+ CPU_Registry.State);

   --  Process fault reported by IOMMU.
   procedure Process_Fault
   with
      Global => (In_Out => State);

end SK.VTd;
