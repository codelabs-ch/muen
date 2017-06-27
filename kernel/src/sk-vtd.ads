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

with Skp.IOMMU;

package SK.VTd
is

   --  Check validity of initial VT-d subsystem state.
   procedure Check_State (Is_Valid : out Boolean)
   with
      Global => (Input => Skp.IOMMU.State);

   --  Initialize VT-d device isolation.
   procedure Initialize
   with
      Global  => (In_Out => (X86_64.State, Skp.IOMMU.State)),
      Depends => (X86_64.State    =>+ Skp.IOMMU.State,
                  Skp.IOMMU.State =>+ null);

   --  Process fault reported by IOMMU.
   procedure Process_Fault
   with
      Global => (In_Out => Skp.IOMMU.State);

end SK.VTd;
