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

with SK.CPU_Info;
with SK.Crash_Audit;
with SK.Crash_Audit_Types;

package SK.VTd
is

   --  Check validity of initial VT-d subsystem state.
   procedure Check_State
     (Is_Valid : out Boolean;
      Ctx      : out Crash_Audit_Types.VTd_IOMMU_Status_Array)
   with
      Global => (Input => Skp.IOMMU.State);

   --  Initialize VT-d device isolation.
   procedure Initialize
   with
      Global  => (Input  => CPU_Info.APIC_ID,
                  In_Out => (Crash_Audit.State, Skp.IOMMU.State,
                             X86_64.State)),
      Depends => ((Crash_Audit.State,
                   X86_64.State)  => (CPU_Info.APIC_ID, Crash_Audit.State,
                                      Skp.IOMMU.State, X86_64.State),
                  Skp.IOMMU.State =>+ null);

   --  Process fault reported by IOMMU.
   procedure Process_Fault
   with
      Global => (In_Out => Skp.IOMMU.State);

end SK.VTd;
