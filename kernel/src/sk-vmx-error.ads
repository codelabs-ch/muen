--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Crash_Audit;
with SK.Crash_Audit_Types;

--  Allocate crash audit entry for given VMX error and trigger system restart.
procedure SK.VMX.Error
  (Reason           : Crash_Audit_Types.VTx_Reason_Range;
   VMCS_Addr_Req    : Word64 := Crash_Audit_Types.VTx_Ctx_Noaddr;
   VMCS_Field       : Word16 := Crash_Audit_Types.VTx_Ctx_Nofield;
   VMCS_Field_Value : Word64 := 0)
with
   Global => (Input  => CPU_Info.APIC_ID,
              In_Out => (Crash_Audit.State, X86_64.State)),
   No_Return;
