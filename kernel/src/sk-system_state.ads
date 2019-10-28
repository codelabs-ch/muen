--
--  Copyright (C) 2013, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Crash_Audit_Types;

--D @Interface
--D This package provides subprograms to check the state and features of the
--D hardware.
package SK.System_State
is

   --  Check validity of system state and return results.
   procedure Check_State
     (Is_Valid : out Boolean;
      Ctx      : out Crash_Audit_Types.System_Init_Context_Type)
   with
      Global => (Input => X86_64.State);

   --D @Section Id => impl_vmcs_enable_vmx, Label => Enabling VMX, Parent => impl_vmcs, Priority => 0
   --D @Text Section => impl_vmcs_enable_vmx, Priority => 0
   --D Enable the VMX feature if it is not already enabled, e.g. by the BIOS.
   --D To ensure that the VMX feature control MSR
   --D (\texttt{IA32\_FEATURE\_CONTROL}) is setup correctly, this action is
   --D only performed after checking the validity of the overall system state.
   procedure Enable_VMX_Feature
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null);

end SK.System_State;
