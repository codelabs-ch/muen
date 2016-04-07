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

package SK.System_State
is

   --  Check validity of initial system state.
   function Is_Valid return Boolean
   with
      Global => (Input => X86_64.State),
      Volatile_Function;

   --  Enable VMX feature (if disabled). Call this procedure after checking the
   --  validity of the overall system state to make sure the VMX feature
   --  control MSR (IA32_FEATURE_CONTROL) is setup correctly.
   procedure Enable_VMX_Feature
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null);

end SK.System_State;
