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

with Validate;
with Validators.Memory;
with Validators.MSR;
with Validators.Device;
with Validators.Scheduling;

package body Validators
is

   -------------------------------------------------------------------------

   procedure Register_All
   is
   begin
      Validate.Register
        (Validator => Memory.Physical_Memory_References'Access);
      Validate.Register
        (Validator => Memory.VMXON_Region_Presence'Access);
      Validate.Register
        (Validator => Memory.VMXON_Region_Size'Access);
      Validate.Register
        (Validator => Memory.VMXON_In_Lowmem'Access);
      Validate.Register
        (Validator => Memory.VMCS_Region_Presence'Access);
      Validate.Register
        (Validator => Memory.VMCS_Region_Size'Access);
      Validate.Register
        (Validator => Memory.VMCS_In_Lowmem'Access);
      Validate.Register
        (Validator => Memory.Physical_Address_Alignment'Access);
      Validate.Register
        (Validator => Memory.Virtual_Address_Alignment'Access);
      Validate.Register
        (Validator => Memory.Region_Size'Access);
      Validate.Register
        (Validator => MSR.Start_Smaller_End'Access);
      Validate.Register
        (Validator => MSR.Low_Or_High'Access);
      Validate.Register
        (Validator => Device.Physical_Device_References'Access);
      Validate.Register
        (Validator => Device.Physical_IRQ_Uniqueness'Access);
      Validate.Register
        (Validator => Device.Physical_IRQ_References'Access);
      Validate.Register
        (Validator => Device.IRQ_Number_Equality'Access);
      Validate.Register
        (Validator => Device.IO_Port_Start_Smaller_End'Access);
      Validate.Register
        (Validator => Device.IO_Port_References'Access);
      Validate.Register
        (Validator => Device.IO_Port_Range_Equality'Access);
      Validate.Register
        (Validator => Scheduling.CPU_Element_Count'Access);
      Validate.Register
        (Validator => Scheduling.Subject_References'Access);
   end Register_All;

end Validators;
