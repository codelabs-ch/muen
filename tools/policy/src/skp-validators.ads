--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Skp.Validators
is

   --  Validate given memory region.
   procedure Validate_Mem_Region (R : Memory_Region_Type);

   --  Validate memory layout.
   procedure Validate_Mem_Layout (L : Memory_Layout_Type);

   --  Validate MSR.
   procedure Validate_MSR (M : MSR_Type);

   --  Validate MSRs.
   procedure Validate_MSRs (M : MSRs_Type);

   --  Validate hardware specification.
   procedure Validate_Hardware (H : Hardware_Type);

   --  Validate kernel specification.
   procedure Validate_Kernel (K : Kernel_Type);

   --  Validate subjects.
   procedure Validate_Subjects (P : Policy_Type);

   --  Validate scheduling policy.
   procedure Validate_Scheduling (P : Policy_Type);

   --  Validate given policy.
   procedure Validate_Policy (P : Policy_Type);

   Validation_Error : exception;

end Skp.Validators;
