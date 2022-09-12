--
--  Copyright (C) 2022  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2022  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

--D @Interface
--D Package providing types for x86 exception handling/interrupt service
--D routines.
package SK.Exceptions
is

   Isr_Ctx_Size : constant := CPU_Regs_Size + 7 * 8;

   --D @Interface
   --D Interrupt Service Routine execution environment state.
   type Isr_Context_Type is record
      --D @Interface
      --D Value of CPU registers on interrupt occurrence.
      Regs       : CPU_Registers_Type;
      --D @Interface
      --D Interrupt vector number.
      Vector     : Interfaces.Unsigned_64;
      --D @Interface
      --D Interrupt error code for interrupts that have an error code, see
      --D Intel SDM Vol. 3A, "6.13 Error Code".
      Error_Code : Interfaces.Unsigned_64;
      --D @Interface
      --D Instruction pointer value on interrupt occurrence.
      RIP        : Interfaces.Unsigned_64;
      --D @Interface
      --D Code Segment value on interrupt occurrence.
      CS         : Interfaces.Unsigned_64;
      --D @Interface
      --D RFLAGS status register value on interrupt occurrence.
      RFLAGS     : Interfaces.Unsigned_64;
      --D @Interface
      --D Stack pointer value on interrupt occurrence.
      RSP        : Interfaces.Unsigned_64;
      --D @Interface
      --D Stack Segment value on interrupt occurrence.
      SS         : Interfaces.Unsigned_64;
   end record
   with
      Pack,
      Size => Isr_Ctx_Size * 8;

   Null_Isr_Context : constant Isr_Context_Type;

private

   Null_Isr_Context : constant Isr_Context_Type
     := (Regs   => Null_CPU_Regs,
         others => 0);

end SK.Exceptions;
