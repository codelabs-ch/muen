--
--  Copyright (C) 2013-2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

generic

   --  Implementation of the New_Line output operation.
   with procedure Output_New_Line;

   --  Implementation of the Put_Line output operation.
   with procedure Output_Put_Line (Item : String);

package SK.Dump_ISR
is

   --  Output ISR execution environment state.
   procedure Output_ISR_State
     (Context       : Isr_Context_Type;
      APIC_ID       : Byte;
      CR0, CR3, CR4 : Word64);

   --  Output CPU registers.
   procedure Output_Registers
     (Regs : CPU_Registers_Type;
      RIP, CS, RFL, RSP, SS, CR0, CR3, CR4 : Word64);

end SK.Dump_ISR;
