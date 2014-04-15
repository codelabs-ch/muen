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

with Skp;

package SK.Dump
is

   --  Print CPU registers.
   procedure Print_Registers
     (GPR : CPU_Registers_Type;
      RIP : Word64; CS  : Word64; RFL : Word64; RSP : Word64; SS  : Word64;
      CR0 : Word64; CR2 : Word64; CR3 : Word64; CR4 : Word64);
   pragma Inline_Always (Print_Registers);

   --  Print ISR execution environment state.
   procedure Print_ISR_State (Context : Isr_Context_Type);
   pragma Inline_Always (Print_ISR_State);

   --  Print subject exit information including the whole subject state.
   procedure Print_Subject (Subject_Id : Skp.Subject_Id_Type);
   pragma Inline_Always (Print_Subject);

   --  Print vmx error after vmlaunch/vmresumed failed.
   procedure Print_VMX_Error;
   pragma Inline_Always (Print_VMX_Error);

end SK.Dump;
