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

with SK.Descriptors;
with SK.CPU_Registry;
with SK.IO_Apic;
with X86_64;

use type Skp.Dst_Vector_Range;

package SK.Interrupts
with
   Abstract_State => State,
   Initializes    => State
is

   --  Initialize IDT structure.
   procedure Init
   with
      Global  => (In_Out => State),
      Depends => (State =>+ null);

   --  Load IDT into IDT register.
   procedure Load
   with
      Global  => (Input => State, In_Out => X86_64.State),
      Depends => (X86_64.State =>+ State);

   --  Return IDT pointer.
   function Get_IDT_Pointer return Descriptors.Pseudo_Descriptor_Type
   with
      Global  => State;

   --  Mask all interrupts in the legacy PIC.
   procedure Disable_Legacy_PIC
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null);

   --  Setup I/O APIC IRQ routing.
   procedure Setup_IRQ_Routing (VTd_Enabled : Boolean)
   with
      Global  => (Input => CPU_Registry.State, In_Out => IO_Apic.State),
      Depends => (IO_Apic.State =>+ (CPU_Registry.State, VTd_Enabled));

   pragma $Prove_Warnings (Off, "unused variable ""Unused_Context""",
      Reason => "Unused Context is only used for debugging");

   --  Halt on (unexpected) exception.
   procedure Dispatch_Exception (Unused_Context : SK.Isr_Context_Type)
   with
      Global     => (In_Out => X86_64.State),
      Depends    => (X86_64.State =>+ null,
                     null         =>  Unused_Context),
      Export,
      Convention => C,
      Link_Name  => "dispatch_interrupt";

   pragma $Prove_Warnings (On, "unused variable ""Unused_Context""");
end SK.Interrupts;
