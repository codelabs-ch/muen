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

use type Skp.Dst_Vector_Range;

--# inherit
--#    System,
--#    Skp.Interrupts,
--#    X86_64,
--#    SK.IO,
--#    SK.IO_Apic,
--#    SK.Descriptors,
--#    SK.CPU_Registry;
package SK.Interrupts
--# own
--#    State;
--# initializes
--#    State;
is

   --  Initalize IDT structure.
   procedure Init;
   --# global
   --#    in out State;
   --# derives
   --#    State from *;

   --  Load IDT into IDT register.
   procedure Load;
   --# global
   --#    in     State;
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, State;

   --  Return IDT pointer.
   function Get_IDT_Pointer return Descriptors.Pseudo_Descriptor_Type;
   --# global
   --#    State;

   --  Mask all interrupts in the legacy PIC.
   procedure Disable_Legacy_PIC;
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *;

   --  Setup I/O APIC IRQ routing.
   procedure Setup_IRQ_Routing;
   --# global
   --#    in     CPU_Registry.State;
   --#    in out IO_Apic.State;
   --# derives
   --#    IO_Apic.State from *, CPU_Registry.State;

end SK.Interrupts;
