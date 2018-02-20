--
--  Copyright (C) 2013-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

--D @Interface
--D This package implements subprograms corresponding to low-level Intel VT-x
--D instructions. They are required for the management of VMX data structures
--D such as VMXON and VMCS regions, see Intel SDM Vol. 3C, "Chapter 30 VMX
--D Instruction Reference" \cite{intelsdm}.
package SK.CPU.VMX
is

   procedure VMXON
     (Region  :     Word64;
      Success : out Boolean)
   with
      Global  => (In_Out => X86_64.State),
      Depends => ((X86_64.State, Success) => (X86_64.State, Region)),
      Inline_Always;

   procedure VMCLEAR
     (Region  :     Word64;
      Success : out Boolean)
   with
      Global  => (In_Out => X86_64.State),
      Depends => ((X86_64.State, Success) => (X86_64.State, Region)),
      Inline_Always;

   procedure VMPTRLD
     (Region  :     Word64;
      Success : out Boolean)
   with
      Global  => (In_Out => X86_64.State),
      Depends => ((X86_64.State, Success) => (X86_64.State, Region)),
      Inline_Always;

   procedure VMPTRST
     (Region  : out Word64;
      Success : out Boolean)
   with
      Global  => (Input => X86_64.State),
      Depends => ((Region, Success) => X86_64.State),
      Inline_Always;

   procedure VMREAD
     (Field   :     Word64;
      Value   : out Word64;
      Success : out Boolean)
   with
      Global  => (Input => X86_64.State),
      Depends => ((Value, Success) => (X86_64.State, Field)),
      Inline_Always;

   procedure VMWRITE
     (Field   :     Word64;
      Value   :     Word64;
      Success : out Boolean)
   with
      Global  => (In_Out => X86_64.State),
      Depends => ((X86_64.State, Success) => (X86_64.State, Field, Value)),
      Inline_Always;

   procedure INVVPID
     (VPID    :     Word16;
      Success : out Boolean)
   with
      Global  => (In_Out => X86_64.State),
      Depends => ((X86_64.State, Success) => (X86_64.State, VPID)),
      Inline_Always;

end SK.CPU.VMX;
