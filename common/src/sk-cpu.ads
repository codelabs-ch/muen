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

--# inherit
--#    SK,
--#    X86_64;
package SK.CPU
is

   --  Execute CPUID instruction.
   procedure CPUID
     (EAX : in out SK.Word32;
      EBX :    out SK.Word32;
      ECX : in out SK.Word32;
      EDX :    out SK.Word32);
   --# global
   --#    in X86_64.State;
   --# derives
   --#    EAX, EBX, ECX, EDX from X86_64.State, EAX, ECX;
   pragma Inline_Always (CPUID);

   --  Halt the CPU.
   procedure Hlt;
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *;
   pragma Inline_Always (Hlt);

   --  Set Interrupt Flag.
   procedure Sti;
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *;
   pragma Inline_Always (Sti);

   --  Load Interrupt Descriptor Table (IDT) register.
   procedure Lidt (Address : SK.Word64);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Address;
   pragma Inline_Always (Lidt);

   --  Panic.
   procedure Panic;
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *;
   pragma Inline_Always (Panic);

   --  Stop CPU.
   procedure Stop;
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *;
   pragma Inline_Always (Stop);

   --  Return current value of CR0 register.
   function Get_CR0 return SK.Word64;
   --# global
   --#    X86_64.State;
   pragma Inline_Always (Get_CR0);

   --  Return current value of CR2 register.
   function Get_CR2 return SK.Word64;
   --# global
   --#    X86_64.State;
   pragma Inline_Always (Get_CR2);

   --  Return current value of CR3 register.
   function Get_CR3 return SK.Word64;
   --# global
   --#    X86_64.State;
   pragma Inline_Always (Get_CR3);

   --  Return current value of CR4 register.
   function Get_CR4 return SK.Word64;
   --# global
   --#    X86_64.State;
   pragma Inline_Always (Get_CR4);

      --  Set value of CR2.
   procedure Set_CR2 (Value : SK.Word64);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Value;
   pragma Inline_Always (Set_CR2);

   --  Set value of CR4.
   procedure Set_CR4 (Value : SK.Word64);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Value;
   pragma Inline_Always (Set_CR4);

   --  Return current value of given model specific register.
   function Get_MSR64 (Register : SK.Word32) return SK.Word64;
   --# global
   --#    X86_64.State;
   pragma Inline_Always (Get_MSR64);

   --  Return value of given MSR as low/high doublewords.
   procedure Get_MSR
     (Register :     SK.Word32;
      Low      : out SK.Word32;
      High     : out SK.Word32);
   --# global
   --#    X86_64.State;
   --# derives
   --#    Low, High from X86_64.State, Register;
   pragma Inline_Always (Get_MSR);

   --  Write specified quadword to given MSR.
   procedure Write_MSR64
     (Register : SK.Word32;
      Value    : SK.Word64);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Register, Value;
   pragma Inline_Always (Write_MSR64);

   --  Write specified low/high doublewords to given MSR.
   procedure Write_MSR
     (Register : SK.Word32;
      Low      : SK.Word32;
      High     : SK.Word32);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Register, Low, High;
   pragma Inline_Always (Write_MSR);

   --  Return current RFLAGS.
   function Get_RFLAGS return SK.Word64;
   --# global
   --#    X86_64.State;
   pragma Inline_Always (Get_RFLAGS);

   --  Set CPU RSP and RBP registers to given address.
   procedure Set_Stack (Address : SK.Word64);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Address;
   pragma Inline_Always (Set_Stack);

   --  Enter VMX operation.
   procedure VMXON
     (Region  :     SK.Word64;
      Success : out Boolean);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Region &
   --#    Success      from X86_64.State, Region;
   pragma Inline_Always (VMXON);

   procedure VMCLEAR
     (Region  :     SK.Word64;
      Success : out Boolean);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Region &
   --#    Success      from X86_64.State, Region;
   pragma Inline_Always (VMCLEAR);

   procedure VMPTRLD
     (Region  :     SK.Word64;
      Success : out Boolean);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Region &
   --#    Success      from X86_64.State, Region;
   pragma Inline_Always (VMPTRLD);

   procedure VMREAD
     (Field   :     SK.Word64;
      Value   : out SK.Word64;
      Success : out Boolean);
   --# global
   --#    in X86_64.State;
   --# derives
   --#    Value, Success from X86_64.State, Field;
   pragma Inline_Always (VMREAD);

   procedure VMWRITE
     (Field   :     SK.Word64;
      Value   :     SK.Word64;
      Success : out Boolean);
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State, Success from X86_64.State, Field, Value;
   pragma Inline_Always (VMWRITE);

end SK.CPU;
