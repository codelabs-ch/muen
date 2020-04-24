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

package SK.CPU
is

   --  Clear Interrupt Flag.
   procedure Cli
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null),
      Inline_Always;

   --  Execute CPUID instruction.
   procedure CPUID
     (EAX : in out SK.Word32;
      EBX :    out SK.Word32;
      ECX : in out SK.Word32;
      EDX :    out SK.Word32)
   with
      Global  => (Input => X86_64.State),
      Depends => ((EAX, EBX, ECX, EDX) => (EAX, ECX, X86_64.State)),
      Inline_Always;

   --  Initialize FPU without checking for pending unmasked FP exceptions.
   procedure Fninit
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null),
      Inline_Always;

   --  Load MXCSR Register.
   procedure Ldmxcsr (Value : Word32)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ Value),
      Inline_Always;

   --  Halt the CPU.
   procedure Hlt
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null),
      Inline_Always;

   --  Set Interrupt Flag.
   procedure Sti
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null),
      Inline_Always;

   --  Load Global Descriptor Table (GDT) register.
   procedure Lgdt (Descriptor : Pseudo_Descriptor_Type)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ Descriptor),
      Inline_Always;

   --  Load Interrupt Descriptor Table (IDT) register.
   procedure Lidt (Descriptor : Pseudo_Descriptor_Type)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ Descriptor),
      Inline_Always;

   --  Load Task Register (TR).
   procedure Ltr (Address : SK.Word16)
   with
     Global  => (In_Out => X86_64.State),
     Depends => (X86_64.State =>+ Address),
     Inline_Always;

   --  Spin Loop Hint.
   procedure Pause
   with
      Global => null,
      Inline_Always;

   --  RDTSC (Read Time-Stamp Counter).
   function RDTSC return SK.Word64
   with
      Global => (Input => X86_64.State),
      Volatile_Function,
      Inline_Always;
   pragma Annotate (GNATprove, Terminating, RDTSC);

   --  Stop CPU.
   procedure Stop
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null),
      Inline_Always,
      No_Return;

   --  Return current value of CR0 register.
   function Get_CR0 return SK.Word64
   with
      Global  => (Input => X86_64.State),
      Volatile_Function,
      Inline_Always;
   pragma Annotate (GNATprove, Terminating, Get_CR0);

   --  Return current value of CR2 register.
   function Get_CR2 return SK.Word64
   with
      Global  => (Input => X86_64.State),
      Volatile_Function,
      Inline_Always;
   pragma Annotate (GNATprove, Terminating, Get_CR2);

   --  Return current value of CR3 register.
   function Get_CR3 return SK.Word64
   with
      Global  => (Input => X86_64.State),
      Volatile_Function,
      Inline_Always;
   pragma Annotate (GNATprove, Terminating, Get_CR3);

   --  Return current value of CR4 register.
   function Get_CR4 return SK.Word64
   with
      Global  => (Input => X86_64.State),
      Volatile_Function,
      Inline_Always;
   pragma Annotate (GNATprove, Terminating, Get_CR4);

      --  Set value of CR2.
   procedure Set_CR2 (Value : SK.Word64)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ Value),
      Inline_Always;

   --  Set value of CR4.
   procedure Set_CR4 (Value : SK.Word64)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ Value),
      Inline_Always;

   --  Return current value of given model specific register.
   function Get_MSR64 (Register : SK.Word32) return SK.Word64
   with
      Global  => (Input => X86_64.State),
      Volatile_Function,
      Inline_Always;
   pragma Annotate (GNATprove, Terminating, Get_MSR64);

   --  Return value of given MSR as low/high doublewords.
   procedure Get_MSR
     (Register :     SK.Word32;
      Low      : out SK.Word32;
      High     : out SK.Word32)
   with
      Global  => (Input => X86_64.State),
      Depends => ((Low, High) => (X86_64.State, Register)),
      Inline_Always;

   --  Write specified quadword to given MSR.
   procedure Write_MSR64
     (Register : SK.Word32;
      Value    : SK.Word64)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ (Register, Value)),
      Inline_Always;

   --  Write specified low/high doublewords to given MSR.
   procedure Write_MSR
     (Register : SK.Word32;
      Low      : SK.Word32;
      High     : SK.Word32)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ (Register, Low, High)),
      Inline_Always;

   --  Return current RFLAGS.
   function Get_RFLAGS return SK.Word64
   with
      Global  => (Input => X86_64.State),
      Volatile_Function,
      Inline_Always;
   pragma Annotate (GNATprove, Terminating, Get_RFLAGS);

   --  Restore specified Processor Extended States from given XSAVE area.
   procedure XRSTOR
     (Source : XSAVE_Area_Type;
      State  : Word64)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ (Source, State)),
      Inline_Always;

   --  Save specified Processor Extended States to given XSAVE area.
   procedure XSAVE
     (Target : out XSAVE_Area_Type;
      State  :     Word64)
   with
      Global  => (Input => X86_64.State),
      Depends => (Target => (X86_64.State, State)),
      Inline_Always;

   --  Set specified Extended Control Register (XCR) to given value.
   procedure XSETBV
     (Register : SK.Word32;
      Value    : SK.Word64)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ (Register, Value)),
      Inline_Always;

end SK.CPU;
