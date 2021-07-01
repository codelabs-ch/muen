--
--  Copyright (C) 2013, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
--D Top-level package defining common types.
package SK
is

   type Bit_Type is range 0 .. 1
   with
      Size => 1;

   type Bit_Array is array (Positive range <>) of Bit_Type
   with
      Pack;

   subtype Byte   is Interfaces.Unsigned_8;
   subtype Word16 is Interfaces.Unsigned_16;
   subtype Word32 is Interfaces.Unsigned_32;
   subtype Word64 is Interfaces.Unsigned_64;

   pragma Warnings (Off, Reason => "Simplify type usage in child packages");
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;
   pragma Warnings (On);

   CPU_Regs_Size : constant := 16 * 8;

   --  CPU registers.
   type CPU_Registers_Type is record
      CR2 : Word64;
      RAX : Word64;
      RBX : Word64;
      RCX : Word64;
      RDX : Word64;
      RDI : Word64;
      RSI : Word64;
      RBP : Word64;
      R08 : Word64;
      R09 : Word64;
      R10 : Word64;
      R11 : Word64;
      R12 : Word64;
      R13 : Word64;
      R14 : Word64;
      R15 : Word64;
   end record
   with
      Size => CPU_Regs_Size * 8;

   Null_CPU_Regs : constant CPU_Registers_Type;

   --  Size of one page (4k).
   Page_Size : constant := 4096;

   --  Size of XSAVE storage area in bytes. Must be at least as large as
   --  described in Intel SDM Vol. 1, "13.4 XSAVE Area".
   XSAVE_Area_Size          : constant := Page_Size;
   XSAVE_Legacy_Header_Size : constant := 32;

   --  For layout of XSAVE legacy region see Intel SDM Vol. 1,
   --  "13.4.1 Legacy Region of an XSAVE Area".
   type XSAVE_Legacy_Header_Type is record
      FCW        : Word16;
      FSW        : Word16;
      FTW        : Byte;
      Reserved   : Byte;
      FOP        : Word16;
      FIP        : Word64;
      FDP        : Word64;
      MXCSR      : Word32;
      MXCSR_Mask : Word32;
   end record
   with
      Size => XSAVE_Legacy_Header_Size * 8;

   Null_XSAVE_Legacy_Header : constant XSAVE_Legacy_Header_Type;

   type XSAVE_Extended_Region_Type is array (32 .. XSAVE_Area_Size - 1) of Byte
   with
      Size => (XSAVE_Area_Size - XSAVE_Legacy_Header_Size) * 8;

   --D @Interface
   --D XSAVE area used to save the FPU state. see Intel SDM Vol. 1,
   --D "13.4 XSAVE Area".
   type XSAVE_Area_Type is record
      --D @Interface
      --D Legacy region of the XSAVE area excluding the SSE register state.
      Legacy_Header   : XSAVE_Legacy_Header_Type;
      --D @Interface
      --D Extended region of XSAVE area including XSAVE header as well as SSE
      --D register state.
      Extended_Region : XSAVE_Extended_Region_Type;
   end record
   with
      Pack,
      Alignment => 64,
      Size      => XSAVE_Area_Size * 8;

   Seg_Type_Size : constant := 8 + 8 + 4 + 4;

   type Segment_Type is record
      Selector      : Word64;
      Base          : Word64;
      Limit         : Word32;
      Access_Rights : Word32;
   end record
   with
      Size => Seg_Type_Size * 8;

   Null_Segment : constant Segment_Type;

   Segment_Regs_Size : constant := Seg_Type_Size * 8;

   --  Segment registers.
   type Segment_Registers_Type is record
      CS   : Segment_Type;
      SS   : Segment_Type;
      DS   : Segment_Type;
      ES   : Segment_Type;
      FS   : Segment_Type;
      GS   : Segment_Type;
      TR   : Segment_Type;
      LDTR : Segment_Type;
   end record
     with
       Size => Segment_Regs_Size * 8;

   Subj_State_Size : constant :=
     (CPU_Regs_Size + Segment_Regs_Size + 2 * Seg_Type_Size + 4 * 4 + 13 * 8);

   --D @Interface
   --D The Muen SK stores the subject state of each running subject into a
   --D variable of this type on VM exit. Also, subject monitors are able to
   --D inspect the state of the monitored subject using this record.
   type Subject_State_Type is record
      --D @Interface
      --D CPU registers CR2, RAX, RBX, RCX, RDX, RDI, RSI, RBP, R08-R15
      --D (64 bits each).
      Regs                : CPU_Registers_Type;
      --D @Interface
      --D Exit reason; Intel SDM Vol. 3C, "24.9.1 Basic VM-Exit Information".
      --D This field encodes the reason for the VM exit.
      Exit_Reason        : Word32;
      --D @Interface
      --D Interruptibility state; Intel SDM Vol. 3C, "24.4.2 Guest Non-Register
      --D State". The IA-32 architecture includes features that permit certain
      --D events to be blocked for a period of time. This field contains
      --D information about such blocking.
      Intr_State         : Word32;
      --D @Interface
      --D Guest IA32\_SYSENTER\_CS MSR; Intel SDM Vol. 3C, "24.4.1 Guest
      --D Register State".
      SYSENTER_CS        : Word32;
      --D @Interface
      --D Exit instruction length; Intel SDM Vol. 3C, "24.9.4 Information for
      --D VM Exits Due to Instruction Execution". This field receives the
      --D length in bytes of the instruction whose execution led to the VM
      --D exit. Also used in the context of software interrupts or software
      --D exceptions.
      Instruction_Len    : Word32;
      --D @Interface
      --D Exit qualification; Intel SDM Vol. 3C, "24.9.1 Basic VM-Exit
      --D Information". This field contains additional information about the
      --D cause of VM exits.
      Exit_Qualification : Word64;
      --D @Interface
      --D Guest-physical address of exit due to EPT violations and EPT
      --D misconfigurations; Intel SDM Vol. 3C, "24.9.1 Basic VM-Exit
      --D Information".
      Guest_Phys_Addr    : Word64;
      --D @Interface
      --D Guest RIP register; Intel SDM Vol. 3C, "24.4.1 Guest Register State".
      RIP                : Word64;
      --D @Interface
      --D Guest RSP register; Intel SDM Vol. 3C, "24.4.1 Guest Register
      --D State".
      RSP                : Word64;
      --D @Interface
      --D Guest CR0 control register; Intel SDM Vol. 3C, "24.4.1 Guest Register
      --D State".
      CR0                : Word64;
      --D @Interface
      --D CR0 control register read shadow; Intel SDM Vol. 3C, "24.6.6
      --D Guest/Host Masks and Read Shadows for CR0 and CR4".
      SHADOW_CR0         : Word64;
      --D @Interface
      --D Guest CR3 control register.
      CR3                : Word64;
      --D @Interface
      --D Guest CR4 control register; Intel SDM Vol. 3C, "24.4.1 Guest Register
      --D State".
      CR4                : Word64;
      --D @Interface
      --D CR4 control register read shadow; Intel SDM Vol. 3C, "24.6.6
      --D Guest/Host Masks and Read Shadows for CR0 and CR4".
      SHADOW_CR4         : Word64;
      --D @Interface
      --D Guest RFLAGS register; Intel SDM Vol. 3C, "24.4.1 Guest Register
      --D State".
      RFLAGS             : Word64;
      --D @Interface
      --D Guest IA32\_EFER MSR; Intel SDM Vol. 3C, "24.4.1 Guest Register
      --D State".
      IA32_EFER          : Word64;
      --D @Interface
      --D Guest IA32\_SYSENTER\_ESP MSR; Intel SDM Vol. 3C, "24.4.1 Guest
      --D Register State".
      SYSENTER_ESP       : Word64;
      --D @Interface
      --D Guest IA32\_SYSENTER\_EIP MSR; Intel SDM Vol. 3C, "24.4.1 Guest
      --D Register State".
      SYSENTER_EIP       : Word64;
      --D @Interface
      --D Guest segment registers CS, SS, DS, ES, FS, GS, TR and LDTR.
      --D Intel SDM Vol. 3C, "24.4.1 Guest Register State".
      Segment_Regs       : Segment_Registers_Type;
      --D @Interface
      --D Guest global descriptor table register (GDTR).
      GDTR               : Segment_Type;
      --D @Interface
      --D Guest interrupt descriptor table register (IDTR).
      IDTR               : Segment_Type;
   end record
   with
      Pack,
      Size        => Subj_State_Size * 8,
      Object_Size => Subj_State_Size * 8;

   Null_Subject_State : constant Subject_State_Type;

   --  Pseudo Descriptor type, see Intel SDM Vol. 3A, "3.5.1 Segment Descriptor
   --  Tables"
   type Pseudo_Descriptor_Type is record
      Limit : SK.Word16;
      Base  : SK.Word64;
   end record
   with
      Size => 80;

private

   for XSAVE_Legacy_Header_Type use record
      FCW        at  0 range  0 .. 15;
      FSW        at  2 range  0 .. 15;
      FTW        at  4 range  0 ..  7;
      Reserved   at  5 range  0 ..  7;
      FOP        at  6 range  0 .. 15;
      FIP        at  8 range  0 .. 63;
      FDP        at 16 range  0 .. 63;
      MXCSR      at 24 range  0 .. 31;
      MXCSR_Mask at 28 range  0 .. 31;
   end record;

   for Segment_Type use record
      Selector      at  0 range 0 .. 63;
      Base          at  8 range 0 .. 63;
      Limit         at 16 range 0 .. 31;
      Access_Rights at 20 range 0 .. 31;
   end record;

   for Pseudo_Descriptor_Type use record
      Limit at 0 range 0 .. 15;
      Base  at 2 range 0 .. 63;
   end record;

   for CPU_Registers_Type use record
      CR2 at   0 range 0 .. 63;
      RAX at   8 range 0 .. 63;
      RBX at  16 range 0 .. 63;
      RCX at  24 range 0 .. 63;
      RDX at  32 range 0 .. 63;
      RDI at  40 range 0 .. 63;
      RSI at  48 range 0 .. 63;
      RBP at  56 range 0 .. 63;
      R08 at  64 range 0 .. 63;
      R09 at  72 range 0 .. 63;
      R10 at  80 range 0 .. 63;
      R11 at  88 range 0 .. 63;
      R12 at  96 range 0 .. 63;
      R13 at 104 range 0 .. 63;
      R14 at 112 range 0 .. 63;
      R15 at 120 range 0 .. 63;
   end record;

   Null_XSAVE_Legacy_Header : constant XSAVE_Legacy_Header_Type
     := (FCW        => 0,
         FSW        => 0,
         FTW        => 0,
         Reserved   => 0,
         FOP        => 0,
         FIP        => 0,
         FDP        => 0,
         MXCSR      => 0,
         MXCSR_Mask => 0);

   Null_CPU_Regs : constant CPU_Registers_Type
     := CPU_Registers_Type'(others => 0);

   Null_Segment : constant Segment_Type
     := Segment_Type'(Selector      => 0,
                      Base          => 0,
                      Limit         => 0,
                      Access_Rights => 0);

   Null_Subject_State : constant Subject_State_Type
     := Subject_State_Type'
       (Regs            => Null_CPU_Regs,
        Exit_Reason     => 0,
        Intr_State      => 0,
        SYSENTER_CS     => 0,
        Instruction_Len => 0,
        Segment_Regs    => (CS   => Null_Segment,
                            SS   => Null_Segment,
                            DS   => Null_Segment,
                            ES   => Null_Segment,
                            FS   => Null_Segment,
                            GS   => Null_Segment,
                            TR   => Null_Segment,
                            LDTR => Null_Segment),
        GDTR            => Null_Segment,
        IDTR            => Null_Segment,
        others          => 0);

end SK;
