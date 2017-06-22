--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package SK.Crash_Audit_Types
is

   --  xxd -l 8 -p /dev/random
   Crash_Magic : constant := 16#d5ab_c59c_4a9a_2a93#;

   subtype Version_Str_Range is Positive range 1 .. 64;

   type Version_String_Type is new String (Version_Str_Range);

   Null_Version_String : constant Version_String_Type := (others => ASCII.NUL);

   Max_Dumps : constant := 3;

   type Dumpdata_Length is range 0 .. Max_Dumps
   with
      Size => 8;

   subtype Dumpdata_Index is Dumpdata_Length range 1 .. Dumpdata_Length'Last;

   Header_Type_Size : constant := 8 + 64 + (3 * 8) + 2 + 4 + 2;

   type Header_Type is record
      Version_Magic  : Interfaces.Unsigned_64;
      Version_String : Version_String_Type;
      Generation     : Interfaces.Unsigned_64;
      Boot_Count     : Interfaces.Unsigned_64;
      Crash_Count    : Interfaces.Unsigned_64;
      Max_Dump_Count : Dumpdata_Index;
      Dump_Count     : Dumpdata_Length;
      Crc32          : Interfaces.Unsigned_32;
      Padding        : Interfaces.Unsigned_16;
   end record
   with
      Size => Header_Type_Size * 8;

   Null_Header : constant Header_Type;

   ---------------------
   --  Crash Reasons  --
   ---------------------

   type Reason_Type is new Interfaces.Unsigned_64;

   Reason_Undefined : constant Reason_Type := 16#0000#;

   --  Exceptions.

   Hardware_Exception : constant Reason_Type := 16#1000#;

   --  Subject errors.

   Subj_No_Handler_For_Trap : constant Reason_Type := 16#2000#;
   Subj_Unknown_Trap        : constant Reason_Type := 16#2001#;

   --  Init failure.

   System_Init_Failure : constant Reason_Type := 16#3000#;

   subtype Subj_Reason_Range is Reason_Type range
     Subj_No_Handler_For_Trap .. Subj_Unknown_Trap;

   type Validity_Flags_Type is record
      Ex_Context   : Boolean;
      Subj_Context : Boolean;
      Init_Context : Boolean;
      Padding      : Bit_Array (1 .. 5);
   end record
   with
      Size => 8;

   Null_Validity_Flags : constant Validity_Flags_Type;

   Isr_Ctx_Size : constant := CPU_Regs_Size + 7 * 8;

   --  ISR execution environment state.
   type Isr_Context_Type is record
      Regs       : CPU_Registers_Type;
      Vector     : Word64;
      Error_Code : Word64;
      RIP        : Word64;
      CS         : Word64;
      RFLAGS     : Word64;
      RSP        : Word64;
      SS         : Word64;
   end record
   with
      Size => Isr_Ctx_Size * 8;

   Null_Isr_Context : constant Isr_Context_Type;

   Ex_Ctx_Size : constant := Isr_Ctx_Size + 3 * 8;

   type Exception_Context_Type is record
      ISR_Ctx       : Isr_Context_Type;
      CR0, CR3, CR4 : Word64;
   end record
   with
      Size => Ex_Ctx_Size * 8;

   Null_Exception_Context : constant Exception_Context_Type;

   type Subj_Ctx_Validity_Flags_Type is record
      Intr_Info       : Boolean;
      Intr_Error_Code : Boolean;
      Padding         : Bit_Array (1 .. 6);
   end record
   with
      Size => 8;

   Null_Subj_Ctx_Validity_Flags : constant Subj_Ctx_Validity_Flags_Type;

   Subj_Ctx_Size : constant := 2 + 1 + 1 + 4 + 4 + Subj_State_Size;

   type Subj_Context_Type is record
      Subject_ID      : Word16;
      Field_Validity  : Subj_Ctx_Validity_Flags_Type;
      Padding         : Byte;
      Intr_Info       : Word32;
      Intr_Error_Code : Word32;
      Descriptor      : Subject_State_Type;
   end record
   with
      Size => Subj_Ctx_Size * 8;

   Null_Subj_Context : constant Subj_Context_Type;

   Sys_Init_Ctx_Size : constant := 2;

   type System_Init_Context_Type is record
      VMX_Support         : Boolean;
      VMX_Disabled_Locked : Boolean;
      Protected_Mode      : Boolean;
      Paging              : Boolean;
      IA_32e_Mode         : Boolean;
      Apic_Support        : Boolean;
      CR0_Valid           : Boolean;
      CR4_Valid           : Boolean;
      Not_Virtual_8086    : Boolean;
      Invariant_TSC       : Boolean;
      Padding             : Bit_Array (1 .. 6);
   end record
   with
      Size => Sys_Init_Ctx_Size * 8;

   Null_System_Init_Context : constant System_Init_Context_Type;

   FPU_Init_Ctx_Size : constant := 1;

   type FPU_Init_Context_Type is record
      XSAVE_Support : Boolean;
      Area_Size     : Boolean;
      Padding       : Bit_Array (1 .. 6);
   end record
   with
      Size => FPU_Init_Ctx_Size * 8;

   Null_FPU_Init_Context : constant FPU_Init_Context_Type;

   MCE_Init_Ctx_Size : constant := 1;

   type MCE_Init_Context_Type is record
      MCE_Support : Boolean;
      MCA_Support : Boolean;
      Padding     : Bit_Array (1 .. 6);
   end record
   with
      Size => MCE_Init_Ctx_Size * 8;

   Null_MCE_Init_Context : constant MCE_Init_Context_Type;

   type Init_Context_Type is record
      Sys_Ctx : System_Init_Context_Type;
      FPU_Ctx : FPU_Init_Context_Type;
      MCE_Ctx : MCE_Init_Context_Type;
   end record
   with
      Size => (Sys_Init_Ctx_Size + FPU_Init_Ctx_Size + MCE_Init_Ctx_Size) * 8;

   Null_Init_Context : constant Init_Context_Type;

   Dumpdata_Size : constant := (8 + 8 + 1 + 1 + Ex_Ctx_Size + Subj_Ctx_Size);

   type Dumpdata_Type is record
      TSC_Value         : Interfaces.Unsigned_64;
      Reason            : Reason_Type;
      APIC_ID           : Byte;
      Field_Validity    : Validity_Flags_Type;
      Exception_Context : Exception_Context_Type;
      Subject_Context   : Subj_Context_Type;
   end record
   with
      Size => Dumpdata_Size * 8;

   Null_Dumpdata : constant Dumpdata_Type;

   Dumpdata_Array_Size : constant
     := Positive (Dumpdata_Index'Last) * Dumpdata_Size;

   type Dumpdata_Array is array (Dumpdata_Index) of Dumpdata_Type
   with
      Pack,
      Size => Dumpdata_Array_Size * 8;

   Null_Dumpdata_Array : constant Dumpdata_Array;

   type Dump_Type is record
      Header : Header_Type;
      Data   : Dumpdata_Array;
   end record
   with
      Size => (Header_Type_Size + Dumpdata_Array_Size) * 8;

   Null_Dump : constant Dump_Type;

private

   for Header_Type use record
      Version_Magic  at   0 range 0 .. 63;
      Version_String at   8 range 0 .. Version_Str_Range'Last * 8 - 1;
      Generation     at  72 range 0 .. 63;
      Boot_Count     at  80 range 0 .. 63;
      Crash_Count    at  88 range 0 .. 63;
      Max_Dump_Count at  96 range 0 .. 7;
      Dump_Count     at  97 range 0 .. 7;
      Crc32          at  98 range 0 .. 31;
      Padding        at 102 range 0 .. 15;
   end record;

   for Validity_Flags_Type use record
      Ex_Context   at 0 range 0 .. 0;
      Subj_Context at 0 range 1 .. 1;
      Init_Context at 0 range 2 .. 2;
      Padding      at 0 range 3 .. 7;
   end record;

   for Isr_Context_Type use record
      Regs       at 0                  range 0 .. 8 * CPU_Regs_Size - 1;
      Vector     at CPU_Regs_Size      range 0 .. 63;
      Error_Code at CPU_Regs_Size + 8  range 0 .. 63;
      RIP        at CPU_Regs_Size + 16 range 0 .. 63;
      CS         at CPU_Regs_Size + 24 range 0 .. 63;
      RFLAGS     at CPU_Regs_Size + 32 range 0 .. 63;
      RSP        at CPU_Regs_Size + 40 range 0 .. 63;
      SS         at CPU_Regs_Size + 48 range 0 .. 63;
   end record;

   for Exception_Context_Type use record
      ISR_Ctx at 0                 range 0 .. 8 * Isr_Ctx_Size - 1;
      CR0     at Isr_Ctx_Size      range 0 .. 63;
      CR3     at Isr_Ctx_Size + 8  range 0 .. 63;
      CR4     at Isr_Ctx_Size + 16 range 0 .. 63;
   end record;

   for Subj_Ctx_Validity_Flags_Type use record
      Intr_Info       at 0 range 0 .. 0;
      Intr_Error_Code at 0 range 1 .. 1;
      Padding         at 0 range 2 .. 7;
   end record;

   for Subj_Context_Type use record
      Subject_ID      at 0  range 0 .. 15;
      Field_Validity  at 2  range 0 .. 7;
      Padding         at 3  range 0 .. 7;
      Intr_Info       at 4  range 0 .. 31;
      Intr_Error_Code at 8  range 0 .. 31;
      Descriptor      at 12 range 0 .. Subj_State_Size * 8 - 1;
   end record;

   for System_Init_Context_Type use record
      VMX_Support         at 0 range 0 .. 0;
      VMX_Disabled_Locked at 0 range 1 .. 1;
      Protected_Mode      at 0 range 2 .. 2;
      Paging              at 0 range 3 .. 3;
      IA_32e_Mode         at 0 range 4 .. 4;
      Apic_Support        at 0 range 5 .. 5;
      CR0_Valid           at 0 range 6 .. 6;
      CR4_Valid           at 0 range 7 .. 7;
      Not_Virtual_8086    at 1 range 0 .. 0;
      Invariant_TSC       at 1 range 1 .. 1;
      Padding             at 1 range 2 .. 7;
   end record;

   for FPU_Init_Context_Type use record
      XSAVE_Support at 0 range 0 .. 0;
      Area_Size     at 0 range 1 .. 1;
      Padding       at 0 range 2 .. 7;
   end record;

   for MCE_Init_Context_Type use record
      MCE_Support at 0 range 0 .. 0;
      MCA_Support at 0 range 1 .. 1;
      Padding     at 0 range 2 .. 7;
   end record;

   MCE_Offset : constant := Sys_Init_Ctx_Size + FPU_Init_Ctx_Size;

   for Init_Context_Type use record
      Sys_Ctx at 0                 range 0 .. 8 * Sys_Init_Ctx_Size - 1;
      FPU_Ctx at Sys_Init_Ctx_Size range 0 .. 8 * FPU_Init_Ctx_Size - 1;
      MCE_Ctx at MCE_Offset        range 0 .. 8 * MCE_Init_Ctx_Size - 1;
   end record;

   for Dumpdata_Type use record
      TSC_Value         at  0 range 0 .. 63;
      Reason            at  8 range 0 .. 63;
      APIC_ID           at 16 range 0 .. 7;
      Field_Validity    at 17 range 0 .. 7;
      Exception_Context at 18 range 0 .. Ex_Ctx_Size * 8 - 1;
      Subject_Context   at 18 + Ex_Ctx_Size range 0 .. Subj_Ctx_Size * 8 - 1;
   end record;

   for Dump_Type use record
      Header at 0                range 0 .. 8 * Header_Type_Size - 1;
      Data   at Header_Type_Size range 0 .. 8 * Dumpdata_Array_Size - 1;
   end record;

   Null_Header : constant Header_Type
     := (Version_Magic  => Crash_Magic,
         Version_String => Null_Version_String,
         Generation     => 0,
         Boot_Count     => 1,
         Crash_Count    => 0,
         Max_Dump_Count => Max_Dumps,
         Dump_Count     => 0,
         Crc32          => 0,
         Padding        => 0);

   Null_Validity_Flags : constant Validity_Flags_Type
     := (Ex_Context   => False,
         Subj_Context => False,
         Init_Context => False,
         others       => (others => 0));

   Null_Isr_Context : constant Isr_Context_Type
     := (Regs   => Null_CPU_Regs,
         others => 0);

   Null_Exception_Context : constant Exception_Context_Type
     := (ISR_Ctx => Null_Isr_Context,
         others  => 0);

   Null_Subj_Ctx_Validity_Flags : constant Subj_Ctx_Validity_Flags_Type
     := (Intr_Info       => False,
         Intr_Error_Code => False,
         others          => (others => 0));

   Null_Subj_Context : constant Subj_Context_Type
     := (Subject_ID      => 0,
         Field_Validity  => Null_Subj_Ctx_Validity_Flags,
         Padding         => 0,
         Intr_Info       => 0,
         Intr_Error_Code => 0,
         Descriptor      => Null_Subject_State);

   Null_System_Init_Context : constant System_Init_Context_Type
     := (Padding => (others => 0),
         others  => False);

   Null_FPU_Init_Context : constant FPU_Init_Context_Type
     := (Padding => (others => 0),
         others  => False);

   Null_MCE_Init_Context : constant MCE_Init_Context_Type
     := (Padding => (others => 0),
         others  => False);

   Null_Init_Context : constant Init_Context_Type
     := (Sys_Ctx => Null_System_Init_Context,
         FPU_Ctx => Null_FPU_Init_Context,
         MCE_Ctx => Null_MCE_Init_Context);

   Null_Dumpdata : constant Dumpdata_Type
     := (TSC_Value         => 0,
         APIC_ID           => 0,
         Reason            => Reason_Undefined,
         Field_Validity    => Null_Validity_Flags,
         Exception_Context => Null_Exception_Context,
         Subject_Context   => Null_Subj_Context);

   Null_Dumpdata_Array : constant Dumpdata_Array
     := (others => Null_Dumpdata);

   Null_Dump : constant Dump_Type
     := (Header => Null_Header,
         Data   => Null_Dumpdata_Array);

end SK.Crash_Audit_Types;
