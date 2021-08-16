--
--  Copyright (C) 2013, 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with System.Storage_Elements;

with SK.CPU;
with SK.Descriptors;
with SK.Task_State;

package body SK.Interrupt_Tables
with
   Refined_State => (State => (ISRs, Instance))
is

   subtype ISR_Array is Descriptors.ISR_Array (Descriptors.Vector_Range);

   IDT_Type_Size : constant := 256 * 16 * 8;

   subtype IDT_Type is Descriptors.IDT_Type (Descriptors.Vector_Range);

   GDT_Type_Size : constant := 5 * 8 * 8;

   type GDT_Type is array (1 .. 5) of Word64
   with
      Size      => GDT_Type_Size,
      Alignment => 8;

   --  Range of descriptor table entries.
   type Descriptor_Table_Range is range 1 .. 256;

   type Manager_Type is record
      GDT            : GDT_Type;
      IDT            : IDT_Type;
      TSS            : Task_State.TSS_Type;
      GDT_Descriptor : Pseudo_Descriptor_Type;
      IDT_Descriptor : Pseudo_Descriptor_Type;
   end record;

   TSS_Type_Size : constant := 104 * 8;
   Descr_Size    : constant := 10 * 8;

   IDT_Offsetbits  : constant := GDT_Type_Size;
   TSS_Offsetbits  : constant := IDT_Offsetbits  + IDT_Type_Size;
   GDTD_Offsetbits : constant := TSS_Offsetbits  + TSS_Type_Size;
   IDTD_Offsetbits : constant := GDTD_Offsetbits + Descr_Size;

   for Manager_Type use record
      GDT            at 0                   range 0 .. GDT_Type_Size - 1;
      IDT            at IDT_Offsetbits / 8  range 0 .. IDT_Type_Size - 1;
      TSS            at TSS_Offsetbits / 8  range 0 .. TSS_Type_Size - 1;
      GDT_Descriptor at GDTD_Offsetbits / 8 range 0 .. Descr_Size - 1;
      IDT_Descriptor at IDTD_Offsetbits / 8 range 0 .. Descr_Size - 1;
   end record;

   --  ISR array: Only required once because it is read-only in .rodata.
   ISRs : constant ISR_Array
   with
      Import,
      Convention => C,
      Link_Name  => "isrlist";

   Null_Pseudo_Descriptor : constant Pseudo_Descriptor_Type
     := (Limit => 0,
         Base  => 0);

   --D @Text Section => SK.Interrupt_Tables.Instance
   --D Descriptor tables instance consisting of Global Descriptor Table (GDT),
   --D Interrupt Descriptor Table (IDT), Task State Segment (TSS) as well as
   --D GDT and IDT descriptors.
   Instance : Manager_Type
     := (GDT            => (others => 0),
         IDT            => (others => Descriptors.Null_Gate),
         TSS            => Task_State.Null_TSS,
         GDT_Descriptor => Null_Pseudo_Descriptor,
         IDT_Descriptor => Null_Pseudo_Descriptor);

   --  Setup GDT with three entries (code, stack and tss) and load it into
   --  GDTR.
   procedure Load_GDT
     (TSS_Addr       :     Word64;
      GDT_Addr       :     Word64;
      GDT            : out GDT_Type;
      GDT_Descriptor : out Pseudo_Descriptor_Type);

   --  Load IDT into IDTR.
   procedure Load_IDT
     (IDT_Addr       :     Word64;
      IDT_Length     :     Descriptor_Table_Range;
      IDT_Descriptor : out Pseudo_Descriptor_Type);

   --  Setup TSS with IST entry 1 using the given stack address and load it
   --  into TR.
   procedure Load_TSS
     (TSS        : out Task_State.TSS_Type;
      Stack_Addr :     Word64);

   --  Create pseudo-descriptor from given descriptor table address and length.
   function Create_Descriptor
     (Table_Address : SK.Word64;
      Table_Length  : Descriptor_Table_Range)
      return Pseudo_Descriptor_Type;

   --  Returns a TSS Descriptor for given TSS address and limit, split in two
   --  64 bit words as specified by Intel SDM Vol. 3A, "7.2.3 TSS Descriptor in
   --  64-bit mode". The high and low values can be used as consecutive entries
   --  in the GDT.
   procedure Get_TSS_Descriptor
     (TSS_Address, TSS_Limit :     Word64;
      Low, High              : out Word64);

   --  Address getters.
   function Get_GDT_Addr (M : Manager_Type) return Word64;
   function Get_IDT_Addr (M : Manager_Type) return Word64;
   function Get_TSS_Addr (M : Manager_Type) return Word64;

   -------------------------------------------------------------------------

   function Create_Descriptor
     (Table_Address : SK.Word64;
      Table_Length  : Descriptor_Table_Range)
      return Pseudo_Descriptor_Type
   is
   begin
      return Pseudo_Descriptor_Type'
        (Limit => 16 * SK.Word16 (Table_Length) - 1,
         Base  => Table_Address);
   end Create_Descriptor;

   -------------------------------------------------------------------------

   function Get_GDT_Addr (M : Manager_Type) return Word64
   is (Word64 (System.Storage_Elements.To_Integer (M.GDT'Address)))
   with
      SPARK_Mode => Off;
   function Get_IDT_Addr (M : Manager_Type) return Word64
   is (Word64 (System.Storage_Elements.To_Integer (M.IDT'Address)))
   with
      SPARK_Mode => Off;
   function Get_TSS_Addr (M : Manager_Type) return Word64
   is (Word64 (System.Storage_Elements.To_Integer (M.TSS'Address)))
   with
      SPARK_Mode => Off;

   -------------------------------------------------------------------------

   procedure Get_Base_Addresses
     (GDT : out Word64;
      IDT : out Word64;
      TSS : out Word64)
   is
   begin
      GDT := Instance.GDT_Descriptor.Base;
      IDT := Instance.IDT_Descriptor.Base;
      TSS := Get_TSS_Addr (M => Instance);
   end Get_Base_Addresses;

   -------------------------------------------------------------------------

   procedure Get_TSS_Descriptor
     (TSS_Address, TSS_Limit :     Word64;
      Low, High              : out Word64)
   is
   begin
      Low := 16#0020_8900_0000_0000# or (TSS_Limit * 2 ** 48)
        or (TSS_Address and 16#00ff_ffff#) * 2 ** 16
        or (TSS_Address and 16#ff00_0000#) * 2 ** 56;
      High  := TSS_Address / 2 ** 32;
   end Get_TSS_Descriptor;

   -------------------------------------------------------------------------

   procedure Load_GDT
     (TSS_Addr       :     Word64;
      GDT_Addr       :     Word64;
      GDT            : out GDT_Type;
      GDT_Descriptor : out Pseudo_Descriptor_Type)
   is
      TSS_Desc_Low, TSS_Desc_High : Word64;
   begin
      Get_TSS_Descriptor
        (TSS_Address => TSS_Addr,
         TSS_Limit   => TSS_Type_Size / 8 - 1,
         Low         => TSS_Desc_Low,
         High        => TSS_Desc_High);

      GDT := GDT_Type'
        (1 => 0,
         2 => 16#20980000000000#, --  64-bit code segment
         3 => 16#20930000000000#, --  64-bit data segment
         4 => TSS_Desc_Low,
         5 => TSS_Desc_High);
      GDT_Descriptor := Create_Descriptor
        (Table_Address => GDT_Addr,
         Table_Length  => GDT'Length);
      CPU.Lgdt (Descriptor => GDT_Descriptor);
   end Load_GDT;

   -------------------------------------------------------------------------

   procedure Load_IDT
     (IDT_Addr       :     Word64;
      IDT_Length     :     Descriptor_Table_Range;
      IDT_Descriptor : out Pseudo_Descriptor_Type)
   is
   begin
      IDT_Descriptor := Create_Descriptor
        (Table_Address => IDT_Addr,
         Table_Length  => IDT_Length);
      CPU.Lidt (Descriptor => IDT_Descriptor);
   end Load_IDT;

   -------------------------------------------------------------------------

   procedure Load_TSS
     (TSS        : out Task_State.TSS_Type;
      Stack_Addr :     Word64)
   is
   begin
      TSS := Task_State.Null_TSS;

      Task_State.Set_IST_Entry
        (TSS_Data => TSS,
         Index    => 1,
         Address  => Stack_Addr);

      --  TSS is in GDT entry 3.

      CPU.Ltr (Address => 3 * 8);
   end Load_TSS;

   -------------------------------------------------------------------------

   procedure Initialize (Stack_Addr : Word64)
   with
      Refined_Global  => (Input  => ISRs,
                          In_Out => X86_64.State,
                          Output => Instance),
      Refined_Depends => (Instance => (Stack_Addr, ISRs, X86_64.State),
                          X86_64.State =>+ (Stack_Addr, ISRs))
   is
   begin
      Instance := (GDT            => (others => 0),
                   IDT            => (others => Descriptors.Null_Gate),
                   TSS            => Task_State.Null_TSS,
                   GDT_Descriptor => Null_Pseudo_Descriptor,
                   IDT_Descriptor => Null_Pseudo_Descriptor);

      Descriptors.Setup_IDT
        (ISRs => ISRs,
         IDT  => Instance.IDT,
         IST  => 1);
      Load_GDT
        (TSS_Addr       => Get_TSS_Addr (M => Instance),
         GDT_Addr       => Get_GDT_Addr (M => Instance),
         GDT            => Instance.GDT,
         GDT_Descriptor => Instance.GDT_Descriptor);
      Load_IDT
        (IDT_Addr       => Get_IDT_Addr (M => Instance),
         IDT_Length     => Instance.IDT'Length,
         IDT_Descriptor => Instance.IDT_Descriptor);
      Load_TSS (TSS        => Instance.TSS,
                Stack_Addr => Stack_Addr);
   end Initialize;

end SK.Interrupt_Tables;
