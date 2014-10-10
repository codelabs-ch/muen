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

with System.Machine_Code;
with System.Storage_Elements;

with Skp;

with SK.CPU;
with SK.TSS;
with SK.Descriptors;

package body Interrupts
with
   SPARK_Mode => Off
is

   subtype ISR_Array is SK.Descriptors.ISR_Array (Skp.Vector_Range);
   ISRs : ISR_Array
     with
       Import,
       Convention => C,
       Link_Name  => "isrlist";

   subtype IDT_Type is SK.Descriptors.IDT_Type (Skp.Vector_Range);
   IDT : IDT_Type := (others => SK.Descriptors.Null_Gate);

   --  IDT descriptor, loaded into IDTR.
   IDT_Descriptor : SK.Descriptors.Pseudo_Descriptor_Type;

   type GDT_Type is array (1 .. 5) of SK.Word64;
   GDT : GDT_Type
     with
       Alignment => 8;

   --  GDT descriptor, loaded into GDTR.
   GDT_Descriptor : SK.Descriptors.Pseudo_Descriptor_Type;

   --  Task-State Segment needed for stack switching.
   TSS : SK.TSS.TSS_Type := SK.TSS.Null_TSS;

   --  Setup GDT with two entries (code & stack) and load it into GDTR.
   procedure Load_GDT;

   --  Load IDT into IDTR.
   procedure Load_IDT;

   --  Returns a TSS Descriptor split in two 64 bit words as specified by Intel
   --  SDM Vol. 3A, section 7.2.3. The high and low values can be used as
   --  consecutive entries in the GDT.
   procedure Get_TSS_Descriptor (Low, High : out SK.Word64);

   --  Setup TSS with an RSP entry and load it into TR.
   procedure Load_TSS;

   -------------------------------------------------------------------------

   procedure Get_TSS_Descriptor (Low, High : out SK.Word64)
   is
      use type SK.Word64;

      TSS_Address : constant SK.Word64 := SK.Word64
        (System.Storage_Elements.To_Integer (Value => TSS'Address));
      Limit       : SK.Word64;
   begin
      Limit := TSS'Size / 8 - 1;
      Low   := 16#0020_8900_0000_0000# or (Limit * 2 ** 47);
      Low   := Low or (TSS_Address and 16#00ff_ffff#) * 2 ** 16;
      Low   := Low or (TSS_Address and 16#ff00_0000#) * 2 ** 55;
      High  := TSS_Address / 2 ** 32;
   end Get_TSS_Descriptor;

   -------------------------------------------------------------------------

   procedure Initialize
   is
   begin
      SK.Descriptors.Setup_IDT (ISRs => ISRs,
                                IDT  => IDT,
                                IST  => 1);
      Load_GDT;
      Load_IDT;
      Load_TSS;
   end Initialize;

   -------------------------------------------------------------------------

   procedure Load_GDT
   is
      use type SK.Word64;

      TSS_Desc_Low, TSS_Desc_High : SK.Word64;
   begin
      Get_TSS_Descriptor (Low  => TSS_Desc_Low,
                          High => TSS_Desc_High);

      GDT := GDT_Type'(1 => 0,
                       2 => 16#20980000000000#,
                       3 => 16#20930000000000#,
                       4 => TSS_Desc_Low,
                       5 => TSS_Desc_High);
      GDT_Descriptor := SK.Descriptors.Create_Descriptor
        (Table_Address => SK.Word64
           (System.Storage_Elements.To_Integer (Value => GDT'Address)),
         Table_Length  => GDT'Length);
      System.Machine_Code.Asm
        (Template => "lgdt (%0)",
         Inputs   => (System.Address'Asm_Input ("r", GDT_Descriptor'Address)),
         Volatile => True);
   end Load_GDT;

   -------------------------------------------------------------------------

   procedure Load_IDT
   is
   begin
      IDT_Descriptor := SK.Descriptors.Create_Descriptor
        (Table_Address => SK.Word64
           (System.Storage_Elements.To_Integer (Value => IDT'Address)),
         Table_Length  => IDT'Length);
      SK.CPU.Lidt
        (Address => SK.Word64
           (System.Storage_Elements.To_Integer
              (Value => IDT_Descriptor'Address)));
   end Load_IDT;

   -------------------------------------------------------------------------

   procedure Load_TSS
   is
      use type SK.Word16;
   begin
      SK.TSS.Set_IST_Entry
        (TSS_Data => TSS,
         Index    => 1,
         Address  => 16#5000#);

      --  TSS is in GDT entry 3.

      SK.CPU.Ltr (Address => 3 * 8);
   end Load_TSS;

end Interrupts;
