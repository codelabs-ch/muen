with System.Machine_Code;
with System.Storage_Elements;

with SK.IO;

package body SK.Interrupts
is

   --  ISR list type.
   type ISR_List_Type is array (Descriptors.Vector_Type) of SK.Word64;

   --  ISR trampoline list.
   ISR_List : ISR_List_Type;
   pragma Import (C, ISR_List, "isrlist");
   --# assert ISR_List'Always_Valid;

   --  IDT, see Intel SDM 3A, chapter 6.10.
   IDT : Descriptors.IDT_Type;

   --  Interrupt table pointer, loaded into IDTR
   IDT_Pointer : Descriptors.Pseudo_Descriptor_Type;

   -------------------------------------------------------------------------

   procedure Disable_Legacy_PIC
   is
   begin

      --  Disable slave.

      IO.Outb (Port  => 16#a1#,
               Value => 16#ff#);

      --  Disable master.

      IO.Outb (Port  => 16#21#,
               Value => 16#ff#);
   end Disable_Legacy_PIC;

   -------------------------------------------------------------------------

   function Get_IDT_Pointer return Descriptors.Pseudo_Descriptor_Type
   is
   begin
      return IDT_Pointer;
   end Get_IDT_Pointer;

   -------------------------------------------------------------------------

   procedure Load
   is
      --# hide Load;
   begin
      System.Machine_Code.Asm
        (Template => "lidt (%0)",
         Inputs   => (System.Address'Asm_Input ("r", IDT_Pointer'Address)),
         Volatile => True);
   end Load;

begin

   --# hide SK.Interrupts;

   declare
      Temp : SK.Word64;
   begin
      IDT := Descriptors.IDT_Type'
        (others => Descriptors.Gate_Type'
           (Offset_15_00     => 0,
            Segment_Selector => 0,
            Flags            => 0,
            Offset_31_16     => 0,
            Offset_63_32     => 0,
            Reserved         => 0));

      for I in Descriptors.Vector_Type range IDT'Range loop
         Temp := ISR_List (I);

         IDT (I) := Descriptors.Gate_Type'
           (Offset_15_00     => SK.Word16
              (Temp and 16#0000_0000_0000_ffff#),
            Segment_Selector => 16#0008#,
            Flags            => 16#8e00#,
            Offset_31_16     => SK.Word16
              ((Temp and 16#0000_0000_ffff_0000#) / 2 ** 16),
            Offset_63_32     => SK.Word32
              ((Temp and 16#ffff_ffff_0000_0000#) / 2 ** 32),
            Reserved         => 0);
      end loop;

      IDT_Pointer := Descriptors.Pseudo_Descriptor_Type'
        (Limit => 16 * SK.Word16 (IDT'Last) - 1,
         Base  => SK.Word64
           (System.Storage_Elements.To_Integer (Value => IDT'Address)));
   end;
end SK.Interrupts;
