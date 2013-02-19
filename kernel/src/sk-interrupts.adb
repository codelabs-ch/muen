with System.Machine_Code;
with System.Storage_Elements;

package body SK.Interrupts
is

   --  Interrupt vector number.
   subtype Vector_Type is SK.Byte range 0 .. 255;

   --  Address of ISR.
   subtype ISR_Address_Type is SK.Word64;

   --  ISR list type.
   type ISR_List_Type is array (Vector_Type) of ISR_Address_Type;

   --  ISR trampoline list.
   ISR_List : ISR_List_Type;
   pragma Import (C, ISR_List, "isrlist");
   --# assert ISR_List'Always_Valid;

   --  Interrupt gate descriptor.
   type Gate_Type is record
      Offset_15_00     : SK.Word16;
      Segment_Selector : SK.Word16;
      Flags            : SK.Word16;
      Offset_31_16     : SK.Word16;
      Offset_63_32     : SK.Word32;
      Reserved         : SK.Word32;
   end record;

   for Gate_Type use record
      Offset_15_00     at  0 range 0 .. 15;
      Segment_Selector at  2 range 0 .. 15;
      Flags            at  4 range 0 .. 15;
      Offset_31_16     at  6 range 0 .. 15;
      Offset_63_32     at  8 range 0 .. 31;
      Reserved         at 12 range 0 .. 31;
   end record;
   for Gate_Type'Size use 16 * 8;

   type IDT_Type is array (Vector_Type) of Gate_Type;

   --  IDT, see Intel SDM 3A, chapter 6.10.
   IDT : IDT_Type;
   for IDT'Alignment use 8;

   --  Interrupt table pointer, loaded into IDTR
   IDT_Pointer : Descriptors.Pseudo_Descriptor_Type;

   -------------------------------------------------------------------------

   function Get_IDT_Pointer return Descriptors.Pseudo_Descriptor_Type
   is
   begin
      return IDT_Pointer;
   end Get_IDT_Pointer;

   -------------------------------------------------------------------------

   procedure Init
   is
      Temp : SK.Word64;
   begin
      IDT := IDT_Type'
        (others => Gate_Type'
           (Offset_15_00     => 0,
            Segment_Selector => 0,
            Flags            => 0,
            Offset_31_16     => 0,
            Offset_63_32     => 0,
            Reserved         => 0));

      for I in Vector_Type range IDT'Range loop
         Temp := ISR_List (I);
         --# check Temp in SK.Word64;

         IDT (I) := Gate_Type'
           (Offset_15_00     => SK.Word16
              (Temp and 16#0000_0000_0000_ffff#),
            Segment_Selector => 16#0008#,
            Flags            => 16#8e00#,
            Offset_31_16     => SK.Word16
              ((Temp and 16#0000_0000_ffff_0000#) / 2**16),
            Offset_63_32     => SK.Word32
              ((Temp and 16#ffff_ffff_0000_0000#) / 2**32),
            Reserved         => 0);
      end loop;
   end Init;

   -------------------------------------------------------------------------

   procedure Load
   is
      --# hide Load;
   begin
      IDT_Pointer := Descriptors.Pseudo_Descriptor_Type'
        (Limit => SK.Word16 (16 * IDT'Last) - 1,
         Base  => SK.Word64
           (System.Storage_Elements.To_Integer (Value => IDT'Address)));
      System.Machine_Code.Asm
        (Template => "lidt (%0)",
         Inputs   => (System.Address'Asm_Input ("r", IDT_Pointer'Address)),
         Volatile => True);
   end Load;

end SK.Interrupts;
