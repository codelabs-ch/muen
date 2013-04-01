with System.Machine_Code;
with System.Storage_Elements;

with SK.Descriptors;

package body SubjC
is

   --  Assembly ISR.
   ISR_Handler : SK.Word64;
   pragma Import (C, ISR_Handler, "isr_handler_ptr");

   IDT : SK.Descriptors.IDT_Type;

   --  Interrupt table pointer, loaded into IDTR
   IDT_Pointer : SK.Descriptors.Pseudo_Descriptor_Type;

   type GDT_Type is array (1 .. 3) of SK.Word64;

   GDT : GDT_Type;
   for GDT'Alignment use 8;

   --  Global descriptor table pointer, loaded into GDTR
   GDT_Pointer : SK.Descriptors.Pseudo_Descriptor_Type;

   -------------------------------------------------------------------------

   procedure Increment_Counter
   is
      use type SK.Word64;
   begin
      Counter := Counter + 1;
   end Increment_Counter;

   -------------------------------------------------------------------------

   procedure Initialize
   is
      use type SK.Word16;
      use type SK.Word64;
   begin
      Counter := 0;

      GDT := GDT_Type'(1 => 0,
                       2 => 16#20980000000000#,
                       3 => 16#20930000000000#);
      GDT_Pointer := SK.Descriptors.Pseudo_Descriptor_Type'
        (Limit => 16 * SK.Word16 (GDT'Last) - 1,
         Base  => SK.Word64
           (System.Storage_Elements.To_Integer (Value => GDT'Address)));
      System.Machine_Code.Asm
        (Template => "lgdt (%0)",
         Inputs   => (System.Address'Asm_Input ("r", GDT_Pointer'Address)),
         Volatile => True);

      IDT := SK.Descriptors.IDT_Type'
        (others => SK.Descriptors.Gate_Type'
           (Offset_15_00     => 0,
            Segment_Selector => 0,
            Flags            => 0,
            Offset_31_16     => 0,
            Offset_63_32     => 0,
            Reserved         => 0));

      for I in SK.Descriptors.Vector_Type range IDT'Range loop
         IDT (I) := SK.Descriptors.Gate_Type'
           (Offset_15_00     => SK.Word16
              (ISR_Handler and 16#0000_0000_0000_ffff#),
            Segment_Selector => 16#0008#,
            Flags            => 16#8e00#,
            Offset_31_16     => SK.Word16
              ((ISR_Handler and 16#0000_0000_ffff_0000#) / 2 ** 16),
            Offset_63_32     => SK.Word32
              ((ISR_Handler and 16#ffff_ffff_0000_0000#) / 2 ** 32),
            Reserved         => 0);
      end loop;

      IDT_Pointer := SK.Descriptors.Pseudo_Descriptor_Type'
        (Limit => 16 * SK.Word16 (IDT'Last) - 1,
         Base  => SK.Word64
           (System.Storage_Elements.To_Integer (Value => IDT'Address)));
      System.Machine_Code.Asm
        (Template => "lidt (%0)",
         Inputs   => (System.Address'Asm_Input ("r", IDT_Pointer'Address)),
         Volatile => True);
   end Initialize;

end SubjC;
