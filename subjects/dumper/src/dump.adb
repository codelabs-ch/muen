with System.Machine_Code;
with System.Storage_Elements;

with SK.CPU;
with SK.Descriptors;

package body Dump
is

   --  ISR list type.
   type ISR_List_Type is array (SK.Descriptors.Vector_Type) of SK.Word64;

   --  ISR trampoline list.
   ISRs : ISR_List_Type;
   pragma Import (C, ISRs, "isrlist");
   --# assert ISR_List'Always_Valid;

   --  IDT, see Intel SDM Vol. 3A, chapter 6.10.
   IDT : SK.Descriptors.IDT_Type;

   type GDT_Type is array (1 .. 3) of SK.Word64;

   GDT : GDT_Type;
   for GDT'Alignment use 8;

   --  Global descriptor table pointer, loaded into GDTR
   GDT_Pointer : SK.Descriptors.Pseudo_Descriptor_Type;

   --  Load GDT with two entries (code & stack) and load it into GDT register.
   procedure Load_GDT;

   --  Load IDT into IDT register.
   procedure Load_IDT (IDT : SK.Descriptors.IDT_Type);

   --  Setup IDT using the given ISR list.
   procedure Setup_IDT
     (ISR_List :     ISR_List_Type;
      IDT      : out SK.Descriptors.IDT_Type);

   -------------------------------------------------------------------------

   procedure Handle_Interrupt (Vector : SK.Byte)
   is
   begin
      Current_Subject := Integer (Vector) - 32;
   end Handle_Interrupt;

   -------------------------------------------------------------------------

   procedure Initialize
   is
   begin
      Setup_IDT (ISR_List => ISRs,
                 IDT      => IDT);
      Load_IDT (IDT => IDT);
      Load_GDT;
   end Initialize;

   -------------------------------------------------------------------------

   procedure Load_GDT
   is
      use type SK.Word16;
   begin
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
   end Load_GDT;

   -------------------------------------------------------------------------

   procedure Load_IDT (IDT : SK.Descriptors.IDT_Type)
   is
      use type SK.Word16;

      IDT_Pointer : SK.Descriptors.Pseudo_Descriptor_Type;
   begin
      IDT_Pointer := SK.Descriptors.Pseudo_Descriptor_Type'
        (Limit => 16 * SK.Word16 (IDT'Length) - 1,
         Base  => SK.Word64
           (System.Storage_Elements.To_Integer (Value => IDT'Address)));
      SK.CPU.Lidt
        (Address => SK.Word64
           (System.Storage_Elements.To_Integer
              (Value => IDT_Pointer'Address)));
   end Load_IDT;

   -------------------------------------------------------------------------

   procedure Setup_IDT
     (ISR_List :     ISR_List_Type;
      IDT      : out SK.Descriptors.IDT_Type)
   is
      use type SK.Word64;

      Temp : SK.Word64;
   begin
      IDT := SK.Descriptors.IDT_Type'
        (others => SK.Descriptors.Gate_Type'
           (Offset_15_00     => 0,
            Segment_Selector => 0,
            Flags            => 0,
            Offset_31_16     => 0,
            Offset_63_32     => 0,
            Reserved         => 0));

      for I in SK.Descriptors.Vector_Type range IDT'Range loop
         Temp := ISR_List (I);

         IDT (I) := SK.Descriptors.Gate_Type'
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
   end Setup_IDT;

end Dump;
