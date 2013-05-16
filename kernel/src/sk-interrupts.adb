with System.Storage_Elements;

with Skp.Interrupts;

with SK.CPU;
with SK.IO;
with SK.IO_Apic;

package body SK.Interrupts
is

   subtype Exception_Range is Skp.Vector_Range range 0 .. 19;

   --  ISR list type.
   type ISR_List_Type is array (Exception_Range) of SK.Word64;

   --  ISR trampoline list.
   ISR_List : ISR_List_Type;
   pragma Import (C, ISR_List, "isrlist");
   --# assert ISR_List'Always_Valid;

   subtype IDT_Type is Descriptors.IDT_Type (Exception_Range);

   --  IDT, see Intel SDM 3A, chapter 6.10.
   IDT : IDT_Type;

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
      CPU.Lidt (Address => SK.Word64 (System.Storage_Elements.To_Integer
                (Value => IDT_Pointer'Address)));
   end Load;

   -------------------------------------------------------------------------

   procedure Setup_IRQ_Routing
   is
   begin
      for I in Skp.Interrupts.Routing_Range loop
         IO_Apic.Route_IRQ
           (IRQ            => Skp.Interrupts.IRQ_Routing (I).IRQ,
            Vector         => Skp.Interrupts.IRQ_Routing (I).Vector,
            Trigger_Mode   => IO_Apic.Edge,
            Destination_Id => SK.Byte (Skp.Interrupts.IRQ_Routing (I).CPU));
      end loop;
   end Setup_IRQ_Routing;

begin

   --# hide SK.Interrupts;

   for I in Exception_Range loop
      IDT (I) := Descriptors.Gate_Type'
        (Offset_15_00     => SK.Word16
           (ISR_List (I) and 16#0000_0000_0000_ffff#),
         Segment_Selector => 16#0008#,
         Flags            => 16#8e00#,
         Offset_31_16     => SK.Word16
           ((ISR_List (I) and 16#0000_0000_ffff_0000#) / 2 ** 16),
         Offset_63_32     => SK.Word32
           ((ISR_List (I) and 16#ffff_ffff_0000_0000#) / 2 ** 32),
         Reserved         => 0);
   end loop;

   IDT_Pointer := Descriptors.Pseudo_Descriptor_Type'
     (Limit => 16 * SK.Word16 (IDT'Length) - 1,
      Base  => SK.Word64
        (System.Storage_Elements.To_Integer (Value => IDT'Address)));
end SK.Interrupts;
