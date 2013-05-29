package body SK.Descriptors
is

   -------------------------------------------------------------------------

   procedure Setup_IDT
     (ISRs :     ISR_Array;
      IDT  : in out IDT_Type)
   is
   begin
      for I in Skp.Vector_Range range ISRs'Range loop
         IDT (I) := Gate_Type'
           (Offset_15_00     => SK.Word16
              (ISRs (I) and 16#0000_0000_0000_ffff#),
            Segment_Selector => 16#0008#,
            Flags            => 16#8e00#,
            Offset_31_16     => SK.Word16
              ((ISRs (I) and 16#0000_0000_ffff_0000#) / 2 ** 16),
            Offset_63_32     => SK.Word32
              ((ISRs (I) and 16#ffff_ffff_0000_0000#) / 2 ** 32),
            Reserved         => 0);
      end loop;
   end Setup_IDT;

end SK.Descriptors;
