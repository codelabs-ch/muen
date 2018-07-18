with SK;

package Skp.Interrupts
is

   Remap_Offset : constant :=__remap_offset__;

   subtype Remapped_Vector_Type is Dst_Vector_Range range
     Remap_Offset .. Dst_Vector_Range'Last;

   type IRQ_Mode_Type is (Edge, Level);

   type IRQ_Level_Type is (High, Low);

   type RTE_Index_Type is range 1 .. 23;

   type IRQ_Route_Type is record
      APIC_ID   : APIC_ID_Type;
      RTE_Idx   : RTE_Index_Type;
      IRQ       : SK.Byte;
      IRQ_Mode  : IRQ_Mode_Type;
      IRQ_Level : IRQ_Level_Type;
      Vector    : Remapped_Vector_Type;
   end record;

   Null_IRQ_Route : constant IRQ_Route_Type := IRQ_Route_Type'
     (APIC_ID   => 0,
      RTE_Idx   => 1,
      IRQ       => 0,
      IRQ_Mode  => Edge,
      IRQ_Level => High,
      Vector    => Invalid_Vector);

   type Routing_Range is range __routing_range__;

   type IRQ_Routing_Array is array (Routing_Range) of IRQ_Route_Type;

   IRQ_Routing : constant IRQ_Routing_Array := IRQ_Routing_Array'(
__irq_routing_table__);

   type Vector_Route_Type is record
      Subject : Dst_Subject_Type;
      Vector  : Vector_Range;
   end record;

   Null_Vector_Route : constant Vector_Route_Type := Vector_Route_Type'
     (Subject => Invalid_Subject,
      Vector  => 0);

   type Vector_Routing_Array is array (Remapped_Vector_Type)
     of Vector_Route_Type;

   Vector_Routing : constant Vector_Routing_Array := Vector_Routing_Array'(
__vector_routing_table__);

end Skp.Interrupts;
