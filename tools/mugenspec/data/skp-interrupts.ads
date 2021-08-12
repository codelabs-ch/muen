with SK;

--D @Interface
--D This package contains IRQ and vector routing definitions as specified by
--D the system policy.
package Skp.Interrupts
is

   Remap_Offset : constant := 32;

   subtype Remapped_Vector_Type is Dst_Vector_Range range
     Remap_Offset .. Dst_Vector_Range'Last;

   type IRQ_Mode_Type is (Edge, Level);

   type IRQ_Level_Type is (High, Low);

   type RTE_Index_Type is range 1 .. 23;

   subtype Mask_IRQ_Type is Dst_Vector_Range
     with Static_Predicate => Mask_IRQ_Type in 54;

   type IRQ_Route_Type is record
      APIC_ID   : APIC_ID_Type;
      RTE_Idx   : RTE_Index_Type;
      IRQ       : SK.Byte;
      IRQ_Mode  : IRQ_Mode_Type;
      IRQ_Level : IRQ_Level_Type;
      Vector    : Remapped_Vector_Type;
   end record
   with
      Dynamic_Predicate =>
         (if IRQ_Route_Type.IRQ_Mode = Level then
             IRQ_Route_Type.Vector in Mask_IRQ_Type);

   Null_IRQ_Route : constant IRQ_Route_Type := IRQ_Route_Type'
     (APIC_ID   => 0,
      RTE_Idx   => 1,
      IRQ       => 0,
      IRQ_Mode  => Edge,
      IRQ_Level => High,
      Vector    => Invalid_Vector);

   type Routing_Range is range 1 .. 3;

   type IRQ_Routing_Array is array (Routing_Range) of IRQ_Route_Type;

   IRQ_Routing : constant IRQ_Routing_Array := IRQ_Routing_Array'(
       1 => IRQ_Route_Type'(
         APIC_ID   => 2,
         RTE_Idx   => 12,
         IRQ       => 12,
         IRQ_Mode  => Edge,
         IRQ_Level => High,
         Vector    => 44),
       2 => IRQ_Route_Type'(
         APIC_ID   => 2,
         RTE_Idx   => 15,
         IRQ       => 15,
         IRQ_Mode  => Edge,
         IRQ_Level => High,
         Vector    => 47),
       3 => IRQ_Route_Type'(
         APIC_ID   => 2,
         RTE_Idx   => 22,
         IRQ       => 22,
         IRQ_Mode  => Level,
         IRQ_Level => Low,
         Vector    => 54));

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
       44 => Vector_Route_Type'(
         Subject => 1,
         Vector  => 44),
       47 => Vector_Route_Type'(
         Subject => 1,
         Vector  => 47),
       54 => Vector_Route_Type'(
         Subject => 1,
         Vector  => 125),
       72 => Vector_Route_Type'(
         Subject => 1,
         Vector  => 126),
       others => Null_Vector_Route);

end Skp.Interrupts;
