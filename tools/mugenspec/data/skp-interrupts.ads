with SK;

package Skp.Interrupts
is

   Remap_Offset : constant := 32;

   subtype Remapped_Vector_Type is Skp.Dst_Vector_Range range
     Remap_Offset .. Skp.Dst_Vector_Range'Last;

   type IRQ_Mode_Type is (Edge, Level);

   type IRQ_Level_Type is (High, Low);

   type IRQ_Route_Type is record
      CPU       : Skp.CPU_Range;
      IRQ       : SK.Byte;
      IRQ_Mode  : IRQ_Mode_Type;
      IRQ_Level : IRQ_Level_Type;
      Vector    : Remapped_Vector_Type;
   end record;

   Null_IRQ_Route : constant IRQ_Route_Type := IRQ_Route_Type'
     (CPU       => 0,
      IRQ       => 0,
      IRQ_Mode  => Edge,
      IRQ_Level => High,
      Vector    => Skp.Invalid_Vector);

   type Routing_Range is range 1 .. 3;

   type IRQ_Routing_Array is array (Routing_Range) of IRQ_Route_Type;

   IRQ_Routing : constant IRQ_Routing_Array := IRQ_Routing_Array'(
       1 => IRQ_Route_Type'(
         CPU       => 1,
         IRQ       => 12,
         IRQ_Mode  => Edge,
         IRQ_Level => High,
         Vector    => 44),
       2 => IRQ_Route_Type'(
         CPU       => 1,
         IRQ       => 15,
         IRQ_Mode  => Edge,
         IRQ_Level => High,
         Vector    => 47),
       3 => IRQ_Route_Type'(
         CPU       => 1,
         IRQ       => 22,
         IRQ_Mode  => Level,
         IRQ_Level => Low,
         Vector    => 54));

   type Vector_Route_Type is record
      Subject : Skp.Dst_Subject_Type;
      Vector  : Skp.Vector_Range;
   end record;

   Null_Vector_Route : constant Vector_Route_Type := Vector_Route_Type'
     (Subject => Skp.Invalid_Subject,
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
