with SK;

--# inherit SK, Skp;
package Skp.Interrupts is

   subtype Vector_Type is SK.Byte range 32 .. 255;

   type IRQ_Route_Type is record
      CPU    : Skp.CPU_Range;
      IRQ    : SK.Byte;
      Vector : Vector_Type;
   end record;

   Null_IRQ_Route : constant IRQ_Route_Type := IRQ_Route_Type'
     (CPU    => 0,
      IRQ    => 0,
      Vector => 255);

   type IRQ_Range is range __irq_range__;

   type IRQ_Routing_Array is array (IRQ_Range) of IRQ_Route_Type;

   IRQ_Routing : constant IRQ_Routing_Array := IRQ_Routing_Array'(
__irq_routing_table__);

   Invalid_Subject : constant := Skp.Subject_Id_Type'Last + 1;

   subtype Vector_Dst_Type is Natural range 0 .. Invalid_Subject;

   type Vector_Routing_Array is array (Vector_Type) of Vector_Dst_Type;

   Vector_Routing : constant Vector_Routing_Array := Vector_Routing_Array'(
__vector_routing_table__);

end Skp.Interrupts;
