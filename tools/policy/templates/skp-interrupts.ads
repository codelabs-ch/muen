with SK;

--# inherit SK, Skp;
package Skp.Interrupts is

   subtype Vector_Type is SK.Byte range 32 .. 255;

   type IRQ_Route_Type is record
      CPU    : Skp.CPU_Range;
      IRQ    : SK.Byte;
      Vector : Vector_Type;
   end record;

   type IRQ_Range is range __irq_range__;

   type IRQ_Routing_Array is array (IRQ_Range) of IRQ_Route_Type;

   IRQ_Routing : constant IRQ_Routing_Array := IRQ_Routing_Array'(
__irq_routing_table__);

end Skp.Interrupts;
