--# inherit Skp;
package Skp.Interrupts is

   type IRQ_Type    is range 0  .. 255;
   type Vector_Type is range 32 .. 255;

   type IRQ_Route_Type is record
      CPU    : Skp.CPU_Range;
      IRQ    : IRQ_Type;
      Vector : Vector_Type;
   end record;

   type IRQ_Range is range __irq_range__;

   type IRQ_Routing_Array is array (IRQ_Range) of IRQ_Route_Type;

   IRQ_Routing : constant IRQ_Routing_Array := IRQ_Routing_Array'(
__irq_routing_table__);

end Skp.Interrupts;
