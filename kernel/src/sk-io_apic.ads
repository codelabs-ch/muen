--# inherit
--#    SK;
package SK.IO_Apic
--# own
--#    State;
--# initializes
--#    State;
is

   --  Interrupt trigger mode.
   type Trigger_Kind is (Edge, Level);

   --  Route IRQ as interrupt with specified vector to APIC given by
   --  destination id.
   procedure Route_IRQ
     (IRQ            : SK.Byte;
      Vector         : SK.Byte;
      Trigger_Mode   : Trigger_Kind;
      Destination_Id : SK.Byte);
   --# global
   --#    in out State;
   --# derives
   --#    State from *, IRQ, Vector, Trigger_Mode, Destination_Id;

   --  Mask/disable interrupt delivery for specified IRQ.
   procedure Mask_Interrupt (IRQ : SK.Byte);
   --# global
   --#    in out State;
   --# derives
   --#    State from *, IRQ;

   --  Unmask/enable interrupt delivery for specified IRQ.
   procedure Unmask_Interrupt (IRQ : SK.Byte);
   --# global
   --#    in out State;
   --# derives
   --#    State from *, IRQ;

end SK.IO_Apic;
