--# inherit
--#    Ada.Unchecked_Conversion,
--#    System,
--#    SK;

package SK.Interrupts
--# own IDT, IDT_Pointer;
is

   --  Initialize Interrupt Descriptor Table.
   procedure Init;
   --# global
   --#    out IDT;
   --# derives
   --#    IDT from ;

   --  Load IDT into IDT register.
   procedure Load;
   --# global
   --#    in     IDT;
   --#       out IDT_Pointer;
   --# derives
   --#    IDT_Pointer from IDT;

private

   --  Interrupt service routine for #UD fault.
   procedure Isr_UD;
   --# derives ;

end SK.Interrupts;
