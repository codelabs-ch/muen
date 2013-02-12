--# inherit
--#    System,
--#    SK;

package SK.Interrupts
--# own
--#       IDT, IDT_Pointer;
--#    in ISR_List : ISR_List_Type;
is

   --  Initialize Interrupt Descriptor Table.
   procedure Init;
   --# global
   --#    in     ISR_List;
   --#       out IDT;
   --# derives
   --#    IDT from ISR_List;

   --  Load IDT into IDT register.
   procedure Load;
   --# global
   --#    in     IDT;
   --#       out IDT_Pointer;
   --# derives
   --#    IDT_Pointer from IDT;

end SK.Interrupts;
