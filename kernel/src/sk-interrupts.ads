with SK.Descriptors;

--# inherit
--#    System,
--#    SK.Descriptors;
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

   --  Return IDT pointer.
   function Get_IDT_Pointer return Descriptors.Pseudo_Descriptor_Type;
   --# global
   --#    IDT_Pointer;

end SK.Interrupts;
