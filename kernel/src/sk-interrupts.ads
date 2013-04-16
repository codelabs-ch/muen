with SK.Descriptors;

--# inherit
--#    System,
--#    SK.Descriptors,
--#    X86_64;
package SK.Interrupts
--# own
--#       IDT, IDT_Pointer;
--#    in ISR_List : ISR_List_Type;
--# initializes
--#    IDT, IDT_Pointer;
is

   --  Load IDT into IDT register.
   procedure Load;
   --# global
   --#    in     IDT;
   --#    in     IDT_Pointer;
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, IDT, IDT_Pointer;

   --  Return IDT pointer.
   function Get_IDT_Pointer return Descriptors.Pseudo_Descriptor_Type;
   --# global
   --#    IDT_Pointer;

end SK.Interrupts;
