with SK.Descriptors;

--# inherit
--#    System,
--#    X86_64,
--#    SK.IO,
--#    SK.Descriptors;
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

   --  Mask all interrupts in the legacy PIC.
   procedure Disable_Legacy_PIC;
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *;

end SK.Interrupts;
