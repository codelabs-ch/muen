with SK.Descriptors;

--# inherit
--#    System,
--#    Skp.Interrupts,
--#    X86_64,
--#    SK.IO,
--#    SK.IO_Apic,
--#    SK.Descriptors;
package SK.Interrupts
--# own
--#    State;
--# initializes
--#    State;
is

   --  Initalize IDT structure.
   procedure Init;
   --# global
   --#    in out State;
   --# derives
   --#    State from *;

   --  Load IDT into IDT register.
   procedure Load;
   --# global
   --#    in     State;
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, State;

   --  Return IDT pointer.
   function Get_IDT_Pointer return Descriptors.Pseudo_Descriptor_Type;
   --# global
   --#    State;

   --  Mask all interrupts in the legacy PIC.
   procedure Disable_Legacy_PIC;
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *;

   --  Setup I/O APIC IRQ routing.
   procedure Setup_IRQ_Routing;
   --# global
   --#    in out IO_Apic.State;
   --# derives
   --#    IO_Apic.State from *;

end SK.Interrupts;
