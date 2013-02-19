with System;

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


   --  Interrupt Descriptor table pointer, see Intel SDM vol. 3A, chapter 6.10.
   type IDT_Pointer_Type is record
      Limit : SK.Word16;
      Base  : System.Address;
   end record;

private

   for IDT_Pointer_Type use record
      Limit at 0 range 0 .. 15;
      Base  at 2 range 0 .. 63;
   end record;
   for IDT_Pointer_Type'Size use 80;

end SK.Interrupts;
