--# inherit
--#    X86_64,
--#    SK.CPU,
--#    SK.Interrupts,
--#    SK.GDT,
--#    SK.Descriptors,
--#    SK.Constants;
package SK.VMX
--# own
--#    VMXON_Address,
--#    VMCS_Address;
--#    Subject_Main_Address;
--# initializes
--#    VMXON_Address,
--#    VMCS_Address,
--#    Subject_Main_Address;
is

   procedure Enable;
   --# global
   --#    in     VMXON_Address;
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, VMXON_Address;

   procedure Launch;
   --# global
   --#    in     GDT.GDT_Pointer;
   --#    in     Interrupts.IDT_Pointer;
   --#    in     VMCS_Address;
   --#    in     Subject_Main_Address;
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from
   --#       *,
   --#       Interrupts.IDT_Pointer,
   --#       GDT.GDT_Pointer,
   --#       VMCS_Address,
   --#       Subject_Main_Address;

end SK.VMX;
