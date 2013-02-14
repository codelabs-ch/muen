--# inherit
--#    X86_64,
--#    SK.CPU;
package SK.VMX
--# own
--#    VMXON_Address,
--#    VMCS_Address;
--# initializes
--#    VMXON_Address,
--#    VMCS_Address;
is

   procedure Enable;
   --# global
   --#    in     VMXON_Address;
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, VMXON_Address;

   procedure Launch;
   --# global
   --#    in     VMCS_Address;
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, VMCS_Address;

end SK.VMX;
