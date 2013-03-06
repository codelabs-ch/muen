with System;

with SK.Constants;

package body SK.Subjects
is

   type Subject_Array is array (Index_Type) of State_Type;

   --  Descriptors used to manage subjects.
   Descriptors : Subject_Array;

   --# accept Warning, 350, Guest_Stack_Address, "Imported from Linker";
   Guest_Stack_Address : SK.Word64;
   pragma Import (C, Guest_Stack_Address, "guest_stack_pointer");
   --# end accept;

   --# accept Warning, 350, VMCS_Address, "Imported from Linker";
   VMCS_Address : SK.Word64;
   pragma Import (C, VMCS_Address, "vmcs_pointer");
   --# end accept;

   -------------------------------------------------------------------------

   function Get_State (Idx : Index_Type) return State_Type
   is
   begin
      return Descriptors (Idx);
   end Get_State;

   -------------------------------------------------------------------------

   procedure Set_State
     (Idx   : Index_Type;
      State : State_Type)
   is
   begin
      Descriptors (Idx) := State;
   end Set_State;

begin

   --# hide SK.Subjects;

   declare
      Revision, Unused_High, VMCS_Region0, VMCS_Region1 : SK.Word32;
      for VMCS_Region0'Address use System'To_Address (VMCS_Address);
      for VMCS_Region1'Address use System'To_Address (VMCS_Address + 4096);
   begin
      CPU.Get_MSR
        (Register => Constants.IA32_VMX_BASIC,
         Low      => Revision,
         High     => Unused_High);
      VMCS_Region0 := Revision;
      VMCS_Region1 := Revision;

      Descriptors (Descriptors'First)
        := State_Type'
          (Launched      => False,
           Regs          => CPU.Null_Regs,
           Stack_Address => Guest_Stack_Address,
           VMCS_Address  => VMCS_Address,
           Entry_Point   => 16#104ad6#);
      Descriptors (Descriptors'Last)
        := State_Type'
          (Launched      => False,
           Regs          => CPU.Null_Regs,
           Stack_Address => Guest_Stack_Address - 4096,
           VMCS_Address  => VMCS_Address        + 4096,
           Entry_Point   => 16#105056#);
   end;
end SK.Subjects;
