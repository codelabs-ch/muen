with System;

with Config_Subjects.Bins;

with SK.Constants;

package body SK.Subjects
is

   type Subject_Array is array (Index_Type) of State_Type;

   --  Descriptors used to manage subjects.
   Descriptors : Subject_Array;

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

      for S in Config_Subjects.Bins.Subjects'Range loop
         Descriptors (Index_Type (S - 1))
           := State_Type'
             (Launched      => False,
              Regs          => CPU.Null_Regs,
              Stack_Address => Config_Subjects.Bins.Subjects (S).Stack_Address,
              VMCS_Address  => VMCS_Address,
              Entry_Point   => Config_Subjects.Bins.Subjects (S).Entry_Point);

         VMCS_Address := VMCS_Address + 4096;
      end loop;
   end;
end SK.Subjects;
