with System;

with SK.CPU;
with SK.Console;

package body SK.VMX
is

   IA32_VMX_BASIC : constant := 16#480#;

   --# accept Warning, 350, VMXON_Address, "Imported from Linker";
   VMXON_Address : SK.Word64;
   pragma Import (C, VMXON_Address, "vmxon_pointer");
   --# end accept;

   -------------------------------------------------------------------------

   procedure Enable
   is
      --# hide Enable;

      Success : Boolean;
   begin
      CPU.VMXON
        (Region  => VMXON_Address,
         Success => Success);
      if not Success then
         pragma Debug (SK.Console.Put_Line ("VMXON failed"));
         null;
      end if;
   end Enable;

begin

   --# hide SK.VMX;

   declare
      VMXON_Region, Unused_High : SK.Word32;
      for VMXON_Region'Address use System'To_Address (VMXON_Address);
   begin
      CPU.Get_MSR
        (Register => IA32_VMX_BASIC,
         Low      => VMXON_Region,
         High     => Unused_High);
   end;
end SK.VMX;
