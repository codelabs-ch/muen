with System;

with SK.CPU;
with SK.Console;

package body SK.VMX
is

   IA32_VMX_BASIC : constant := 16#480#;

   subtype Alignment_Type is SK.Word16 range 1 .. SK.Word16'Last;

   --# accept Warning, 350, VMXON_Address, "Imported from Linker";
   VMXON_Address : SK.Word64;
   pragma Import (C, VMXON_Address, "vmxon_pointer");
   --# end accept;

   --# accept Warning, 350, VMCS_Address, "Imported from Linker";
   VMCS_Address : SK.Word64;
   pragma Import (C, VMCS_Address, "vmcs_pointer");
   --# end accept;

   ---------------------------------------------------------------------------

   --  Check alignment of given address.
   function Is_Aligned
     (Address   : SK.Word64;
      Alignment : Alignment_Type)
      return Boolean
   is
   begin
      return (Address mod SK.Word64 (Alignment)) = 0;
   end Is_Aligned;

   -------------------------------------------------------------------------

   procedure Enable
   is
      Success : Boolean;
   begin
      Success := Is_Aligned
        (Address   => VMXON_Address,
         Alignment => 4096);
      if not Success then
         pragma Debug (SK.Console.Put_Line ("VMXON region alignment invalid"));
         CPU.Panic;
      end if;

      CPU.Set_CR4 (Value => SK.Bit_Set
                   (Value => CPU.Get_CR4,
                    Pos   => CPU.CR4_VMXE_FLAG));

      CPU.VMXON (Region  => VMXON_Address,
                 Success => Success);
      if not Success then
         pragma Debug (SK.Console.Put_Line (Item => "Error enabling VMX"));
         CPU.Panic;
      end if;
   end Enable;

   -------------------------------------------------------------------------

   procedure Launch
   is
      --# hide Launch;
   begin
      CPU.VMLAUNCH;

      if SK.Bit_Test
        (Value => CPU.Get_RFLAGS,
         Pos   => CPU.RFLAGS_CF_FLAG)
      then
         pragma Debug
           (SK.Console.Put_Line ("Error launching VM (VMfailInvalid)"));
         CPU.Panic;
      elsif SK.Bit_Test
          (Value => CPU.Get_RFLAGS,
           Pos   => CPU.RFLAGS_ZF_FLAG)
      then
         pragma Debug
           (SK.Console.Put_Line ("Error launching VM (VMfailValid"));
         CPU.Panic;
      end if;
   end Launch;

begin

   --# hide SK.VMX;

   declare
      Revision, Unused_High, VMXON_Region, VMCS_Region : SK.Word32;
      for VMXON_Region'Address use System'To_Address (VMXON_Address);
      for VMCS_Region'Address use System'To_Address (VMCS_Address);
   begin
      CPU.Get_MSR
        (Register => IA32_VMX_BASIC,
         Low      => Revision,
         High     => Unused_High);

      VMXON_Region := Revision;
      VMCS_Region  := Revision;
   end;
end SK.VMX;
