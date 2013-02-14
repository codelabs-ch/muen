with System;

with SK.CPU;
with SK.Console;

package body SK.VMX
is

   IA32_VMX_BASIC : constant := 16#480#;
   VMX_INST_ERROR : constant := 16#4400#;

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
      Success : Boolean;
      Error   : SK.Word64;
   begin
      Success := Is_Aligned
        (Address   => VMCS_Address,
         Alignment => 4096);
      if not Success then
         pragma Debug (SK.Console.Put_Line ("VMCS region alignment invalid"));
         CPU.Panic;
      end if;

      CPU.VMCLEAR (Region  => VMCS_Address,
                   Success => Success);
      if not Success then
         pragma Debug (SK.Console.Put_Line (Item => "Error clearing VMCS"));
         CPU.Panic;
      end if;

      CPU.VMPTRLD (Region  => VMCS_Address,
                   Success => Success);
      if not Success then
         pragma Debug
           (SK.Console.Put_Line (Item => "Error loading VMCS pointer"));
         CPU.Panic;
      end if;

      CPU.VMLAUNCH (Success => Success);
      if not Success then
         pragma Debug (CPU.VMREAD (Field   => VMX_INST_ERROR,
                                   Value   => Error,
                                   Success => Success));
         pragma Debug
           (SK.Console.Put_String (Item => "Error launching VM ("));
         pragma Debug (SK.Console.Put_Byte (Item => Byte (Error)));
         pragma Debug (SK.Console.Put_Line (Item => ")"));
         CPU.Panic;
      end if;
      --# accept Warning, 400, Error, "Only used for debug output";
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
