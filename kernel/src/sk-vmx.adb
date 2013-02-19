with System;

with SK.CPU;
with SK.Console;

package body SK.VMX
is

   IA32_VMX_BASIC          : constant := 16#480#;
   IA32_VMX_PINBASED_CTLS  : constant := 16#481#;
   IA32_VMX_PROCBASED_CTLS : constant := 16#482#;
   IA32_VMX_EXIT_CTLS      : constant := 16#483#;
   IA32_VMX_ENTRY_CTLS     : constant := 16#484#;

   PIN_BASED_EXEC_CONTROL : constant := 16#4000#;
   CPU_BASED_EXEC_CONTROL : constant := 16#4002#;
   VM_EXIT_CONTROLS       : constant := 16#400c#;
   VM_ENTRY_CONTROLS      : constant := 16#4012#;
   VMX_INST_ERROR         : constant := 16#4400#;

   --  VM-Exit controls
   VM_EXIT_IA32E_MODE : constant := 16#200#;

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

   --  Write given value to the specified field of the current, active VMCS. If
   --  the operation fails, CPU.Panic is called.
   procedure VMCS_Write
     (Field : SK.Word16;
      Value : SK.Word64)
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, Field, Value;
   is
      Success : Boolean;
   begin
      CPU.VMWRITE (Field   => SK.Word64 (Field),
                   Value   => Value,
                   Success => Success);
      if not Success then
         pragma Debug (SK.Console.Put_String
                       (Item => "Error setting VMCS field "));
         pragma Debug (SK.Console.Put_Word16 (Item => Field));
         pragma Debug (SK.Console.Put_String (Item => " to value "));
         pragma Debug (SK.Console.Put_Word64 (Item => Value));
         pragma Debug (SK.Console.New_Line);
         CPU.Panic;
      end if;
   end VMCS_Write;

   -------------------------------------------------------------------------

   procedure VMCS_Setup_Control_Fields
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *;
   is
      Default0, Default1 : SK.Word32;
      Value              : SK.Word64;
   begin
      CPU.Get_MSR (Register => IA32_VMX_PINBASED_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := 0;
      Value := Value and SK.Word64 (Default1);
      Value := Value or  SK.Word64 (Default0);
      VMCS_Write (Field => PIN_BASED_EXEC_CONTROL,
                  Value => Value);

      CPU.Get_MSR (Register => IA32_VMX_PROCBASED_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := 0;
      Value := Value and SK.Word64 (Default1);
      Value := Value or  SK.Word64 (Default0);
      VMCS_Write (Field => CPU_BASED_EXEC_CONTROL,
                  Value => Value);

      CPU.Get_MSR (Register => IA32_VMX_EXIT_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := VM_EXIT_IA32E_MODE;
      Value := Value and SK.Word64 (Default1);
      Value := Value or  SK.Word64 (Default0);
      VMCS_Write (Field => VM_EXIT_CONTROLS,
                  Value => Value);

      CPU.Get_MSR (Register => IA32_VMX_ENTRY_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := 0;
      Value := Value and SK.Word64 (Default1);
      Value := Value or  SK.Word64 (Default0);
      VMCS_Write (Field => VM_ENTRY_CONTROLS,
                  Value => Value);
   end VMCS_Setup_Control_Fields;

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

      VMCS_Setup_Control_Fields;

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
