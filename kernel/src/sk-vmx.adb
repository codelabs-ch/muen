--
--  Copyright (C) 2013-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with System;

with Skp.Kernel;

with SK.CPU.VMX;
with SK.Dump;
with SK.KC;
with SK.Bitops;
with SK.Constants;
with SK.Strings;
with SK.Crash_Audit_Types;

private with SK.VMX.Error;

package body SK.VMX
with
   Refined_State => (VMCS_State => VMCS)
is

   --  Segment selectors

   SEL_KERN_CODE : constant := 16#08#;
   SEL_KERN_DATA : constant := 16#10#;
   SEL_TSS       : constant := 16#18#;

   --  VMCS region format, see Intel SDM Vol. 3C, section 24.2.
   type VMCS_Header_Type is record
      Revision_ID     : SK.Word32;
      Abort_Indicator : SK.Word32;
   end record
   with
      Size => 8 * 8;

   type VMCS_Data is array (1 .. SK.Page_Size - (VMCS_Header_Type'Size / 8))
     of SK.Byte;

   type VMCS_Region_Type is record
      Header : VMCS_Header_Type;
      Data   : VMCS_Data;
   end record
   with
      Alignment => SK.Page_Size,
      Size      => SK.Page_Size * 8;

   type VMCS_Array is array (Skp.Global_Subject_ID_Type'Range)
     of VMCS_Region_Type
   with
      Independent_Components;

   VMCS : VMCS_Array
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Skp.Kernel.Subj_VMCS_Address);

   ---------------------------------------------------------------------------

   --  Return per-CPU memory offset.
   function Get_CPU_Offset return SK.Word64
   is (SK.Word64 (CPU_Info.CPU_ID) * SK.Page_Size)
   with
      Global => (Input => CPU_Info.CPU_ID);

   -------------------------------------------------------------------------

   procedure VMCS_Write
     (Field : SK.Word16;
      Value : SK.Word64)
   is
      Success : Boolean;
   begin
      CPU.VMX.VMWRITE
        (Field   => SK.Word64 (Field),
         Value   => Value,
         Success => Success);
      pragma Debug
        (not Success, KC.Put_Line
           (Item => "Error setting VMCS field " & Strings.Img (Field)
            & " to value " & Strings.Img (Value)));

      if not Success then
         Error (Reason           => Crash_Audit_Types.VTx_VMCS_Write_Failed,
                VMCS_Field       => Field,
                VMCS_Field_Value => Value);
      end if;
   end VMCS_Write;

   -------------------------------------------------------------------------

   procedure VMCS_Read
     (Field :     SK.Word16;
      Value : out SK.Word64)
   is
      Success : Boolean;
   begin
      CPU.VMX.VMREAD
        (Field   => SK.Word64 (Field),
         Value   => Value,
         Success => Success);
      pragma Debug (not Success, Dump.Print_Message
                    (Msg => "Error reading VMCS field "
                     & Strings.Img (Field)));

      if not Success then
         VMX_Error;
      end if;
   end VMCS_Read;

   -------------------------------------------------------------------------

   procedure VMX_Error
   is
   begin
      CPU.Panic;
   end VMX_Error;

   -------------------------------------------------------------------------

   procedure VMCS_Set_Interrupt_Window (Value : Boolean)
   is
      Interrupt_Window_Exit_Flag : constant Bitops.Word64_Pos := 2;

      Cur_Flags : Word64;
   begin
      VMCS_Read (Field => Constants.CPU_BASED_EXEC_CONTROL,
                 Value => Cur_Flags);
      if Value then
         Cur_Flags := Bitops.Bit_Set
           (Value => Cur_Flags,
            Pos   => Interrupt_Window_Exit_Flag);
      else
         Cur_Flags := Bitops.Bit_Clear
           (Value => Cur_Flags,
            Pos   => Interrupt_Window_Exit_Flag);
      end if;
      VMCS_Write (Field => Constants.CPU_BASED_EXEC_CONTROL,
                  Value => Cur_Flags);
   end VMCS_Set_Interrupt_Window;

   -------------------------------------------------------------------------

   procedure VMCS_Setup_Control_Fields
     (IO_Bitmap_Address  : SK.Word64;
      MSR_Bitmap_Address : SK.Word64;
      MSR_Store_Address  : SK.Word64;
      MSR_Count          : SK.Word32;
      Ctls_Exec_Pin      : SK.Word32;
      Ctls_Exec_Proc     : SK.Word32;
      Ctls_Exec_Proc2    : SK.Word32;
      Ctls_Exit          : SK.Word32;
      Ctls_Entry         : SK.Word32;
      CR0_Mask           : SK.Word64;
      CR4_Mask           : SK.Word64;
      Exception_Bitmap   : SK.Word32)
   is
      Default0, Default1, Value : SK.Word32;
   begin

      --  Pin-based controls.

      CPU.Get_MSR (Register => Constants.IA32_VMX_TRUE_PINBASED_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Ctls_Exec_Pin;
      Value := Value and Default1;
      Value := Value or  Default0;
      VMCS_Write (Field => Constants.PIN_BASED_EXEC_CONTROL,
                  Value => SK.Word64 (Value));

      --  Primary processor-based controls.

      CPU.Get_MSR (Register => Constants.IA32_VMX_TRUE_PROCBASED_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Ctls_Exec_Proc;
      Value := Value and Default1;
      Value := Value or  Default0;
      VMCS_Write (Field => Constants.CPU_BASED_EXEC_CONTROL,
                  Value => SK.Word64 (Value));

      --  Secondary processor-based controls.

      CPU.Get_MSR (Register => Constants.IA32_VMX_PROCBASED_CTLS2,
                   Low      => Default0,
                   High     => Default1);
      Value := Ctls_Exec_Proc2;
      Value := Value and Default1;
      Value := Value or  Default0;
      VMCS_Write (Field => Constants.CPU_BASED_EXEC_CONTROL2,
                  Value => SK.Word64 (Value));

      --  Exception bitmap.

      VMCS_Write (Field => Constants.EXCEPTION_BITMAP,
                  Value => SK.Word64 (Exception_Bitmap));

      --  Write access to CR0/CR4.

      VMCS_Write (Field => Constants.CR0_MASK,
                  Value => CR0_Mask);
      VMCS_Write (Field => Constants.CR4_MASK,
                  Value => CR4_Mask);

      --  I/O bitmaps.

      VMCS_Write (Field => Constants.IO_BITMAP_A,
                  Value => IO_Bitmap_Address);
      VMCS_Write (Field => Constants.IO_BITMAP_B,
                  Value => IO_Bitmap_Address + SK.Page_Size);

      --  MSR bitmaps.

      VMCS_Write (Field => Constants.MSR_BITMAP,
                  Value => MSR_Bitmap_Address);

      --  MSR store.

      VMCS_Write (Field => Constants.VM_EXIT_MSR_STORE_ADDRESS,
                  Value => MSR_Store_Address);
      VMCS_Write (Field => Constants.VM_ENTRY_MSR_LOAD_ADDRESS,
                  Value => MSR_Store_Address);
      VMCS_Write (Field => Constants.VM_EXIT_MSR_STORE_COUNT,
                  Value => SK.Word64 (MSR_Count));
      VMCS_Write (Field => Constants.VM_ENTRY_MSR_LOAD_COUNT,
                  Value => SK.Word64 (MSR_Count));

      --  VM-exit controls.

      CPU.Get_MSR (Register => Constants.IA32_VMX_TRUE_EXIT_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Ctls_Exit;
      Value := Value and Default1;
      Value := Value or  Default0;
      VMCS_Write (Field => Constants.VM_EXIT_CONTROLS,
                  Value => SK.Word64 (Value));

      --  VM-entry controls.

      CPU.Get_MSR (Register => Constants.IA32_VMX_TRUE_ENTRY_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Ctls_Entry;
      Value := Value and Default1;
      Value := Value or  Default0;
      VMCS_Write (Field => Constants.VM_ENTRY_CONTROLS,
                  Value => SK.Word64 (Value));
   end VMCS_Setup_Control_Fields;

   -------------------------------------------------------------------------

   procedure VMCS_Setup_Host_Fields
   is
      GDT_Base, IDT_Base, TSS_Base : Word64;

      CR0 : constant Word64 := CPU.Get_CR0;
      CR3 : constant Word64 := CPU.Get_CR3;
      CR4 : constant Word64 := CPU.Get_CR4;

      IA32_EFER : constant Word64 := CPU.Get_MSR64
        (Register => Constants.IA32_EFER);
   begin
      VMCS_Write (Field => Constants.HOST_SEL_CS,
                  Value => SEL_KERN_CODE);
      VMCS_Write (Field => Constants.HOST_SEL_DS,
                  Value => SEL_KERN_DATA);
      VMCS_Write (Field => Constants.HOST_SEL_ES,
                  Value => SEL_KERN_DATA);
      VMCS_Write (Field => Constants.HOST_SEL_SS,
                  Value => SEL_KERN_DATA);
      VMCS_Write (Field => Constants.HOST_SEL_FS,
                  Value => SEL_KERN_DATA);
      VMCS_Write (Field => Constants.HOST_SEL_GS,
                  Value => SEL_KERN_DATA);
      VMCS_Write (Field => Constants.HOST_SEL_TR,
                  Value => SEL_TSS);

      VMCS_Write (Field => Constants.HOST_CR0,
                  Value => CR0);
      VMCS_Write (Field => Constants.HOST_CR3,
                  Value => CR3);
      VMCS_Write (Field => Constants.HOST_CR4,
                  Value => CR4);

      Interrupt_Tables.Get_Base_Addresses
        (GDT => GDT_Base,
         IDT => IDT_Base,
         TSS => TSS_Base);
      VMCS_Write (Field => Constants.HOST_BASE_GDTR,
                  Value => GDT_Base);
      VMCS_Write (Field => Constants.HOST_BASE_IDTR,
                  Value => IDT_Base);
      VMCS_Write (Field => Constants.HOST_BASE_TR,
                  Value => TSS_Base);

      VMCS_Write (Field => Constants.HOST_RSP,
                  Value => Skp.Kernel.Stack_Address);
      VMCS_Write (Field => Constants.HOST_RIP,
                  Value => Exit_Address);
      VMCS_Write (Field => Constants.HOST_IA32_EFER,
                  Value => IA32_EFER);
   end VMCS_Setup_Host_Fields;

   -------------------------------------------------------------------------

   procedure VMCS_Setup_Guest_Fields
     (PML4_Address : SK.Word64;
      EPT_Pointer  : SK.Word64;
      RIP_Value    : SK.Word64;
      RSP_Value    : SK.Word64;
      CR0_Value    : SK.Word64;
      CR4_Value    : SK.Word64;
      CS_Access    : SK.Word32)
   is
   begin
      VMCS_Write (Field => Constants.VMCS_LINK_POINTER,
                  Value => SK.Word64'Last);

      VMCS_Write (Field => Constants.GUEST_SEL_CS,
                  Value => SEL_KERN_CODE);
      VMCS_Write (Field => Constants.GUEST_SEL_DS,
                  Value => SEL_KERN_DATA);
      VMCS_Write (Field => Constants.GUEST_SEL_ES,
                  Value => SEL_KERN_DATA);
      VMCS_Write (Field => Constants.GUEST_SEL_FS,
                  Value => SEL_KERN_DATA);
      VMCS_Write (Field => Constants.GUEST_SEL_GS,
                  Value => SEL_KERN_DATA);
      VMCS_Write (Field => Constants.GUEST_SEL_SS,
                  Value => SEL_KERN_DATA);
      VMCS_Write (Field => Constants.GUEST_SEL_TR,
                  Value => SEL_TSS);
      VMCS_Write (Field => Constants.GUEST_SEL_LDTR,
                  Value => SEL_TSS);

      VMCS_Write (Field => Constants.GUEST_LIMIT_CS,
                  Value => SK.Word64 (SK.Word32'Last));
      VMCS_Write (Field => Constants.GUEST_LIMIT_DS,
                  Value => SK.Word64 (SK.Word32'Last));
      VMCS_Write (Field => Constants.GUEST_LIMIT_ES,
                  Value => SK.Word64 (SK.Word32'Last));
      VMCS_Write (Field => Constants.GUEST_LIMIT_SS,
                  Value => SK.Word64 (SK.Word32'Last));
      VMCS_Write (Field => Constants.GUEST_LIMIT_TR,
                  Value => SK.Word64 (SK.Byte'Last));
      VMCS_Write (Field => Constants.GUEST_LIMIT_LDTR,
                  Value => SK.Word64 (SK.Byte'Last));

      VMCS_Write (Field => Constants.GUEST_ACCESS_RIGHTS_CS,
                  Value => SK.Word64 (CS_Access));
      VMCS_Write (Field => Constants.GUEST_ACCESS_RIGHTS_DS,
                  Value => 16#c093#);
      VMCS_Write (Field => Constants.GUEST_ACCESS_RIGHTS_ES,
                  Value => 16#c093#);
      VMCS_Write (Field => Constants.GUEST_ACCESS_RIGHTS_SS,
                  Value => 16#c093#);
      VMCS_Write (Field => Constants.GUEST_ACCESS_RIGHTS_TR,
                  Value => 16#8b#);

      --  Disable fs, gs and ldt segments; they can be enabled by guest code
      --  if needed.

      VMCS_Write (Field => Constants.GUEST_ACCESS_RIGHTS_FS,
                  Value => 16#10000#);
      VMCS_Write (Field => Constants.GUEST_ACCESS_RIGHTS_GS,
                  Value => 16#10000#);
      VMCS_Write (Field => Constants.GUEST_ACCESS_RIGHTS_LDTR,
                  Value => 16#10000#);

      VMCS_Write (Field => Constants.GUEST_CR0,
                  Value => CR0_Value);
      VMCS_Write (Field => Constants.GUEST_CR3,
                  Value => PML4_Address);
      VMCS_Write (Field => Constants.GUEST_CR4,
                  Value => CR4_Value);

      VMCS_Write (Field => Constants.EPT_POINTER,
                  Value => EPT_Pointer);

      VMCS_Write (Field => Constants.GUEST_RIP,
                  Value => RIP_Value);
      VMCS_Write (Field => Constants.GUEST_RSP,
                  Value => RSP_Value);
      VMCS_Write (Field => Constants.GUEST_RFLAGS,
                  Value => 2);
   end VMCS_Setup_Guest_Fields;

   -------------------------------------------------------------------------

   --  Clear VMCS with given address.
   procedure Clear (VMCS_Address : SK.Word64)
   with
      Global => (In_Out => X86_64.State)
   is
      Success : Boolean;
   begin
      CPU.VMX.VMCLEAR
        (Region  => VMCS_Address,
         Success => Success);
      pragma Debug (not Success, Dump.Print_Message
                    (Msg => "Error clearing VMCS: "
                     & Strings.Img (VMCS_Address)));

      if not Success then
         VMX_Error;
      end if;
   end Clear;

   -------------------------------------------------------------------------

   procedure Reset
     (VMCS_Address : SK.Word64;
      Subject_ID   : Skp.Global_Subject_ID_Type)
   is
      Rev_ID, Unused_High : SK.Word32;
   begin

      --  Invalidate current VMCS and force CPU to sync data to VMCS memory.

      Clear (VMCS_Address => VMCS_Address);

      --  MSR IA32_VMX_BASIC definition, see Intel SDM Vol. 3C, appendix A.1.

      pragma Warnings
        (GNATprove, Off, "unused assignment to ""Unused_High""",
         Reason => "Lower 32-bits contain revision ID, rest not needed");
      CPU.Get_MSR (Register => Constants.IA32_VMX_BASIC,
                   Low      => Rev_ID,
                   High     => Unused_High);
      pragma Warnings (GNATprove, On, "unused assignment to ""Unused_High""");

      VMCS (Subject_ID).Header := (Revision_ID     => Rev_ID,
                                   Abort_Indicator => 0);
      VMCS (Subject_ID).Data   := (others => 0);

      --  Clear VMCS and set some initial values.

      Clear (VMCS_Address => VMCS_Address);
   end Reset;

   -------------------------------------------------------------------------

   procedure Load (VMCS_Address : SK.Word64)
   is
      Success : Boolean;
   begin
      CPU.VMX.VMPTRLD
        (Region  => VMCS_Address,
         Success => Success);
      pragma Debug (not Success, Dump.Print_Message
                    (Msg => "Error loading VMCS pointer: "
                     & Strings.Img (VMCS_Address)));

      if not Success then
         VMX_Error;
      end if;
   end Load;

   -------------------------------------------------------------------------

   procedure Enter_Root_Mode
   is
      Success : Boolean;
      CR4     : constant Word64 := CPU.Get_CR4;
   begin
      CPU.Set_CR4 (Value => Bitops.Bit_Set
                   (Value => CR4,
                    Pos   => Constants.CR4_VMXE_FLAG));

      CPU.VMX.VMXON
        (Region  => Skp.Vmxon_Address + Get_CPU_Offset,
         Success => Success);
      pragma Debug (not Success, KC.Put_Line (Item => "Error enabling VMX"));

      if not Success then
         Error (Reason => Crash_Audit_Types.VTx_VMX_Root_Mode_Failed);
      end if;
   end Enter_Root_Mode;

end SK.VMX;
