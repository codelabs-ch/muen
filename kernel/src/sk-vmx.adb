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

with Skp.Arch;
with Skp.Kernel;

with SK.Dump;
with SK.CPU.VMX;
with SK.Bitops;
with SK.Constants;
with SK.Crash_Audit_Types;

package body SK.VMX
with
   Refined_State => (VMCS_State => VMCS)
is

   VMCS_Header_Size : constant := 8;

   --  VMCS region format, see Intel SDM Vol. 3C, "24.2 Format of the VMCS
   --  Region".
   type VMCS_Header_Type is record
      Revision_ID     : Word32;
      Abort_Indicator : Word32;
   end record
   with
      Size => VMCS_Header_Size * 8;

   type VMCS_Data is array (VMCS_Header_Size + 1 .. Page_Size) of Byte
   with
      Size => (Page_Size - VMCS_Header_Size) * 8;

   --D @Interface
   --D Virtual-Machine Control Structure as specified in Intel SDM Vol. 3C,
   --D "Chapter 24 Virtual Machine Control Structures".
   type VMCS_Region_Type is record
      --D @Interface
      --D Header comprised of VMCS revision identifier and VMX-abort indicator.
      Header : VMCS_Header_Type;
      --D @Interface
      --D VMCS data which is declared as implementation-specific by Intel.
      Data   : VMCS_Data;
   end record
   with
      Alignment => Page_Size,
      Size      => Page_Size * 8;

   type VMCS_Array is array (Skp.Global_Subject_ID_Type) of VMCS_Region_Type
   with
      Object_Size => (Skp.Global_Subject_ID_Type'Last + 1) * Page_Size * 8,
      Independent_Components;

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   --D @Interface
   --D A Virtual Machine Control Structure (VMCS) is used by the hardware to
   --D manage the VM state of each subject designated by ID. The VM state is
   --D saved and restored on each VM-exit and entry according to the VM-controls
   --D and as specified in Intel SDM Vol. 3C, "Chapter 24 Virtual Machine
   --D Control Structures" \cite{intelsdm}.
   VMCS : VMCS_Array
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Skp.Kernel.Subj_VMCS_Address);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

   --  Allocate crash audit entry for given VMX error and trigger system
   --  restart.
   procedure VMX_Error
     (Reason  : Crash_Audit_Types.VTx_Reason_Range;
      Context : Crash_Audit_Types.VTx_Context_Type)
   with
      Global => (Input  => CPU_Info.APIC_ID,
                 In_Out => (Crash_Audit.State, X86_64.State)),
      No_Return;

   --  Report VMX launch/resume error and panic.
   procedure VMX_Error_From_Asm
   with
      Global     => (Input  => CPU_Info.APIC_ID,
                     In_Out => (Crash_Audit.State, X86_64.State)),
      Convention => C,
      Link_Name  => "vmx_error_from_asm",
      No_Return,
      Export;

   ---------------------------------------------------------------------------

   --  Return per-CPU memory offset.
   function Get_CPU_Offset return Word64
   is (Word64 (CPU_Info.CPU_ID) * Page_Size)
   with
      Global => (Input => CPU_Info.CPU_ID);

   -------------------------------------------------------------------------

   procedure VMCS_Write
     (Field : Word16;
      Value : Word64)
   is
      Success : Boolean;
   begin
      CPU.VMX.VMWRITE
        (Field   => Word64 (Field),
         Value   => Value,
         Success => Success);
      if not Success then
         declare
            Ctx : Crash_Audit_Types.VTx_Context_Type
              := Crash_Audit_Types.Null_VTx_Context;
         begin
            Ctx.VMCS_Field       := Field;
            Ctx.VMCS_Field_Value := Value;
            Ctx.Field_Validity.Field_Valid       := True;
            Ctx.Field_Validity.Field_Value_Valid := True;
            VMX_Error (Reason  => Crash_Audit_Types.VTx_VMCS_Write_Failed,
                       Context => Ctx);
         end;
      end if;
   end VMCS_Write;

   -------------------------------------------------------------------------

   procedure VMCS_Read
     (Field :     Word16;
      Value : out Word64)
   is
      Success : Boolean;
   begin
      CPU.VMX.VMREAD
        (Field   => Word64 (Field),
         Value   => Value,
         Success => Success);
      if not Success then
         declare
            Ctx : Crash_Audit_Types.VTx_Context_Type
              := Crash_Audit_Types.Null_VTx_Context;
         begin
            Ctx.VMCS_Field                 := Field;
            Ctx.Field_Validity.Field_Valid := True;
            VMX_Error (Reason  => Crash_Audit_Types.VTx_VMCS_Read_Failed,
                       Context => Ctx);
         end;
      end if;
   end VMCS_Read;

   -------------------------------------------------------------------------

   procedure VMX_Error
     (Reason  : Crash_Audit_Types.VTx_Reason_Range;
      Context : Crash_Audit_Types.VTx_Context_Type)
   is
      use type Crash_Audit_Types.VTx_Reason_Range;

      Val         : Word64;
      Audit_Entry : Crash_Audit.Entry_Type;
      VTx_Ctx     : Crash_Audit_Types.VTx_Context_Type
        := Context;
   begin
      Crash_Audit.Allocate (Audit => Audit_Entry);
      Crash_Audit.Set_Reason (Audit  => Audit_Entry,
                              Reason => Reason);

      if Reason /= Crash_Audit_Types.VTx_VMX_Root_Mode_Failed then
         CPU.VMX.VMREAD
           (Field   => Constants.VMX_INST_ERROR,
            Value   => Val,
            Success => VTx_Ctx.Field_Validity.Instrerr_Valid);
         if VTx_Ctx.Field_Validity.Instrerr_Valid then
            VTx_Ctx.VM_Instr_Error := Byte'Mod (Val);
         end if;

         CPU.VMX.VMPTRST
           (Region  => Val,
            Success => VTx_Ctx.Field_Validity.Addr_Active_Valid);
         if VTx_Ctx.Field_Validity.Addr_Active_Valid then
            VTx_Ctx.VMCS_Address_Active := Val;
         end if;

         Crash_Audit.Set_VTx_Context
           (Audit   => Audit_Entry,
            Context => VTx_Ctx);
      end if;
      pragma Debug (Dump.Print_VMX_Error
                    (Reason  => Reason,
                     Context => VTx_Ctx));

      Crash_Audit.Finalize (Audit => Audit_Entry);
   end VMX_Error;

   -------------------------------------------------------------------------

   procedure VMX_Error_From_Asm
   is
   begin
      VMX_Error (Reason  => Crash_Audit_Types.VTx_VMX_Vmentry_Failed,
                 Context => Crash_Audit_Types.Null_VTx_Context);
   end VMX_Error_From_Asm;

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
     (IO_Bitmap_Address  : Word64;
      MSR_Bitmap_Address : Word64;
      MSR_Store_Address  : Word64;
      MSR_Count          : Word32;
      Ctls_Exec_Pin      : Word32;
      Ctls_Exec_Proc     : Word32;
      Ctls_Exec_Proc2    : Word32;
      Ctls_Exit          : Word32;
      Ctls_Entry         : Word32;
      CR0_Mask           : Word64;
      CR4_Mask           : Word64;
      Exception_Bitmap   : Word32)
   is
      Default0, Default1, Value : Word32;
   begin
      --D @OL Id => impl_vmcs_setup_ctrl_steps, Section => impl_vmcs_setup_ctrl, Priority => 10
      --D @Item List => impl_vmcs_setup_ctrl_steps
      --D Read Default0 and Default1 from the
      --D \texttt{IA32\_VMX\_TRUE\_PINBASED\_CTLS} MSR. Combine them with the
      --D policy-defined value by setting the defaults for reserved control bits
      --D according to Intel SDM Vol. 3D,
      --D "A.3.1 Pin-Based VM-Execution Controls". Then, set the Pin-Based
      --D VM-Execution controls by writing the value to the corresponding VMCS
      --D field.
      CPU.Get_MSR (Register => Constants.IA32_VMX_TRUE_PINBASED_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Ctls_Exec_Pin;
      Value := Value and Default1;
      Value := Value or  Default0;
      VMCS_Write (Field => Constants.PIN_BASED_EXEC_CONTROL,
                  Value => Word64 (Value));

      --D @Item List => impl_vmcs_setup_ctrl_steps
      --D Read Default0 and Default1 from the
      --D \texttt{IA32\_VMX\_TRUE\_PROCBASED\_CTLS} MSR. Combine them with the
      --D policy-defined value by setting the defaults for reserved control bits
      --D according to Intel SDM Vol. 3D,
      --D "A.3.2 Primary Processor-Based VM-Execution Controls". Then, set the
      --D Processor-Based VM-Execution controls by writing the value to the
      --D corresponding VMCS field.
      CPU.Get_MSR (Register => Constants.IA32_VMX_TRUE_PROCBASED_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Ctls_Exec_Proc;
      Value := Value and Default1;
      Value := Value or  Default0;
      VMCS_Write (Field => Constants.CPU_BASED_EXEC_CONTROL,
                  Value => Word64 (Value));

      --D @Item List => impl_vmcs_setup_ctrl_steps
      --D Read Default0 and Default1 from the
      --D \texttt{IA32\_VMX\_PROCBASED\_CTLS2} MSR. Combine them with the
      --D policy-defined value by setting the defaults for reserved control bits
      --D according to Intel SDM Vol. 3D,
      --D "A.3.3 Secondary Processor-Based VM-Execution Controls". Then, set the
      --D secondary processor-based VM-Execution controls by writing the value
      --D to the corresponding VMCS field.
      CPU.Get_MSR (Register => Constants.IA32_VMX_PROCBASED_CTLS2,
                   Low      => Default0,
                   High     => Default1);
      Value := Ctls_Exec_Proc2;
      Value := Value and Default1;
      Value := Value or  Default0;
      VMCS_Write (Field => Constants.CPU_BASED_EXEC_CONTROL2,
                  Value => Word64 (Value));

      --D @Item List => impl_vmcs_setup_ctrl_steps
      --D Write policy-defined exception bitmap value to the corresponding
      --D VMCS field.
      VMCS_Write (Field => Constants.EXCEPTION_BITMAP,
                  Value => Word64 (Exception_Bitmap));

      --D @Item List => impl_vmcs_setup_ctrl_steps
      --D Write policy-defined CR0 and CR4 mask values to the corresponding
      --D VMCS fields.
      VMCS_Write (Field => Constants.CR0_MASK,
                  Value => CR0_Mask);
      VMCS_Write (Field => Constants.CR4_MASK,
                  Value => CR4_Mask);

      --D @Item List => impl_vmcs_setup_ctrl_steps
      --D Explicitly set CR3-target count to 0 to always force CR3-load
      --D exiting, by writing zero to the corresponding VMCS field.
      VMCS_Write (Field => Constants.CR3_TARGET_COUNT,
                  Value => 0);

      --D @Item List => impl_vmcs_setup_ctrl_steps
      --D Write policy-defined I/O bitmap address to the corresponding VMCS
      --D fields. I/O bitmap B is expected to be located in the next memory page
      --D after bitmap A (which is enforced by the validtor).
      VMCS_Write (Field => Constants.IO_BITMAP_A,
                  Value => IO_Bitmap_Address);
      VMCS_Write (Field => Constants.IO_BITMAP_B,
                  Value => IO_Bitmap_Address + Page_Size);

      --D @Item List => impl_vmcs_setup_ctrl_steps
      --D Write policy-defined MSR bitmap address to the corresponding VMCS
      --D field.
      VMCS_Write (Field => Constants.MSR_BITMAP,
                  Value => MSR_Bitmap_Address);

      --D @Item List => impl_vmcs_setup_ctrl_steps
      --D Write policy-defined MSR store address to the VM-Exit MSR store and
      --D the VM-Entry MSR load address VMCS fields. Also set the VM-Exit MSR
      --D store and VM-Entry load count fields to the policy-defined MSR count.
      VMCS_Write (Field => Constants.VM_EXIT_MSR_STORE_ADDRESS,
                  Value => MSR_Store_Address);
      VMCS_Write (Field => Constants.VM_ENTRY_MSR_LOAD_ADDRESS,
                  Value => MSR_Store_Address);
      VMCS_Write (Field => Constants.VM_EXIT_MSR_STORE_COUNT,
                  Value => Word64 (MSR_Count));
      VMCS_Write (Field => Constants.VM_ENTRY_MSR_LOAD_COUNT,
                  Value => Word64 (MSR_Count));

      --D @Item List => impl_vmcs_setup_ctrl_steps
      --D Read Default0 and Default1 from the
      --D \texttt{IA32\_VMX\_TRUE\_EXIT\_CTLS} MSR. Combine them with the
      --D policy-defined value by setting the defaults for reserved control bits
      --D according to Intel SDM Vol. 3D, "A.4 VM-Exit Controls". Then, set the
      --D VM-Exit controls by writing the value to the corresponding VMCS field.
      CPU.Get_MSR (Register => Constants.IA32_VMX_TRUE_EXIT_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Ctls_Exit;
      Value := Value and Default1;
      Value := Value or  Default0;
      VMCS_Write (Field => Constants.VM_EXIT_CONTROLS,
                  Value => Word64 (Value));

      --D @Item List => impl_vmcs_setup_ctrl_steps
      --D Read Default0 and Default1 from the
      --D \texttt{IA32\_VMX\_TRUE\_ENTRY\_CTLS} MSR. Combine them with the
      --D policy-defined value by setting the defaults for reserved control bits
      --D according to Intel SDM Vol. 3D, "A.5 VM-Entry Controls". Then, set the
      --D VM-Entry controls by writing the value to the corresponding VMCS
      --D field.
      CPU.Get_MSR (Register => Constants.IA32_VMX_TRUE_ENTRY_CTLS,
                   Low      => Default0,
                   High     => Default1);
      Value := Ctls_Entry;
      Value := Value and Default1;
      Value := Value or  Default0;
      VMCS_Write (Field => Constants.VM_ENTRY_CONTROLS,
                  Value => Word64 (Value));
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
      --D @OL Id => impl_vmcs_setup_host_steps, Section => impl_vmcs_setup_host, Priority => 10
      --D @Item List => impl_vmcs_setup_host_steps
      --D Set host CS segment selector field to \texttt{SEL\_KERN\_CODE}.
      VMCS_Write (Field => Constants.HOST_SEL_CS,
                  Value => Constants.SEL_KERN_CODE);
      --D @Item List => impl_vmcs_setup_host_steps
      --D Set host DS segment selector field to \texttt{SEL\_KERN\_DATA}.
      VMCS_Write (Field => Constants.HOST_SEL_DS,
                  Value => Constants.SEL_KERN_DATA);
      --D @Item List => impl_vmcs_setup_host_steps
      --D Set host ES segment selector field to \texttt{SEL\_KERN\_DATA}.
      VMCS_Write (Field => Constants.HOST_SEL_ES,
                  Value => Constants.SEL_KERN_DATA);
      --D @Item List => impl_vmcs_setup_host_steps
      --D Set host SS segment selector field to \texttt{SEL\_KERN\_DATA}.
      VMCS_Write (Field => Constants.HOST_SEL_SS,
                  Value => Constants.SEL_KERN_DATA);
      --D @Item List => impl_vmcs_setup_host_steps
      --D Set host FS segment selector field to \texttt{SEL\_KERN\_DATA}.
      VMCS_Write (Field => Constants.HOST_SEL_FS,
                  Value => Constants.SEL_KERN_DATA);
      --D @Item List => impl_vmcs_setup_host_steps
      --D Set host FS segment selector field to \texttt{SEL\_KERN\_DATA}.
      VMCS_Write (Field => Constants.HOST_SEL_GS,
                  Value => Constants.SEL_KERN_DATA);
      --D @Item List => impl_vmcs_setup_host_steps
      --D Set host TR segment selector field to \texttt{SEL\_TSS}.
      VMCS_Write (Field => Constants.HOST_SEL_TR,
                  Value => Constants.SEL_TSS);

      --D @Item List => impl_vmcs_setup_host_steps
      --D Set host CR0 field to current control register 0 value.
      VMCS_Write (Field => Constants.HOST_CR0,
                  Value => CR0);
      --D @Item List => impl_vmcs_setup_host_steps
      --D Set host CR3 field to current control register 3 value.
      VMCS_Write (Field => Constants.HOST_CR3,
                  Value => CR3);
      --D @Item List => impl_vmcs_setup_host_steps
      --D Set host CR4 field to current control register 4 value.
      VMCS_Write (Field => Constants.HOST_CR4,
                  Value => CR4);

      Interrupt_Tables.Get_Base_Addresses
        (GDT => GDT_Base,
         IDT => IDT_Base,
         TSS => TSS_Base);
      --D @Item List => impl_vmcs_setup_host_steps
      --D Set host GDTR base address field to current GDT base address.
      VMCS_Write (Field => Constants.HOST_BASE_GDTR,
                  Value => GDT_Base);
      --D @Item List => impl_vmcs_setup_host_steps
      --D Set host IDT base address field to current IDT base address.
      VMCS_Write (Field => Constants.HOST_BASE_IDTR,
                  Value => IDT_Base);
      --D @Item List => impl_vmcs_setup_host_steps
      --D Set host TR base address field to current TSS base address.
      VMCS_Write (Field => Constants.HOST_BASE_TR,
                  Value => TSS_Base);

      --D @Item List => impl_vmcs_setup_host_steps
      --D Set host RSP field to top of kernel stack as specified by the policy.
      VMCS_Write (Field => Constants.HOST_RSP,
                  Value => Skp.Kernel.Stack_Address);
      --D @Item List => impl_vmcs_setup_host_steps
      --D Set host RIP field to exit address which points to
      --D \texttt{vmx\_exit\_hander} declared in assembly.
      VMCS_Write (Field => Constants.HOST_RIP,
                  Value => Exit_Address);
      --D @Item List => impl_vmcs_setup_host_steps
      --D Set host IA32\_EFER field to current IA32\_EFER value.
      VMCS_Write (Field => Constants.HOST_IA32_EFER,
                  Value => IA32_EFER);
   end VMCS_Setup_Host_Fields;

   -------------------------------------------------------------------------

   procedure VMCS_Setup_Guest_Fields
     (PML4_Address : Word64;
      EPT_Pointer  : Word64)
   is
   begin
      --D @OL Id => impl_vmcs_setup_guest_steps, Section => impl_vmcs_setup_guest, Priority => 10
      --D @Item List => impl_vmcs_setup_guest_steps
      --D Set VMCS link pointer field to \texttt{16\#ffffffff\_ffffffff\#} since
      --D "VMCS shadowing" is not used, see Intel SDM Vol. 3C,
      --D "24.4.2 Guest Non-Register State".
      VMCS_Write (Field => Constants.VMCS_LINK_POINTER,
                  Value => Word64'Last);
      --D @Item List => impl_vmcs_setup_guest_steps
      --D Set guest CR3 field to policy-defined PML4 address value.
      VMCS_Write (Field => Constants.GUEST_CR3,
                  Value => PML4_Address);
      --D @Item List => impl_vmcs_setup_guest_steps
      --D Set EPT pointer field to policy-defined address value.
      VMCS_Write (Field => Constants.EPT_POINTER,
                  Value => EPT_Pointer);
   end VMCS_Setup_Guest_Fields;

   -------------------------------------------------------------------------

   --  Clear VMCS with given address.
   procedure Clear (VMCS_Address : Word64)
   with
      Global => (Input  => CPU_Info.APIC_ID,
                 In_Out => (Crash_Audit.State, X86_64.State))
   is
      Success : Boolean;
   begin
      CPU.VMX.VMCLEAR
        (Region  => VMCS_Address,
         Success => Success);
      if not Success then
         declare
            Ctx : Crash_Audit_Types.VTx_Context_Type
              := Crash_Audit_Types.Null_VTx_Context;
         begin
            Ctx.VMCS_Address_Request              := VMCS_Address;
            Ctx.Field_Validity.Addr_Request_Valid := True;
            VMX_Error (Reason  => Crash_Audit_Types.VTx_VMCS_Clear_Failed,
                       Context => Ctx);
         end;
      end if;
   end Clear;

   -------------------------------------------------------------------------

   procedure Reset
     (VMCS_Address : Word64;
      Subject_ID   : Skp.Global_Subject_ID_Type)
   is
      Rev_ID, Unused_High : Word32;
   begin
      --D @OL Id => impl_vmcs_reset_steps, Section => impl_vmcs_reset, Priority => 10
      --D @Item List => impl_vmcs_reset_steps
      --D Make the VMCS inactive, not current and clear to force the CPU to
      --D synchronize any cached data to the VMCS memory designated by the
      --D physical memory address.
      Clear (VMCS_Address => VMCS_Address);

      --D @Item List => impl_vmcs_reset_steps
      --D Read \texttt{IA32\_VMX\_BASIC} MSR to determine the VMCS revision
      --D identifier of the processor.
      CPU.Get_MSR (Register => Constants.IA32_VMX_BASIC,
                   Low      => Rev_ID,
                   High     => Unused_High);
      --D @Item List => impl_vmcs_reset_steps
      --D Set VMCS revision ID field to CPU value and the abort indicator to 0.
      --D Note, that bit 31 of the MSR is always 0 which means, the
      --D shadow-VMCS indicator will always be cleared, see Intel SDM Vol. 3D,
      --D "A.1 Basic VMX Information".
      VMCS (Subject_ID).Header := (Revision_ID     => Rev_ID,
                                   Abort_Indicator => 0);
      --D @Item List => impl_vmcs_reset_steps
      --D Set all remaining VMCS data to zero, see Intel SDM Vol. 3C,
      --D "24.2 Format of the VMCS Region".
      VMCS (Subject_ID).Data   := (others => 0);

      --D @Item List => impl_vmcs_reset_steps
      --D Execute VMCLEAR instruction again to initialize
      --D implementation-specific information in the VMCS region, see Intel SDM
      --D Vol. 3C, "24.11.3 Initializing a VMCS".
      Clear (VMCS_Address => VMCS_Address);
   end Reset;

   -------------------------------------------------------------------------

   procedure Load (VMCS_Address : Word64)
   is
      Success : Boolean;
   begin
      CPU.VMX.VMPTRLD
        (Region  => VMCS_Address,
         Success => Success);
      if not Success then
         declare
            Ctx : Crash_Audit_Types.VTx_Context_Type
              := Crash_Audit_Types.Null_VTx_Context;
         begin
            Ctx.VMCS_Address_Request              := VMCS_Address;
            Ctx.Field_Validity.Addr_Request_Valid := True;
            VMX_Error (Reason  => Crash_Audit_Types.VTx_VMCS_Load_Failed,
                       Context => Ctx);
         end;
      end if;
   end Load;

   -------------------------------------------------------------------------

   procedure Enter_Root_Mode
   is
      Success : Boolean;
      CR4     : constant Word64 := CPU.Get_CR4;
   begin
      --D @Text Section => impl_vmcs_enter_root_mode, Priority => 10
      --D First, set \texttt{CR4.VMXE} bit. Then, execute \texttt{vmxon} with
      --D the address of the VMXON region assigned to the CPU. VMXON regions are
      --D laid out in memory consecutively like an array and each CPU uses its
      --D CPU\_ID as index. VMXON regions are not mapped into the kernel address
      --D space, as they are cleared and initialized during boot in assembly
      --D (\texttt{init.S}). No further access is required.
      --D The requirements for executing the \texttt{vmxon} instruction are
      --D assured when the VMX feature is enabled see
      --D \ref{impl_vmcs_enable_vmx}. For further reference see Intel SDM Vol.
      --D 3C, "23.7 Enabling and Entering VMX Operation".
      CPU.Set_CR4 (Value => Bitops.Bit_Set
                   (Value => CR4,
                    Pos   => Constants.CR4_VMXE_FLAG));
      CPU.VMX.VMXON
        (Region  => Skp.Arch.Vmxon_Address + Get_CPU_Offset,
         Success => Success);
      if not Success then
         VMX_Error (Reason  => Crash_Audit_Types.VTx_VMX_Root_Mode_Failed,
                    Context => Crash_Audit_Types.Null_VTx_Context);
      end if;
   end Enter_Root_Mode;

end SK.VMX;
