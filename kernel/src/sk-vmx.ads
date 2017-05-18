--
--  Copyright (C) 2013, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp;

with X86_64;

with SK.CPU_Global;

package SK.VMX
with
   Abstract_State =>
     (VMCS_State with External => (Async_Readers, Async_Writers))
is

   --  Physical memory address of VMX exit handler.
   Exit_Address : constant SK.Word64
   with
      Import,
      Convention => C,
      Link_Name  => "vmx_exit_handler_ptr";

   --  Enter VMX root operation.
   procedure Enter_Root_Mode
   with
      Global => (Input  => CPU_Global.CPU_ID,
                 In_Out => X86_64.State);

   --  Reset VMCS of subject specified by ID.
   procedure Reset
     (VMCS_Address : SK.Word64;
      Subject_ID   : Skp.Subject_Id_Type)
   with
      Global  => (In_Out => (X86_64.State, VMCS_State)),
      Depends => (VMCS_State   =>+ (X86_64.State, Subject_ID, VMCS_Address),
                  X86_64.State =>+ VMCS_Address);

   --  Load VMCS with given address.
   procedure Load (VMCS_Address : SK.Word64)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ VMCS_Address);

   --  Read value from specified field of the current, active VMCS. If the
   --  operation fails, CPU.Panic is called.
   procedure VMCS_Read
     (Field :     SK.Word16;
      Value : out SK.Word64)
   with
      Global  => (In_Out => X86_64.State),
      Depends => ((Value, X86_64.State) => (Field, X86_64.State));

   --  Write given value to the specified field of the current, active VMCS. If
   --  the operation fails, CPU.Panic is called.
   procedure VMCS_Write
     (Field : SK.Word16;
      Value : SK.Word64)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ (Field, Value));

   --  Setup control fields of the currently active VMCS.
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
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+
                  (CR0_Mask, CR4_Mask, Ctls_Entry, Ctls_Exec_Pin,
                   Ctls_Exec_Proc, Ctls_Exec_Proc2, Ctls_Exit,
                   Exception_Bitmap, IO_Bitmap_Address, MSR_Bitmap_Address,
                   MSR_Count, MSR_Store_Address));

   --  Setup host fields of the currently active VMCS.
   procedure VMCS_Setup_Host_Fields
   with
      Global  => (Input  => (Exit_Address, CPU_Global.State),
                  In_Out => X86_64.State),
      Depends => (X86_64.State =>+ (Exit_Address, CPU_Global.State));

   --  Setup guest fields of the currently active VMCS.
   procedure VMCS_Setup_Guest_Fields
     (PML4_Address : SK.Word64;
      EPT_Pointer  : SK.Word64;
      RIP_Value    : SK.Word64;
      RSP_Value    : SK.Word64;
      CR0_Value    : SK.Word64;
      CR4_Value    : SK.Word64;
      CS_Access    : SK.Word32)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+
                  (CR0_Value, CR4_Value, CS_Access, EPT_Pointer,
                   PML4_Address, RIP_Value, RSP_Value));

   --  Enable/Disable interrupt-window exiting depending on the given value.
   procedure VMCS_Set_Interrupt_Window (Value : Boolean)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ (Value, X86_64.State));

   --  Report VMX launch/resume error and panic.
   procedure VMX_Error
   with
      Global     => (In_Out => X86_64.State),
      Depends    => (X86_64.State =>+ null),
      No_Return,
      Export,
      Convention => C,
      Link_Name  => "vmx_error";

end SK.VMX;
