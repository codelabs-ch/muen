--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Dump;
with SK.CPU.VMX;
with SK.Constants;

procedure SK.VMX.Error
  (Reason           : Crash_Audit_Types.VTx_Reason_Range;
   VMCS_Addr_Req    : Word64 := Crash_Audit_Types.VTx_Ctx_Noaddr;
   VMCS_Field       : Word16 := Crash_Audit_Types.VTx_Ctx_Nofield;
   VMCS_Field_Value : Word64 := 0)
is
   use type Crash_Audit_Types.VTx_Reason_Range;

   Val         : Word64;
   Success     : Boolean;
   Audit_Entry : Crash_Audit.Entry_Type := Crash_Audit.Null_Entry;
   VTx_Ctx     : Crash_Audit_Types.VTx_Context_Type
     := Crash_Audit_Types.Null_VTx_Context;
begin
   Crash_Audit.Allocate (Audit => Audit_Entry);
   Crash_Audit.Set_Reason (Audit  => Audit_Entry,
                           Reason => Reason);

   if Reason /= Crash_Audit_Types.VTx_VMX_Root_Mode_Failed then
      VTx_Ctx.VMCS_Field           := VMCS_Field;
      VTx_Ctx.VMCS_Field_Value     := VMCS_Field_Value;
      VTx_Ctx.VMCS_Address_Request := VMCS_Addr_Req;

      CPU.VMX.VMREAD
        (Field   => Constants.VMX_INST_ERROR,
         Value   => Val,
         Success => Success);
      if Success then
         VTx_Ctx.VM_Instr_Error := Byte'Mod (Val);
      end if;

      CPU.VMX.VMPTRST
        (Region  => Val,
         Success => Success);
      if Success then
         VTx_Ctx.VMCS_Address_Active := Val;
      end if;

      Crash_Audit.Set_VTx_Context
        (Audit   => Audit_Entry,
         Context => VTx_Ctx);
      pragma Debug (Dump.Print_VMX_Error (Context => VTx_Ctx));
   end if;

   Crash_Audit.Finalize (Audit => Audit_Entry);
end SK.VMX.Error;
