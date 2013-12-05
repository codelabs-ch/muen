--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.VMX;
with SK.CPU;
with SK.Constants;

package body SK.Subjects
--# own
--#    State is Descriptors;
is

   pragma Warnings (Off, "*padded by * bits");
   type Subject_State_Array is array
     (Skp.Subject_Id_Type) of SK.Subject_State_Type;
   for Subject_State_Array'Component_Size use Page_Size * 8;
   for Subject_State_Array'Alignment use Page_Size;
   pragma Warnings (On, "*padded by * bits");

   --  Descriptors used to manage subject states.
   --# accept Warning, 396, Descriptors, "Not an external variable";
   Descriptors : Subject_State_Array;
   for Descriptors'Address use System'To_Address (16#001e0000#);
   --# end accept;

   -------------------------------------------------------------------------

   function Get_Instruction_Length (Id : Skp.Subject_Id_Type) return SK.Word64
   --# global
   --#    Descriptors;
   --# return
   --#    Descriptors (Id).Instruction_Len;
   is
   begin
      return Descriptors (Id).Instruction_Len;
   end Get_Instruction_Length;

   -------------------------------------------------------------------------

   function Get_Interrupt_Info (Id : Skp.Subject_Id_Type) return SK.Word64
   --# global
   --#    Descriptors;
   --# return
   --#    Descriptors (Id).Interrupt_Info;
   is
   begin
      return Descriptors (Id).Interrupt_Info;
   end Get_Interrupt_Info;

   -------------------------------------------------------------------------

   function Get_RFLAGS (Id : Skp.Subject_Id_Type) return SK.Word64
   --# global
   --#    Descriptors;
   --# return
   --#    Descriptors (Id).RFLAGS;
   is
   begin
      return Descriptors (Id).RFLAGS;
   end Get_RFLAGS;

   -------------------------------------------------------------------------

   function Get_RIP (Id : Skp.Subject_Id_Type) return SK.Word64
   --# global
   --#    Descriptors;
   --# return
   --#    Descriptors (Id).RIP;
   is
   begin
      return Descriptors (Id).RIP;
   end Get_RIP;

   -------------------------------------------------------------------------

   function Get_State (Id : Skp.Subject_Id_Type) return SK.Subject_State_Type
   --# global
   --#    Descriptors;
   --# return
   --#    Descriptors (Id);
   is
   begin
      return Descriptors (Id);
   end Get_State;

   -------------------------------------------------------------------------

   procedure Restore_State
     (Id   :     Skp.Subject_Id_Type;
      GPRs : out SK.CPU_Registers_Type)
   --# global
   --#    in     Descriptors;
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from
   --#       *,
   --#       Id,
   --#       Descriptors,
   --#       X86_64.State &
   --#    GPRs from Descriptors, Id;
   --# post
   --#    GPRs = Descriptors (Id).Regs;
   is
   begin
      VMX.VMCS_Write (Field => Constants.GUEST_RIP,
                      Value => Descriptors (Id).RIP);
      VMX.VMCS_Write (Field => Constants.GUEST_RSP,
                      Value => Descriptors (Id).RSP);
      VMX.VMCS_Write (Field => Constants.GUEST_CR0,
                      Value => Descriptors (Id).CR0);
      VMX.VMCS_Write (Field => Constants.CR0_READ_SHADOW,
                      Value => Descriptors (Id).SHADOW_CR0);
      CPU.XRSTOR (Source => Descriptors (Id).XSAVE_Area);

      if CPU.Get_CR2 /= Descriptors (Id).CR2 then
         CPU.Set_CR2 (Value => Descriptors (Id).CR2);
      end if;

      GPRs := Descriptors (Id).Regs;
   end Restore_State;

   -------------------------------------------------------------------------

   procedure Save_State
     (Id   : Skp.Subject_Id_Type;
      GPRs : SK.CPU_Registers_Type)
   --# global
   --#    in out X86_64.State;
   --#    in out Descriptors;
   --# derives
   --#    Descriptors from
   --#       *,
   --#       Id,
   --#       GPRs,
   --#       X86_64.State &
   --#    X86_64.State from *;
   is
   begin
      Descriptors (Id).Regs := GPRs;

      VMX.VMCS_Read (Field => Constants.VMX_EXIT_REASON,
                     Value => Descriptors (Id).Exit_Reason);
      VMX.VMCS_Read (Field => Constants.VMX_EXIT_QUALIFICATION,
                     Value => Descriptors (Id).Exit_Qualification);
      VMX.VMCS_Read (Field => Constants.VMX_EXIT_INTR_INFO,
                     Value => Descriptors (Id).Interrupt_Info);
      VMX.VMCS_Read (Field => Constants.VMX_EXIT_INSTRUCTION_LEN,
                     Value => Descriptors (Id).Instruction_Len);

      VMX.VMCS_Read (Field => Constants.GUEST_PHYSICAL_ADDRESS,
                     Value => Descriptors (Id).Guest_Phys_Addr);

      VMX.VMCS_Read (Field => Constants.GUEST_RIP,
                     Value => Descriptors (Id).RIP);
      VMX.VMCS_Read (Field => Constants.GUEST_SEL_CS,
                     Value => Descriptors (Id).CS);
      VMX.VMCS_Read (Field => Constants.GUEST_RSP,
                     Value => Descriptors (Id).RSP);
      VMX.VMCS_Read (Field => Constants.GUEST_SEL_SS,
                     Value => Descriptors (Id).SS);
      VMX.VMCS_Read (Field => Constants.GUEST_CR0,
                     Value => Descriptors (Id).CR0);
      VMX.VMCS_Read (Field => Constants.CR0_READ_SHADOW,
                     Value => Descriptors (Id).SHADOW_CR0);
      Descriptors (Id).CR2 := CPU.Get_CR2;
      VMX.VMCS_Read (Field => Constants.GUEST_CR3,
                     Value => Descriptors (Id).CR3);
      VMX.VMCS_Read (Field => Constants.GUEST_CR4,
                     Value => Descriptors (Id).CR4);
      VMX.VMCS_Read (Field => Constants.GUEST_RFLAGS,
                     Value => Descriptors (Id).RFLAGS);
      VMX.VMCS_Read (Field => Constants.GUEST_IA32_EFER,
                     Value => Descriptors (Id).IA32_EFER);
      CPU.XSAVE (Target => Descriptors (Id).XSAVE_Area);
   end Save_State;

   -------------------------------------------------------------------------

   procedure Set_CR0
     (Id    : Skp.Subject_Id_Type;
      Value : SK.Word64)
   --# global
   --#    Descriptors;
   --# derives
   --#    Descriptors from *, Id, Value;
   --# post
   --#    Descriptors (Id).CR0 = Value;
   is
   begin
      Descriptors (Id).CR0 := Value;
   end Set_CR0;

   -------------------------------------------------------------------------

   procedure Set_RIP
     (Id    : Skp.Subject_Id_Type;
      Value : SK.Word64)
   --# global
   --#    Descriptors;
   --# derives
   --#    Descriptors from *, Id, Value;
   --# post
   --#    Descriptors (Id).RIP = Value;
   is
   begin
      Descriptors (Id).RIP := Value;
   end Set_RIP;

   -------------------------------------------------------------------------

   procedure Set_RSP
     (Id    : Skp.Subject_Id_Type;
      Value : SK.Word64)
   --# global
   --#    Descriptors;
   --# derives
   --#    Descriptors from *, Id, Value;
   --# post
   --#    Descriptors (Id).RSP = Value;
   is
   begin
      Descriptors (Id).RSP := Value;
   end Set_RSP;

begin

   --# hide SK.Subjects;

   for D in Skp.Subject_Id_Type range Subject_State_Array'Range loop
      Descriptors (D) := SK.Null_Subject_State;
   end loop;
end SK.Subjects;
