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

with SK.VMX;
with SK.Bitops;
with SK.Constants;

package body SK.Subjects
with
   Refined_State => (State => Descriptors)
is

   type Segment_ID_Type is (CS, SS, DS, ES, FS, GS, TR, LDTR);

   type VMCS_Seg_Fields_Type is record
      Selector_Field      : Word16;
      Base_Field          : Word16;
      Limit_Field         : Word16;
      Access_Rights_Field : Word16;
   end record;

   --  Mapping of segment ID to corresponding VMCS fields.
   Seg_to_VMCS_Map : constant array (Segment_ID_Type) of VMCS_Seg_Fields_Type
     := (CS   => (Selector_Field      => Constants.GUEST_SEL_CS,
                  Base_Field          => Constants.GUEST_BASE_CS,
                  Limit_Field         => Constants.GUEST_LIMIT_CS,
                  Access_Rights_Field => Constants.GUEST_ACCESS_RIGHTS_CS),
         SS   => (Selector_Field      => Constants.GUEST_SEL_SS,
                  Base_Field          => Constants.GUEST_BASE_SS,
                  Limit_Field         => Constants.GUEST_LIMIT_SS,
                  Access_Rights_Field => Constants.GUEST_ACCESS_RIGHTS_SS),
         DS   => (Selector_Field      => Constants.GUEST_SEL_DS,
                  Base_Field          => Constants.GUEST_BASE_DS,
                  Limit_Field         => Constants.GUEST_LIMIT_DS,
                  Access_Rights_Field => Constants.GUEST_ACCESS_RIGHTS_DS),
         ES   => (Selector_Field      => Constants.GUEST_SEL_ES,
                  Base_Field          => Constants.GUEST_BASE_ES,
                  Limit_Field         => Constants.GUEST_LIMIT_ES,
                  Access_Rights_Field => Constants.GUEST_ACCESS_RIGHTS_ES),
         FS   => (Selector_Field      => Constants.GUEST_SEL_FS,
                  Base_Field          => Constants.GUEST_BASE_FS,
                  Limit_Field         => Constants.GUEST_LIMIT_FS,
                  Access_Rights_Field => Constants.GUEST_ACCESS_RIGHTS_FS),
         GS   => (Selector_Field      => Constants.GUEST_SEL_GS,
                  Base_Field          => Constants.GUEST_BASE_GS,
                  Limit_Field         => Constants.GUEST_LIMIT_GS,
                  Access_Rights_Field => Constants.GUEST_ACCESS_RIGHTS_GS),
         TR   => (Selector_Field      => Constants.GUEST_SEL_TR,
                  Base_Field          => Constants.GUEST_BASE_TR,
                  Limit_Field         => Constants.GUEST_LIMIT_TR,
                  Access_Rights_Field => Constants.GUEST_ACCESS_RIGHTS_TR),
         LDTR => (Selector_Field      => Constants.GUEST_SEL_LDTR,
                  Base_Field          => Constants.GUEST_BASE_LDTR,
                  Limit_Field         => Constants.GUEST_LIMIT_LDTR,
                  Access_Rights_Field => Constants.GUEST_ACCESS_RIGHTS_LDTR));

   VMX_EXIT_INTR_INFO_ERROR_CODE_VALID_FLAG : constant := 11;
   VMX_EXIT_INTR_INFO_VALID_FLAG            : constant := 31;

   --  Segment selector privilege level mask, see Intel SDM Vol. 3A,
   --  "3.4.2 Segment Selectors".
   SEGMENT_SELECTOR_PL_MASK : constant := 2#11#;

   --  Guest interruptibility state, see Intel SDM Vol. 3C,
   --  "24.4.2 Guest Non-Register State".
   --   Bit 0: Blocking by STI
   --   Bit 1: Blocking by MOV SS
   INTERRUPTIBILITY_BLOCK_MASK : constant SK.Word32 := 2#11#;

   -------------------------------------------------------------------------

   --  Stores the VMCS guest selector and descriptor information of the segment
   --  specified by ID in the returned segment type.
   procedure Save_Segment
     (Segment_ID :     Segment_ID_Type;
      Segment    : out Segment_Type)
   with
      Global  => (Input  => CPU_Info.APIC_ID,
                  In_Out => (Crash_Audit.State, X86_64.State)),
      Depends => ((Segment, Crash_Audit.State,
                   X86_64.State) => (Segment_ID, CPU_Info.APIC_ID,
                                     Crash_Audit.State, X86_64.State))
   is
      Value : Word64;
   begin
      VMX.VMCS_Read (Field => Seg_to_VMCS_Map (Segment_ID).Base_Field,
                     Value => Segment.Base);
      VMX.VMCS_Read (Field => Seg_to_VMCS_Map (Segment_ID).Selector_Field,
                     Value => Segment.Selector);
      VMX.VMCS_Read (Field => Seg_to_VMCS_Map (Segment_ID).Limit_Field,
                     Value => Value);
      Segment.Limit := Word32'Mod (Value);
      VMX.VMCS_Read (Field => Seg_to_VMCS_Map (Segment_ID).Access_Rights_Field,
                     Value => Value);
      Segment.Access_Rights := Word32'Mod (Value);
   end Save_Segment;

   -------------------------------------------------------------------------

   --  Loads the VMCS guest selector and descriptor fields of the segment
   --  specified by ID with the values of the given segment type.
   procedure Restore_Segment
     (Segment_ID : Segment_ID_Type;
      Segment    : Segment_Type)
   with
      Global  => (Input  => CPU_Info.APIC_ID,
                  In_Out => (Crash_Audit.State, X86_64.State)),
      Depends => ((Crash_Audit.State,
                   X86_64.State) => (Segment_ID, Segment, CPU_Info.APIC_ID,
                                     Crash_Audit.State, X86_64.State))
   is
   begin
      VMX.VMCS_Write (Field => Seg_to_VMCS_Map (Segment_ID).Base_Field,
                      Value => Segment.Base);
      VMX.VMCS_Write (Field => Seg_to_VMCS_Map (Segment_ID).Selector_Field,
                      Value => Segment.Selector);
      VMX.VMCS_Write (Field => Seg_to_VMCS_Map (Segment_ID).Limit_Field,
                      Value => Word64 (Segment.Limit));
      VMX.VMCS_Write
        (Field => Seg_to_VMCS_Map (Segment_ID).Access_Rights_Field,
         Value => Word64 (Segment.Access_Rights));
   end Restore_Segment;

   -------------------------------------------------------------------------

   function Accepts_Interrupts (ID : Skp.Global_Subject_ID_Type) return Boolean
   with
      Refined_Global => (Input => Descriptors),
      Refined_Post   => Accepts_Interrupts'Result =
         (Descriptors (ID).Data.Intr_State = 0 and then
            Bitops.Bit_Test (Value => Descriptors (ID).Data.RFLAGS,
                             Pos   => Constants.RFLAGS_IF_FLAG))
   is
   begin
      --D @Text Section => impl_inject_interrupt
      --D A subject accepts interrupts if RFLAGS.IF is set and the VM
      --D interruptibility state does not designate a blocking condition, see
      --D Intel SDM Vol. 3C, "24.4.2 Guest Non-Register State".
      return Descriptors (ID).Data.Intr_State = 0
        and then Bitops.Bit_Test
          (Value => Descriptors (ID).Data.RFLAGS,
           Pos   => Constants.RFLAGS_IF_FLAG);
   end Accepts_Interrupts;

   -------------------------------------------------------------------------

   procedure Create_Context
     (ID  :     Skp.Global_Subject_ID_Type;
      Ctx : out Crash_Audit_Types.Subj_Context_Type)
   with
      Refined_Global => (Input  => (Descriptors, CPU_Info.APIC_ID),
                         In_Out => (Crash_Audit.State, X86_64.State))
   is
      Intr_Info : Word64;
      Err_Code  : Word64;
   begin
      Ctx := Crash_Audit_Types.Null_Subj_Context;

      VMX.VMCS_Read (Field => Constants.VMX_EXIT_INTR_INFO,
                     Value => Intr_Info);
      if Bitops.Bit_Test
        (Value => Intr_Info,
         Pos   => VMX_EXIT_INTR_INFO_VALID_FLAG)
      then
         Ctx.Intr_Info := Word32'Mod (Intr_Info);
         Ctx.Field_Validity.Intr_Info := True;

         if Bitops.Bit_Test
           (Value => Intr_Info,
            Pos   => VMX_EXIT_INTR_INFO_ERROR_CODE_VALID_FLAG)
         then
            VMX.VMCS_Read
              (Field => Constants.VMX_EXIT_INTR_ERROR_CODE,
               Value => Err_Code);
            Ctx.Intr_Error_Code := Word32'Mod (Err_Code);
            Ctx.Field_Validity.Intr_Error_Code := True;
         end if;
      end if;

      Ctx.Subject_ID := Word16 (ID);
      Ctx.Descriptor := Descriptors (ID).Data;
   end Create_Context;

   -------------------------------------------------------------------------

   --D @Section Id => impl_subjects_state_filter, Label => State Filtering, Parent => impl_subjects_state, Priority => 30
   --D @Text Section => impl_subjects_state_filter
   --D Filtering the state of a subject with given ID means that the state
   --D values fulfill the invariants specified by the \texttt{Valid\_State}
   --D function:
   procedure Filter_State (ID : Skp.Global_Subject_ID_Type)
   is
   begin
      --D @OL Id => impl_subjects_filter_state_invariants, Section => impl_subjects_state_filter, Priority => 10
      --D @Item List => impl_subjects_filter_state_invariants
      --D Force CR4.MCE bit to be set to ensure Machine Check Exceptions are
      --D active.
      Descriptors (ID).Data.CR4 := Bitops.Bit_Set
        (Value => Descriptors (ID).Data.CR4,
         Pos   => Constants.CR4_MCE_FLAG);
   end Filter_State;

   -------------------------------------------------------------------------

   function Get_Activity_State (ID : Skp.Global_Subject_ID_Type) return Boolean
   is (Descriptors (ID).Data.Activity_State
       = SK.Constants.GUEST_ACTIVITY_ACTIVE)
   with
      Refined_Global  => (Input => Descriptors),
      Refined_Depends => (Get_Activity_State'Result => (ID, Descriptors)),
      Refined_Post    =>
         Get_Activity_State'Result =
           (Descriptors (ID).Data.Activity_State = SK.Constants.GUEST_ACTIVITY_ACTIVE);

   -------------------------------------------------------------------------

   procedure Increment_RIP (ID : Skp.Global_Subject_ID_Type)
   with
      Refined_Global  => (In_Out => Descriptors),
      Refined_Depends => (Descriptors  =>+ ID),
      Refined_Post    => Descriptors (ID).Data.RIP =
        Descriptors (ID).Data.RIP'Old + Word64
       (Descriptors (ID).Data.Instruction_Len)
        and
       (Descriptors (ID).Data.Intr_State and INTERRUPTIBILITY_BLOCK_MASK) = 0
   is
      --D @Text Section => hypercall_handling, Priority => 10
      --D The RIP of the subject is incremented by the value of the current
      --D instruction length.
      Next_RIP   : constant Word64 := Descriptors (ID).Data.RIP +
        Word64 (Descriptors (ID).Data.Instruction_Len);
      Intr_State : constant SK.Word32 := Descriptors (ID).Data.Intr_State;
   begin
      Descriptors (ID).Data.RIP := Next_RIP;

      --  Clear any interrupt blocking which might have been in effect for the
      --  emulated instruction which is being skipped by incrementing the RIP.

      if (Intr_State and INTERRUPTIBILITY_BLOCK_MASK) /= 0 then
         Descriptors (ID).Data.Intr_State
           := Intr_State and not INTERRUPTIBILITY_BLOCK_MASK;
      end if;
   end Increment_RIP;

   -------------------------------------------------------------------------

   function Is_CPL_0 (ID : Skp.Global_Subject_ID_Type) return Boolean
   is ((Descriptors (ID).Data.Segment_Regs.CS.Selector
        and SEGMENT_SELECTOR_PL_MASK) = 0)
   with
      Refined_Global => (Input => Descriptors),
      Refined_Post   => Is_CPL_0'Result =
       ((Descriptors (ID).Data.Segment_Regs.CS.Selector
        and SEGMENT_SELECTOR_PL_MASK) = 0);

   -------------------------------------------------------------------------

   function Is_Running (ID : Skp.Global_Subject_ID_Type) return Boolean
   is (Descriptors (ID).Data.Running)
   with
      Refined_Global => (Input => Descriptors),
      Refined_Post   => Is_Running'Result =
       (Descriptors (ID).Data.Running);

   -------------------------------------------------------------------------

   --D @Section Id => impl_subjects_state_reset, Label => State Resetting, Parent => impl_subjects_state, Priority => 25
   --D @Text Section => impl_subjects_state_reset
   --D Resetting the state of a subject with given ID means that all state
   --D values are set to those specified in the policy. Fields that are not set
   --D by the policy are cleared to zero, except for RFLAGS which is initialized
   --D to \verb!Constants.RFLAGS_Default_Value!.
   procedure Reset_State
     (ID         : Skp.Global_Subject_ID_Type;
      GPRs       : CPU_Registers_Type;
      RIP        : Word64;
      RSP        : Word64;
      CR0        : Word64;
      CR0_Shadow : Word64;
      CR4        : Word64;
      CR4_Shadow : Word64;
      Segments   : Segment_Registers_Type)
   with
      Refined_Global  => (In_Out => Descriptors),
      Refined_Depends => (Descriptors =>+ (ID, GPRs, RIP, RSP, CR0, CR0_Shadow,
                                           CR4, CR4_Shadow, Segments))
   is
   begin
      Descriptors (ID).Data :=
        (Regs            => GPRs,
         Exit_Reason     => 0,
         Intr_State      => 0,
         Activity_State  => 0,
         SYSENTER_CS     => 0,
         Instruction_Len => 0,
         RIP             => RIP,
         RSP             => RSP,
         CR0             => CR0,
         SHADOW_CR0      => CR0_Shadow,
         CR4             => CR4,
         SHADOW_CR4      => CR4_Shadow,
         RFLAGS          => Constants.RFLAGS_Default_Value,
         Segment_Regs    => Segments,
         GDTR            => Null_Segment,
         IDTR            => Null_Segment,
         Running         => True,
         Padding         => 0,
         others          => 0);
   end Reset_State;

   -------------------------------------------------------------------------

   --D @Section Id => impl_subjects_state_restore, Label => State Restoring, Parent => impl_subjects_state, Priority => 10
   --D @Text Section => impl_subjects_state_restore
   --D Restoring the state of a subject with given ID means that the current
   --D state values are written to the corresponding VMCS fields.
   --D @OL Id => impl_subjects_state_restore_steps, Section => impl_subjects_state_restore, Priority => 10
   procedure Restore_State
     (ID   :     Skp.Global_Subject_ID_Type;
      Regs : out CPU_Registers_Type)
     with
      Refined_Global  => (Input  => (Descriptors, CPU_Info.APIC_ID),
                          In_Out => (Crash_Audit.State, X86_64.State)),
      Refined_Depends => ((Crash_Audit.State,
                           X86_64.State) => (ID, Descriptors, CPU_Info.APIC_ID,
                                             Crash_Audit.State, X86_64.State),
                          Regs           => (ID, Descriptors)),
      Refined_Post    => Descriptors (ID).Data.Regs = Regs
   is
   begin
      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest interruptibility from \texttt{Intr\_State} field.
      VMX.VMCS_Write (Field => Constants.GUEST_INTERRUPTIBILITY,
                      Value => Word64 (Descriptors (ID).Data.Intr_State));
      VMX.VMCS_Write (Field => Constants.GUEST_ACTIVITY_STATE,
                      Value => Word64 (Descriptors (ID).Data.Activity_State));

      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest RIP from \texttt{RIP} field.
      VMX.VMCS_Write (Field => Constants.GUEST_RIP,
                      Value => Descriptors (ID).Data.RIP);
      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest RSP from \texttt{RSP} field.
      VMX.VMCS_Write (Field => Constants.GUEST_RSP,
                      Value => Descriptors (ID).Data.RSP);

      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest CR0 from \texttt{CR0} field.
      VMX.VMCS_Write (Field => Constants.GUEST_CR0,
                      Value => Descriptors (ID).Data.CR0);
      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest CR0 read shadown from \texttt{SHADOW\_CR0} field.
      VMX.VMCS_Write (Field => Constants.CR0_READ_SHADOW,
                      Value => Descriptors (ID).Data.SHADOW_CR0);
      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest CR4 from \texttt{CR4} field.
      VMX.VMCS_Write (Field => Constants.GUEST_CR4,
                      Value => Descriptors (ID).Data.CR4);
      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest CR4 read shadow from \texttt{SHADOW\_CR4} field.
      VMX.VMCS_Write (Field => Constants.CR4_READ_SHADOW,
                      Value => Descriptors (ID).Data.SHADOW_CR4);

      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest RFLAGS from \texttt{RFLAGS} field.
      VMX.VMCS_Write (Field => Constants.GUEST_RFLAGS,
                      Value => Descriptors (ID).Data.RFLAGS);
      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest IA32\_EFER from \texttt{IA32\_EFER} field.
      VMX.VMCS_Write (Field => Constants.GUEST_IA32_EFER,
                      Value => Descriptors (ID).Data.IA32_EFER);

      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest GDTR base from \texttt{GDTR.Base} field.
      VMX.VMCS_Write (Field => Constants.GUEST_BASE_GDTR,
                      Value => Descriptors (ID).Data.GDTR.Base);
      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest GDTR limit from \texttt{GDTR.Limit} field.
      VMX.VMCS_Write (Field => Constants.GUEST_LIMIT_GDTR,
                      Value => Word64 (Descriptors (ID).Data.GDTR.Limit));
      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest IDTR base from \texttt{IDTR.Base} field.
      VMX.VMCS_Write (Field => Constants.GUEST_BASE_IDTR,
                      Value => Descriptors (ID).Data.IDTR.Base);
      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest IDTR limit from \texttt{IDTR.Limit} field.
      VMX.VMCS_Write (Field => Constants.GUEST_LIMIT_IDTR,
                      Value => Word64 (Descriptors (ID).Data.IDTR.Limit));

      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest SYSENTER\_CS from \texttt{SYSENTER\_CS} field.
      VMX.VMCS_Write (Field => Constants.GUEST_SYSENTER_CS,
                      Value => Word64 (Descriptors (ID).Data.SYSENTER_CS));
      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest SYSENTER\_EIP from \texttt{SYSENTER\_EIP} field.
      VMX.VMCS_Write (Field => Constants.GUEST_SYSENTER_EIP,
                      Value => Descriptors (ID).Data.SYSENTER_EIP);
      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest SYSENTER\_ESP from \texttt{SYSENTER\_ESP} field.
      VMX.VMCS_Write (Field => Constants.GUEST_SYSENTER_ESP,
                      Value => Descriptors (ID).Data.SYSENTER_ESP);

      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest CS segment from \texttt{CS} field.
      Restore_Segment (Segment_ID => CS,
                       Segment    => Descriptors (ID).Data.Segment_Regs.CS);
      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest SS segment from \texttt{SS} field.
      Restore_Segment (Segment_ID => SS,
                       Segment    => Descriptors (ID).Data.Segment_Regs.SS);
      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest DS segment from \texttt{DS} field.
      Restore_Segment (Segment_ID => DS,
                       Segment    => Descriptors (ID).Data.Segment_Regs.DS);
      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest ES segment from \texttt{ES} field.
      Restore_Segment (Segment_ID => ES,
                       Segment    => Descriptors (ID).Data.Segment_Regs.ES);
      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest FS segment from \texttt{FS} field.
      Restore_Segment (Segment_ID => FS,
                       Segment    => Descriptors (ID).Data.Segment_Regs.FS);
      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest GS segment from \texttt{GS} field.
      Restore_Segment (Segment_ID => GS,
                       Segment    => Descriptors (ID).Data.Segment_Regs.GS);
      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest TR segment from \texttt{TR} field.
      Restore_Segment (Segment_ID => TR,
                       Segment    => Descriptors (ID).Data.Segment_Regs.TR);
      --D @Item List => impl_subjects_state_restore_steps
      --D Restore guest LDTR segment from \texttt{LDTR} field.
      Restore_Segment (Segment_ID => LDTR,
                       Segment    => Descriptors (ID).Data.Segment_Regs.LDTR);

      --D @Item List => impl_subjects_state_restore_steps
      --D Restore subject registers from \texttt{Regs} field.
      Regs := Descriptors (ID).Data.Regs;
   end Restore_State;

   -------------------------------------------------------------------------

   --D @Section Id => impl_subjects_state_save, Label => State Saving, Parent => impl_subjects_state
   --D @Text Section => impl_subjects_state_save
   --D Saving the state of a subject with given ID means that the state values
   --D are updated to the current, corresponding VMCS field values.
   --D @OL Id => impl_subjects_state_save_steps, Section => impl_subjects_state_save, Priority => 10
   procedure Save_State
     (ID          : Skp.Global_Subject_ID_Type;
      Exit_Reason : Word64;
      Regs        : CPU_Registers_Type)
   with
      Refined_Global  => (Input  => CPU_Info.APIC_ID,
                          In_Out => (Descriptors, Crash_Audit.State,
                                     X86_64.State)),
      Refined_Depends =>
        (Descriptors         =>+ (ID, Exit_Reason, Regs, CPU_Info.APIC_ID,
                                  Crash_Audit.State, X86_64.State),
         (Crash_Audit.State,
          X86_64.State)      => (CPU_Info.APIC_ID, Crash_Audit.State,
                                 X86_64.State)),
      Refined_Post    => Descriptors (ID).Data.Regs = Regs
   is
      Value : Word64;
   begin
      --D @Item List => impl_subjects_state_save_steps
      --D Save VM-exit reason to \texttt{Exit\_Reason} field.
      Descriptors (ID).Data.Exit_Reason := Word32'Mod (Exit_Reason);
      --D @Item List => impl_subjects_state_save_steps
      --D Save VM-exit qualification to \texttt{Exit\_Qualification} field.
      VMX.VMCS_Read (Field => Constants.VMX_EXIT_QUALIFICATION,
                     Value => Descriptors (ID).Data.Exit_Qualification);
      --D @Item List => impl_subjects_state_save_steps
      --D Save guest interruptibility to \texttt{Intr\_State} field.
      VMX.VMCS_Read (Field => Constants.GUEST_INTERRUPTIBILITY,
                     Value => Value);
      Descriptors (ID).Data.Intr_State := Word32'Mod (Value);
      VMX.VMCS_Read (Field => Constants.GUEST_ACTIVITY_STATE,
                     Value => Value);
      Descriptors (ID).Data.Activity_State := Word32'Mod (Value);

      --D @Item List => impl_subjects_state_save_steps
      --D Save VM-Exit instruction length to \texttt{Instruction\_Len} field.
      VMX.VMCS_Read (Field => Constants.VMX_EXIT_INSTRUCTION_LEN,
                     Value => Value);
      Descriptors (ID).Data.Instruction_Len := Word32'Mod (Value);

      --D @Item List => impl_subjects_state_save_steps
      --D Save guest physical address to \texttt{Guest\_Phys\_Addr} field.
      VMX.VMCS_Read (Field => Constants.GUEST_PHYSICAL_ADDRESS,
                     Value => Descriptors (ID).Data.Guest_Phys_Addr);

      --D @Item List => impl_subjects_state_save_steps
      --D Save guest RIP to \texttt{RIP} field.
      VMX.VMCS_Read (Field => Constants.GUEST_RIP,
                     Value => Descriptors (ID).Data.RIP);
      --D @Item List => impl_subjects_state_save_steps
      --D Save guest RSP to \texttt{RSP} field.
      VMX.VMCS_Read (Field => Constants.GUEST_RSP,
                     Value => Descriptors (ID).Data.RSP);
      --D @Item List => impl_subjects_state_save_steps
      --D Save guest CR0 to \texttt{CR0} field.
      VMX.VMCS_Read (Field => Constants.GUEST_CR0,
                     Value => Descriptors (ID).Data.CR0);
      --D @Item List => impl_subjects_state_save_steps
      --D Save CR0 read shadow to \texttt{SHADOW\_CR0} field.
      VMX.VMCS_Read (Field => Constants.CR0_READ_SHADOW,
                     Value => Descriptors (ID).Data.SHADOW_CR0);
      --D @Item List => impl_subjects_state_save_steps
      --D Save guest CR3 to \texttt{CR3} field.
      VMX.VMCS_Read (Field => Constants.GUEST_CR3,
                     Value => Descriptors (ID).Data.CR3);
      --D @Item List => impl_subjects_state_save_steps
      --D Save guest CR4 to \texttt{CR4} field.
      VMX.VMCS_Read (Field => Constants.GUEST_CR4,
                     Value => Descriptors (ID).Data.CR4);
      --D @Item List => impl_subjects_state_save_steps
      --D Save CR4 read shadow to \texttt{SHADOW\_CR4} field.
      VMX.VMCS_Read (Field => Constants.CR4_READ_SHADOW,
                     Value => Descriptors (ID).Data.SHADOW_CR4);
      --D @Item List => impl_subjects_state_save_steps
      --D Save guest RFLAGS to \texttt{RFLAGS} field.
      VMX.VMCS_Read (Field => Constants.GUEST_RFLAGS,
                     Value => Descriptors (ID).Data.RFLAGS);
      --D @Item List => impl_subjects_state_save_steps
      --D Save IA32\_EFER to \texttt{IA32\_EFER} field.
      VMX.VMCS_Read (Field => Constants.GUEST_IA32_EFER,
                     Value => Descriptors (ID).Data.IA32_EFER);

      --D @Item List => impl_subjects_state_save_steps
      --D Save guest GDTR base to \texttt{GDTR.Base} field.
      VMX.VMCS_Read (Field => Constants.GUEST_BASE_GDTR,
                     Value => Descriptors (ID).Data.GDTR.Base);
      --D @Item List => impl_subjects_state_save_steps
      --D Save guest GDTR limit to \texttt{GDTR.Limit} field.
      VMX.VMCS_Read (Field => Constants.GUEST_LIMIT_GDTR,
                     Value => Value);
      Descriptors (ID).Data.GDTR.Limit := Word32'Mod (Value);
      --D @Item List => impl_subjects_state_save_steps
      --D Save guest IDTR base to \texttt{IDTR.Base} field.
      VMX.VMCS_Read (Field => Constants.GUEST_BASE_IDTR,
                     Value => Descriptors (ID).Data.IDTR.Base);
      --D @Item List => impl_subjects_state_save_steps
      --D Save guest IDTR limit to \texttt{IDTR.Limit} field.
      VMX.VMCS_Read (Field => Constants.GUEST_LIMIT_IDTR,
                     Value => Value);
      Descriptors (ID).Data.IDTR.Limit := Word32'Mod (Value);

      --D @Item List => impl_subjects_state_save_steps
      --D Save guest SYSENTER\_CS to \texttt{SYSENTER\_CS} field.
      VMX.VMCS_Read (Field => Constants.GUEST_SYSENTER_CS,
                     Value => Value);
      Descriptors (ID).Data.SYSENTER_CS := Word32'Mod (Value);
      --D @Item List => impl_subjects_state_save_steps
      --D Save guest SYSENTER\_EIP to \texttt{SYSENTER\_EIP} field.
      VMX.VMCS_Read (Field => Constants.GUEST_SYSENTER_EIP,
                     Value => Descriptors (ID).Data.SYSENTER_EIP);
      --D @Item List => impl_subjects_state_save_steps
      --D Save guest SYSENTER\_ESP to \texttt{SYSENTER\_ESP} field.
      VMX.VMCS_Read (Field => Constants.GUEST_SYSENTER_ESP,
                     Value => Descriptors (ID).Data.SYSENTER_ESP);

      --D @Item List => impl_subjects_state_save_steps
      --D Save guest CS segment to \texttt{CS} field.
      Save_Segment (Segment_ID => CS,
                    Segment    => Descriptors (ID).Data.Segment_Regs.CS);
      --D @Item List => impl_subjects_state_save_steps
      --D Save guest SS segment to \texttt{SS} field.
      Save_Segment (Segment_ID => SS,
                    Segment    => Descriptors (ID).Data.Segment_Regs.SS);
      --D @Item List => impl_subjects_state_save_steps
      --D Save guest DS segment to \texttt{DS} field.
      Save_Segment (Segment_ID => DS,
                    Segment    => Descriptors (ID).Data.Segment_Regs.DS);
      --D @Item List => impl_subjects_state_save_steps
      --D Save guest ES segment to \texttt{ES} field.
      Save_Segment (Segment_ID => ES,
                    Segment    => Descriptors (ID).Data.Segment_Regs.ES);
      --D @Item List => impl_subjects_state_save_steps
      --D Save guest FS segment to \texttt{FS} field.
      Save_Segment (Segment_ID => FS,
                    Segment    => Descriptors (ID).Data.Segment_Regs.FS);
      --D @Item List => impl_subjects_state_save_steps
      --D Save guest GS segment to \texttt{GS} field.
      Save_Segment (Segment_ID => GS,
                    Segment    => Descriptors (ID).Data.Segment_Regs.GS);
      --D @Item List => impl_subjects_state_save_steps
      --D Save guest GTR segment to \texttt{TR} field.
      Save_Segment (Segment_ID => TR,
                    Segment    => Descriptors (ID).Data.Segment_Regs.TR);
      --D @Item List => impl_subjects_state_save_steps
      --D Save guest LDTR segment to \texttt{LDTR} field.
      Save_Segment (Segment_ID => LDTR,
                    Segment    => Descriptors (ID).Data.Segment_Regs.LDTR);

      --D @Item List => impl_subjects_state_save_steps
      --D Save subject registers to \texttt{Regs} field.
      Descriptors (ID).Data.Regs := Regs;
   end Save_State;

   -------------------------------------------------------------------------

   procedure Set_Active
     (ID    : Skp.Global_Subject_ID_Type;
      Value : Boolean)
    with
      Refined_Global  => (In_Out => Descriptors),
      Refined_Depends => (Descriptors  =>+ (ID, Value)),
      Refined_Post    => Descriptors (ID).Data.Activity_State =
         (if Value then SK.Constants.GUEST_ACTIVITY_ACTIVE
          else SK.Constants.GUEST_ACTIVITY_HLT)
   is
   begin
      Descriptors (ID).Data.Activity_State
        := (if Value then SK.Constants.GUEST_ACTIVITY_ACTIVE
            else SK.Constants.GUEST_ACTIVITY_HLT);
   end Set_Active;

   -------------------------------------------------------------------------

   procedure Set_Running
     (ID    : Skp.Global_Subject_ID_Type;
      Value : Boolean)
   with
      Refined_Global  => (In_Out => Descriptors),
      Refined_Depends => (Descriptors  =>+ (ID, Value)),
      Refined_Post    => Descriptors (ID).Data.Running = Value
   is
   begin
      Descriptors (ID).Data.Running := Value;
   end Set_Running;

   -------------------------------------------------------------------------

   function Valid_State (ID : Skp.Global_Subject_ID_Type) return Boolean
   is
     (Bitops.Bit_Test (Value => Descriptors (ID).Data.CR4,
                       Pos   => Constants.CR4_MCE_FLAG));

end SK.Subjects;
