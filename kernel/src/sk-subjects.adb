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

   procedure Filter_State (ID : Skp.Global_Subject_ID_Type)
   is
   begin
      Descriptors (ID).Data.CR4 := Bitops.Bit_Set
        (Value => Descriptors (ID).Data.CR4,
         Pos   => Constants.CR4_MCE_FLAG);
   end Filter_State;

   -------------------------------------------------------------------------

   procedure Increment_RIP (ID : Skp.Global_Subject_ID_Type)
   with
      Refined_Global  => (In_Out => Descriptors),
      Refined_Depends => (Descriptors  =>+ ID),
      Refined_Post    => Descriptors (ID).Data.RIP =
        Descriptors (ID).Data.RIP'Old + Word64
       (Descriptors (ID).Data.Instruction_Len)
   is
      Next_RIP : constant Word64 := Descriptors (ID).Data.RIP +
                   Word64 (Descriptors (ID).Data.Instruction_Len);
   begin
      Descriptors (ID).Data.RIP := Next_RIP;
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

   procedure Reset_State
     (ID       : Skp.Global_Subject_ID_Type;
      GPRs     : CPU_Registers_Type;
      RIP      : Word64;
      RSP      : Word64;
      CR0      : Word64;
      CR4      : Word64;
      Segments : Segment_Registers_Type)
   with
      Refined_Global  => (In_Out => Descriptors),
      Refined_Depends => (Descriptors =>+ (ID, GPRs, RIP, RSP, CR0, CR4,
                                           Segments))
   is
   begin
      Descriptors (ID).Data :=
        (Regs            => GPRs,
         Exit_Reason     => 0,
         Intr_State      => 0,
         SYSENTER_CS     => 0,
         Instruction_Len => 0,
         RIP             => RIP,
         RSP             => RSP,
         CR0             => CR0,
         CR4             => CR4,
         RFLAGS          => Constants.RFLAGS_Default_Value,
         Segment_Regs    => Segments,
         GDTR            => Null_Segment,
         IDTR            => Null_Segment,
         others          => 0);
   end Reset_State;

   -------------------------------------------------------------------------

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
      VMX.VMCS_Write (Field => Constants.GUEST_INTERRUPTIBILITY,
                      Value => Word64 (Descriptors (ID).Data.Intr_State));
      VMX.VMCS_Write (Field => Constants.GUEST_RIP,
                      Value => Descriptors (ID).Data.RIP);
      VMX.VMCS_Write (Field => Constants.GUEST_RSP,
                      Value => Descriptors (ID).Data.RSP);

      VMX.VMCS_Write (Field => Constants.GUEST_CR0,
                      Value => Descriptors (ID).Data.CR0);
      VMX.VMCS_Write (Field => Constants.CR0_READ_SHADOW,
                      Value => Descriptors (ID).Data.SHADOW_CR0);
      VMX.VMCS_Write (Field => Constants.GUEST_CR4,
                      Value => Descriptors (ID).Data.CR4);
      VMX.VMCS_Write (Field => Constants.CR4_READ_SHADOW,
                      Value => Descriptors (ID).Data.SHADOW_CR4);

      VMX.VMCS_Write (Field => Constants.GUEST_RFLAGS,
                      Value => Descriptors (ID).Data.RFLAGS);
      VMX.VMCS_Write (Field => Constants.GUEST_IA32_EFER,
                      Value => Descriptors (ID).Data.IA32_EFER);

      VMX.VMCS_Write (Field => Constants.GUEST_BASE_GDTR,
                      Value => Descriptors (ID).Data.GDTR.Base);
      VMX.VMCS_Write (Field => Constants.GUEST_LIMIT_GDTR,
                      Value => Word64 (Descriptors (ID).Data.GDTR.Limit));
      VMX.VMCS_Write (Field => Constants.GUEST_BASE_IDTR,
                      Value => Descriptors (ID).Data.IDTR.Base);
      VMX.VMCS_Write (Field => Constants.GUEST_LIMIT_IDTR,
                      Value => Word64 (Descriptors (ID).Data.IDTR.Limit));

      VMX.VMCS_Write (Field => Constants.GUEST_SYSENTER_CS,
                      Value => Word64 (Descriptors (ID).Data.SYSENTER_CS));
      VMX.VMCS_Write (Field => Constants.GUEST_SYSENTER_EIP,
                      Value => Descriptors (ID).Data.SYSENTER_EIP);
      VMX.VMCS_Write (Field => Constants.GUEST_SYSENTER_ESP,
                      Value => Descriptors (ID).Data.SYSENTER_ESP);

      Restore_Segment (Segment_ID => CS,
                       Segment    => Descriptors (ID).Data.Segment_Regs.CS);
      Restore_Segment (Segment_ID => SS,
                       Segment    => Descriptors (ID).Data.Segment_Regs.SS);
      Restore_Segment (Segment_ID => DS,
                       Segment    => Descriptors (ID).Data.Segment_Regs.DS);
      Restore_Segment (Segment_ID => ES,
                       Segment    => Descriptors (ID).Data.Segment_Regs.ES);
      Restore_Segment (Segment_ID => FS,
                       Segment    => Descriptors (ID).Data.Segment_Regs.FS);
      Restore_Segment (Segment_ID => GS,
                       Segment    => Descriptors (ID).Data.Segment_Regs.GS);
      Restore_Segment (Segment_ID => TR,
                       Segment    => Descriptors (ID).Data.Segment_Regs.TR);
      Restore_Segment (Segment_ID => LDTR,
                       Segment    => Descriptors (ID).Data.Segment_Regs.LDTR);

      Regs := Descriptors (ID).Data.Regs;
   end Restore_State;

   -------------------------------------------------------------------------

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
      Descriptors (ID).Data.Exit_Reason := Word32'Mod (Exit_Reason);
      VMX.VMCS_Read (Field => Constants.VMX_EXIT_QUALIFICATION,
                     Value => Descriptors (ID).Data.Exit_Qualification);
      VMX.VMCS_Read (Field => Constants.GUEST_INTERRUPTIBILITY,
                     Value => Value);
      Descriptors (ID).Data.Intr_State := Word32'Mod (Value);
      VMX.VMCS_Read (Field => Constants.VMX_EXIT_INSTRUCTION_LEN,
                     Value => Value);
      Descriptors (ID).Data.Instruction_Len := Word32'Mod (Value);

      VMX.VMCS_Read (Field => Constants.GUEST_PHYSICAL_ADDRESS,
                     Value => Descriptors (ID).Data.Guest_Phys_Addr);

      VMX.VMCS_Read (Field => Constants.GUEST_RIP,
                     Value => Descriptors (ID).Data.RIP);
      VMX.VMCS_Read (Field => Constants.GUEST_RSP,
                     Value => Descriptors (ID).Data.RSP);
      VMX.VMCS_Read (Field => Constants.GUEST_CR0,
                     Value => Descriptors (ID).Data.CR0);
      VMX.VMCS_Read (Field => Constants.CR0_READ_SHADOW,
                     Value => Descriptors (ID).Data.SHADOW_CR0);
      VMX.VMCS_Read (Field => Constants.GUEST_CR3,
                     Value => Descriptors (ID).Data.CR3);
      VMX.VMCS_Read (Field => Constants.GUEST_CR4,
                     Value => Descriptors (ID).Data.CR4);
      VMX.VMCS_Read (Field => Constants.CR4_READ_SHADOW,
                     Value => Descriptors (ID).Data.SHADOW_CR4);
      VMX.VMCS_Read (Field => Constants.GUEST_RFLAGS,
                     Value => Descriptors (ID).Data.RFLAGS);
      VMX.VMCS_Read (Field => Constants.GUEST_IA32_EFER,
                     Value => Descriptors (ID).Data.IA32_EFER);

      VMX.VMCS_Read (Field => Constants.GUEST_BASE_GDTR,
                     Value => Descriptors (ID).Data.GDTR.Base);
      VMX.VMCS_Read (Field => Constants.GUEST_LIMIT_GDTR,
                     Value => Value);
      Descriptors (ID).Data.GDTR.Limit := Word32'Mod (Value);
      VMX.VMCS_Read (Field => Constants.GUEST_BASE_IDTR,
                     Value => Descriptors (ID).Data.IDTR.Base);
      VMX.VMCS_Read (Field => Constants.GUEST_LIMIT_IDTR,
                     Value => Value);
      Descriptors (ID).Data.IDTR.Limit := Word32'Mod (Value);

      VMX.VMCS_Read (Field => Constants.GUEST_SYSENTER_CS,
                     Value => Value);
      Descriptors (ID).Data.SYSENTER_CS := Word32'Mod (Value);
      VMX.VMCS_Read (Field => Constants.GUEST_SYSENTER_EIP,
                     Value => Descriptors (ID).Data.SYSENTER_EIP);
      VMX.VMCS_Read (Field => Constants.GUEST_SYSENTER_ESP,
                     Value => Descriptors (ID).Data.SYSENTER_ESP);

      Save_Segment (Segment_ID => CS,
                    Segment    => Descriptors (ID).Data.Segment_Regs.CS);
      Save_Segment (Segment_ID => SS,
                    Segment    => Descriptors (ID).Data.Segment_Regs.SS);
      Save_Segment (Segment_ID => DS,
                    Segment    => Descriptors (ID).Data.Segment_Regs.DS);
      Save_Segment (Segment_ID => ES,
                    Segment    => Descriptors (ID).Data.Segment_Regs.ES);
      Save_Segment (Segment_ID => FS,
                    Segment    => Descriptors (ID).Data.Segment_Regs.FS);
      Save_Segment (Segment_ID => GS,
                    Segment    => Descriptors (ID).Data.Segment_Regs.GS);
      Save_Segment (Segment_ID => TR,
                    Segment    => Descriptors (ID).Data.Segment_Regs.TR);
      Save_Segment (Segment_ID => LDTR,
                    Segment    => Descriptors (ID).Data.Segment_Regs.LDTR);

      Descriptors (ID).Data.Regs := Regs;
   end Save_State;

   -------------------------------------------------------------------------

   function Valid_State (ID : Skp.Global_Subject_ID_Type) return Boolean
   is
     (Bitops.Bit_Test (Value => Descriptors (ID).Data.CR4,
                       Pos   => Constants.CR4_MCE_FLAG));

end SK.Subjects;
