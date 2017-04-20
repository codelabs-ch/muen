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

   -------------------------------------------------------------------------

   --  Stores the VMCS guest selector and descriptor information of the segment
   --  specified by ID in the returned segment type.
   procedure Save_Segment
     (Segment_ID :     Segment_ID_Type;
      Segment    : out Segment_Type)
   with
      Global  => (In_Out => X86_64.State),
      Depends => ((Segment, X86_64.State) => (Segment_ID, X86_64.State))
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
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ (Segment_ID, Segment))
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

   function Accepts_Interrupts (ID : Skp.Subject_Id_Type) return Boolean
   with
      Refined_Global => (Input => Descriptors),
      Refined_Post   => Accepts_Interrupts'Result =
         (Descriptors (ID).Intr_State = 0 and then
            Bitops.Bit_Test (Value => Descriptors (ID).RFLAGS,
                             Pos   => Constants.RFLAGS_IF_FLAG))
   is
   begin
      return Descriptors (ID).Intr_State = 0
        and then Bitops.Bit_Test
          (Value => Descriptors (ID).RFLAGS,
           Pos   => Constants.RFLAGS_IF_FLAG);
   end Accepts_Interrupts;

   -------------------------------------------------------------------------

   procedure Clear_State (Id : Skp.Subject_Id_Type)
   with
      Refined_Global  => (In_Out => Descriptors),
      Refined_Depends => (Descriptors =>+ Id),
      Refined_Post    => Descriptors (Id) = SK.Null_Subject_State
   is
   begin
      Descriptors (Id) := SK.Null_Subject_State;
   end Clear_State;

   -------------------------------------------------------------------------

   procedure Filter_State (Id : Skp.Subject_Id_Type)
   is
   begin
      Descriptors (Id).CR4 := Bitops.Bit_Set
        (Value => Descriptors (Id).CR4,
         Pos   => Constants.CR4_MCE_FLAG);
   end Filter_State;

   -------------------------------------------------------------------------

   procedure Increment_RIP (ID : Skp.Subject_Id_Type)
   with
      Refined_Global  => (In_Out => Descriptors),
      Refined_Depends => (Descriptors  => + ID),
      Refined_Post    => Descriptors (ID).RIP =
        Descriptors (ID).RIP'Old + Descriptors (ID).Instruction_Len
   is
      Next_RIP : constant SK.Word64
        := Descriptors (ID).RIP + Descriptors (ID).Instruction_Len;
   begin
      Descriptors (ID).RIP := Next_RIP;
   end Increment_RIP;

   -------------------------------------------------------------------------

   procedure Restore_State
     (Id   :     Skp.Subject_Id_Type;
      Regs : out SK.CPU_Registers_Type)
     with
      Refined_Global  => (Input  => Descriptors,
                          In_Out => X86_64.State),
      Refined_Depends => (Regs         =>  (Descriptors, Id),
                          X86_64.State =>+ (Descriptors, Id)),
      Refined_Post    => Descriptors (Id).Regs = Regs
   is
   begin
      VMX.VMCS_Write (Field => Constants.GUEST_INTERRUPTIBILITY,
                      Value => Word64 (Descriptors (Id).Intr_State));
      VMX.VMCS_Write (Field => Constants.GUEST_RIP,
                      Value => Descriptors (Id).RIP);
      VMX.VMCS_Write (Field => Constants.GUEST_RSP,
                      Value => Descriptors (Id).RSP);

      VMX.VMCS_Write (Field => Constants.GUEST_CR0,
                      Value => Descriptors (Id).CR0);
      VMX.VMCS_Write (Field => Constants.CR0_READ_SHADOW,
                      Value => Descriptors (Id).SHADOW_CR0);
      VMX.VMCS_Write (Field => Constants.GUEST_CR4,
                      Value => Descriptors (Id).CR4);
      VMX.VMCS_Write (Field => Constants.CR4_READ_SHADOW,
                      Value => Descriptors (Id).SHADOW_CR4);

      VMX.VMCS_Write (Field => Constants.GUEST_RFLAGS,
                      Value => Descriptors (Id).RFLAGS);
      VMX.VMCS_Write (Field => Constants.GUEST_IA32_EFER,
                      Value => Descriptors (Id).IA32_EFER);

      VMX.VMCS_Write (Field => Constants.GUEST_BASE_GDTR,
                      Value => Descriptors (Id).GDTR.Base);
      VMX.VMCS_Write (Field => Constants.GUEST_LIMIT_GDTR,
                      Value => Word64 (Descriptors (Id).GDTR.Limit));
      VMX.VMCS_Write (Field => Constants.GUEST_BASE_IDTR,
                      Value => Descriptors (Id).IDTR.Base);
      VMX.VMCS_Write (Field => Constants.GUEST_LIMIT_IDTR,
                      Value => Word64 (Descriptors (Id).IDTR.Limit));

      VMX.VMCS_Write (Field => Constants.GUEST_SYSENTER_CS,
                      Value => Word64 (Descriptors (Id).SYSENTER_CS));
      VMX.VMCS_Write (Field => Constants.GUEST_SYSENTER_EIP,
                      Value => Descriptors (Id).SYSENTER_EIP);
      VMX.VMCS_Write (Field => Constants.GUEST_SYSENTER_ESP,
                      Value => Descriptors (Id).SYSENTER_ESP);

      Restore_Segment (Segment_ID => CS,
                       Segment    => Descriptors (Id).CS);
      Restore_Segment (Segment_ID => SS,
                       Segment    => Descriptors (Id).SS);
      Restore_Segment (Segment_ID => DS,
                       Segment    => Descriptors (Id).DS);
      Restore_Segment (Segment_ID => ES,
                       Segment    => Descriptors (Id).ES);
      Restore_Segment (Segment_ID => FS,
                       Segment    => Descriptors (Id).FS);
      Restore_Segment (Segment_ID => GS,
                       Segment    => Descriptors (Id).GS);
      Restore_Segment (Segment_ID => TR,
                       Segment    => Descriptors (Id).TR);
      Restore_Segment (Segment_ID => LDTR,
                       Segment    => Descriptors (Id).LDTR);

      Regs := Descriptors (Id).Regs;
   end Restore_State;

   -------------------------------------------------------------------------

   procedure Save_State
     (Id   : Skp.Subject_Id_Type;
      Regs : SK.CPU_Registers_Type)
   with
      Refined_Global  => (In_Out => (Descriptors, X86_64.State)),
      Refined_Depends => (Descriptors  =>+ (Id, Regs, X86_64.State),
                          X86_64.State =>+ null),
      Refined_Post    => Descriptors (Id).Regs = Regs
   is
      Value : Word64;
   begin
      VMX.VMCS_Read (Field => Constants.VMX_EXIT_REASON,
                     Value => Value);
      Descriptors (Id).Exit_Reason := Word32'Mod (Value);
      VMX.VMCS_Read (Field => Constants.VMX_EXIT_QUALIFICATION,
                     Value => Descriptors (Id).Exit_Qualification);
      VMX.VMCS_Read (Field => Constants.GUEST_INTERRUPTIBILITY,
                     Value => Value);
      Descriptors (Id).Intr_State := Word32'Mod (Value);
      VMX.VMCS_Read (Field => Constants.VMX_EXIT_INSTRUCTION_LEN,
                     Value => Descriptors (Id).Instruction_Len);

      VMX.VMCS_Read (Field => Constants.GUEST_PHYSICAL_ADDRESS,
                     Value => Descriptors (Id).Guest_Phys_Addr);

      VMX.VMCS_Read (Field => Constants.GUEST_RIP,
                     Value => Descriptors (Id).RIP);
      VMX.VMCS_Read (Field => Constants.GUEST_RSP,
                     Value => Descriptors (Id).RSP);
      VMX.VMCS_Read (Field => Constants.GUEST_CR0,
                     Value => Descriptors (Id).CR0);
      VMX.VMCS_Read (Field => Constants.CR0_READ_SHADOW,
                     Value => Descriptors (Id).SHADOW_CR0);
      VMX.VMCS_Read (Field => Constants.GUEST_CR3,
                     Value => Descriptors (Id).CR3);
      VMX.VMCS_Read (Field => Constants.GUEST_CR4,
                     Value => Descriptors (Id).CR4);
      VMX.VMCS_Read (Field => Constants.CR4_READ_SHADOW,
                     Value => Descriptors (Id).SHADOW_CR4);
      VMX.VMCS_Read (Field => Constants.GUEST_RFLAGS,
                     Value => Descriptors (Id).RFLAGS);
      VMX.VMCS_Read (Field => Constants.GUEST_IA32_EFER,
                     Value => Descriptors (Id).IA32_EFER);

      VMX.VMCS_Read (Field => Constants.GUEST_BASE_GDTR,
                     Value => Descriptors (Id).GDTR.Base);
      VMX.VMCS_Read (Field => Constants.GUEST_LIMIT_GDTR,
                     Value => Value);
      Descriptors (Id).GDTR.Limit := Word32'Mod (Value);
      VMX.VMCS_Read (Field => Constants.GUEST_BASE_IDTR,
                     Value => Descriptors (Id).IDTR.Base);
      VMX.VMCS_Read (Field => Constants.GUEST_LIMIT_IDTR,
                     Value => Value);
      Descriptors (Id).IDTR.Limit := Word32'Mod (Value);

      VMX.VMCS_Read (Field => Constants.GUEST_SYSENTER_CS,
                     Value => Value);
      Descriptors (Id).SYSENTER_CS := Word32'Mod (Value);
      VMX.VMCS_Read (Field => Constants.GUEST_SYSENTER_EIP,
                     Value => Descriptors (Id).SYSENTER_EIP);
      VMX.VMCS_Read (Field => Constants.GUEST_SYSENTER_ESP,
                     Value => Descriptors (Id).SYSENTER_ESP);

      Save_Segment (Segment_ID => CS,
                    Segment    => Descriptors (Id).CS);
      Save_Segment (Segment_ID => SS,
                    Segment    => Descriptors (Id).SS);
      Save_Segment (Segment_ID => DS,
                    Segment    => Descriptors (Id).DS);
      Save_Segment (Segment_ID => ES,
                    Segment    => Descriptors (Id).ES);
      Save_Segment (Segment_ID => FS,
                    Segment    => Descriptors (Id).FS);
      Save_Segment (Segment_ID => GS,
                    Segment    => Descriptors (Id).GS);
      Save_Segment (Segment_ID => TR,
                    Segment    => Descriptors (Id).TR);
      Save_Segment (Segment_ID => LDTR,
                    Segment    => Descriptors (Id).LDTR);

      Descriptors (Id).Regs := Regs;
   end Save_State;

   -------------------------------------------------------------------------

   function Valid_State (Id : Skp.Subject_Id_Type) return Boolean
   is
     (Bitops.Bit_Test (Value => Descriptors (Id).CR4,
                       Pos   => Constants.CR4_MCE_FLAG));

end SK.Subjects;
