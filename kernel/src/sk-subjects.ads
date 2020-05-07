--
--  Copyright (C) 2013, 2015, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2015, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

private with System;

private with Skp.Kernel;

with Skp;

with X86_64;

with SK.CPU_Info;
with SK.Crash_Audit;
with SK.Crash_Audit_Types;

package SK.Subjects
with
   Abstract_State => State,
   Initializes    => State
is

   --  Returns True if the subject with given ID can accept interrupts.
   function Accepts_Interrupts (ID : Skp.Global_Subject_ID_Type) return Boolean
   with
      Global => (Input => State);

   --  Returns true if the current privilege level of the subject with given ID
   --  is zero.
   function Is_CPL_0 (ID : Skp.Global_Subject_ID_Type) return Boolean
   with
      Global => (Input => State);

   --  Move instruction pointer of subject with given ID to next instruction.
   procedure Increment_RIP (ID : Skp.Global_Subject_ID_Type)
   with
      Global  => (In_Out => State),
      Depends => (State  =>+ ID);

   --  Returns True if required invariants hold for given subject state.
   function Valid_State (ID : Skp.Global_Subject_ID_Type) return Boolean
   with
      Ghost;

   --  Restore VMCS guest state from the subject state identified by ID.
   --  The Regs field of the subject state is returned to the caller.
   procedure Restore_State
     (ID   :     Skp.Global_Subject_ID_Type;
      Regs : out SK.CPU_Registers_Type)
   with
      Global  => (Input  => (State, CPU_Info.APIC_ID),
                  In_Out => (Crash_Audit.State, X86_64.State)),
      Depends => ((Crash_Audit.State,
                   X86_64.State) => (ID, State, CPU_Info.APIC_ID,
                                     Crash_Audit.State, X86_64.State),
                  Regs           => (ID, State)),
      Pre     => Valid_State (ID => ID);

   --  Ensure subject state invariants.
   procedure Filter_State (ID : Skp.Global_Subject_ID_Type)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ ID),
      Post    => Valid_State (ID => ID);

   --  Save registers and VMCS guest data to the state of the subject
   --  identified by ID.
   procedure Save_State
     (ID          : Skp.Global_Subject_ID_Type;
      Exit_Reason : Word64;
      Regs        : SK.CPU_Registers_Type)
   with
      Global  => (Input  => CPU_Info.APIC_ID,
                  In_Out => (State, Crash_Audit.State, X86_64.State)),
      Depends => (State               =>+ (ID, Exit_Reason, Regs,
                                           CPU_Info.APIC_ID, Crash_Audit.State,
                                           X86_64.State),
                  (Crash_Audit.State,
                   X86_64.State)      => (CPU_Info.APIC_ID, Crash_Audit.State,
                                          X86_64.State));

   --  Reset state of subject with given ID to the specified initial values.
   procedure Reset_State
     (ID       : Skp.Global_Subject_ID_Type;
      GPRs     : CPU_Registers_Type;
      RIP      : Word64;
      RSP      : Word64;
      CR0      : Word64;
      CR4      : Word64;
      Segments : Segment_Registers_Type)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ (ID, GPRs, RIP, RSP, CR0, CR4, Segments));

   --  Create crash audit context for subject with given ID.
   procedure Create_Context
     (ID  :     Skp.Global_Subject_ID_Type;
      Ctx : out Crash_Audit_Types.Subj_Context_Type)
   with
      Global => (Input  => (State, CPU_Info.APIC_ID),
                 In_Out => (Crash_Audit.State, X86_64.State));

private

   pragma Warnings
     (Off,
      "component size overrides size clause for ""Subject_State_Type""",
      Reason => "Reserved memory size is bigger than actual size of type");
   pragma Warnings (GNAT, Off, "*padded by * bits");
   type Subject_State_Array is array
     (Skp.Global_Subject_ID_Type) of SK.Subject_State_Type
   with
      Independent_Components,
      Component_Size => Page_Size * 8,
      Alignment      => Page_Size;
   pragma Warnings (GNAT, On, "*padded by * bits");
   pragma Warnings
     (On,
      "component size overrides size clause for ""Subject_State_Type""",
      Reason => "Reserved memory size is bigger than actual size of type");

   --  Descriptors used to manage subject states.
   --  TODO: Model access rules
   --  TODO: Handle initialization
   Descriptors : Subject_State_Array
   with
      Part_Of => State,
      Address => System'To_Address (Skp.Kernel.Subj_States_Address);
   pragma Annotate
     (GNATprove, Intentional,
      "not initialized",
      "Subject states are initialized by their owning CPU. Not yet modeled");

end SK.Subjects;
