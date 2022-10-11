--
--  Copyright (C) 2013, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp.Events;
with Skp.Interrupts;
with Skp.Kernel;
with Skp.Subjects;

with SK.Apic;
with SK.Bitops;
with SK.CPU;
with SK.Constants;
with SK.Dump;
with SK.KC;
with SK.Version;
with SK.Power;
with SK.Strings;
with SK.Subjects.Debug;
with SK.System_State;
with SK.VTd.Debug;
with SK.VTd.Interrupts;
with SK.Interrupts;
with SK.Crash_Audit_Types;

package body SK.Kernel
is

   -------------------------------------------------------------------------

   use type Crash_Audit_Types.Reason_Type;

   --  Allocate crash audit entry for given error and trigger system restart.
   procedure Error
     (Reason   : Crash_Audit_Types.Reason_Type;
      Subj_Ctx : Crash_Audit_Types.Subj_Context_Type
      := Crash_Audit_Types.Null_Subj_Context;
      MCE_Ctx  : Crash_Audit_Types.MCE_Context_Type
      := Crash_Audit_Types.Null_MCE_Context)
   with
      Pre => Reason /= Crash_Audit_Types.Reason_Undefined,
      No_Return
   is
      use type Crash_Audit_Types.Subj_Context_Type;
      use type Crash_Audit_Types.MCE_Context_Type;

      A : Crash_Audit.Entry_Type;
   begin
      Crash_Audit.Allocate (Audit => A);
      Crash_Audit.Set_Reason
        (Audit  => A,
         Reason => Reason);
      if Subj_Ctx /= Crash_Audit_Types.Null_Subj_Context then
         Crash_Audit.Set_Subject_Context
           (Audit   => A,
            Context => Subj_Ctx);
      end if;
      if MCE_Ctx /= Crash_Audit_Types.Null_MCE_Context then
         Crash_Audit.Set_MCE_Context
           (Audit   => A,
            Context => MCE_Ctx);
      end if;
      Crash_Audit.Finalize (Audit => A);
   end Error;

   -------------------------------------------------------------------------

   --  Inject pending interrupt into subject identified by ID. Sets interrupt
   --  window if interrupt(s) remain pending.
   --D @Section Id => impl_inject_interrupt, Label => Interrupt Injection, Parent => impl_exit_handler, Priority => 70
   procedure Inject_Interrupt (Subject_ID : Skp.Global_Subject_ID_Type)
   with
      Global => (Input  => (CPU_Info.APIC_ID, Subjects.State),
                 In_Out => (Crash_Audit.State, Subjects_Interrupts.State,
                            X86_64.State))
   is
      Vector            : Byte;
      Interrupt_Pending : Boolean;
   begin
      if Subjects.Accepts_Interrupts (ID => Subject_ID) then
         --D @Text Section => impl_inject_interrupt, Priority => 10
         --D If a subject is ready to accept interrupts, check if it has a
         --D pending interrupt.
         Subjects_Interrupts.Consume_Interrupt
           (Subject => Subject_ID,
            Found   => Interrupt_Pending,
            Vector  => Vector);

         if Interrupt_Pending then
            --D @Text Section => impl_inject_interrupt, Priority => 10
            --D Consume the pending interrupt by writing the corresponding
            --D vector to the VM-entry interruption-information and setting the
            --D valid bit, see Intel SDM Vol. 3C, "26.6 Event Injection".
            VMX.VMCS_Write
              (Field => Constants.VM_ENTRY_INTERRUPT_INFO,
               Value => Constants.VM_INTERRUPT_INFO_VALID + Word64 (Vector));
         end if;
      end if;

      --D @Text Section => impl_inject_interrupt, Priority => 10
      --D Then, check if the subject has more pending interrupts and activate
      --D interrupt window exiting if required, see Intel SDM Vol. 3C, "26.7.5
      --D Interrupt-Window Exiting and Virtual-Interrupt Delivery".
      Interrupt_Pending := Subjects_Interrupts.Has_Pending_Interrupt
        (Subject => Subject_ID);
      if Interrupt_Pending then
         VMX.VMCS_Set_Interrupt_Window (Value => True);
      end if;
   end Inject_Interrupt;

   -------------------------------------------------------------------------

   --D @Section Id => impl_handle_target_event, Label => Target Event Handling, Parent => impl_exit_handler, Priority => 60
   --D @Text Section => impl_handle_target_event
   --D Target events are actions performed prior to resuming execution of a
   --D given subject.
   procedure Handle_Pending_Target_Event
     (Subject_ID : Skp.Global_Subject_ID_Type)
   with
      Global =>
        (Input  => (CPU_Info.APIC_ID, Interrupt_Tables.State,
                    VMX.Exit_Address),
         In_Out => (Crash_Audit.State, FPU.State, Subjects.State,
                    Subjects_Events.State, Subjects_Interrupts.State,
                    Subjects_MSR_Store.State, Timed_Events.State,
                    VMX.VMCS_State, X86_64.State))
   is
      Found    : Boolean;
      Event_ID : Skp.Events.Event_Range;
   begin
      loop
         --D @Text Section => impl_handle_target_event
         --D First, check if the subject specified by ID has a target event pending
         --D by consulting the subject events data.
         Subjects_Events.Consume_Event
           (Subject => Subject_ID,
            Found   => Found,
            Event   => Event_ID);

         exit when not Found;

         --D @Text Section => impl_handle_target_event
         --D If an event is pending, it is consumed by looking up the target
         --D event and its action as specified by the policy.
         --D @UL Id => impl_handle_target_event_actions, Section => impl_handle_target_event
         declare
            Cur_Event : constant Skp.Events.Target_Event_Type
              := Skp.Events.Get_Target_Event (Subject_ID => Subject_ID,
                                              Event_Nr   => Event_ID);
         begin
            case Skp.Events.Get_Kind (Target_Event => Cur_Event)
            is
               when Skp.Events.No_Action        => null;
                  --D @Item List => impl_handle_target_event_actions
                  --D If the designated action is no action, then nothing is
                  --D done.
               when Skp.Events.Inject_Interrupt =>
                  --D @Item List => impl_handle_target_event_actions
                  --D If the designated action is interrupt injection, then the
                  --D interrupt with the vector specified in the policy is
                  --D marked as pending for the subject.
                  Subjects_Interrupts.Insert_Interrupt
                    (Subject => Subject_ID,
                     Vector  => Byte (Skp.Events.Get_Vector
                       (Target_Event => Cur_Event)));
               when Skp.Events.Reset            =>
                  --D @Item List => impl_handle_target_event_actions
                  --D If the designated action is subject reset, then the
                  --D subject state is initialized, see \ref{impl_subject_init}.
                  Init_Subject (ID => Subject_ID);
            end case;
         end;
      end loop;
   end Handle_Pending_Target_Event;

   -------------------------------------------------------------------------

   --D @Section Id => impl_handle_system_panic, Label => System Panic Action Handling, Parent => impl_handle_source_event, Priority => 10
   procedure Handle_System_Panic (Subject : Skp.Global_Subject_ID_Type)
   with
       Global => (Input  => (CPU_Info.APIC_ID, FPU.State, Subjects.State),
                  In_Out => (Crash_Audit.State, X86_64.State)),
       No_Return
   is
      S : Crash_Audit_Types.Subj_Context_Type;
   begin
      --D @Text Section => impl_handle_system_panic
      --D A system panic action triggered by a source event of a given subject
      --D is handled by creating a crash audit entry with the state of the
      --D triggering subject and invoking the crash audit facility.
      Subjects.Create_Context (ID  => Subject,
                               Ctx => S);
      FPU.Get_Registers (ID   => Subject,
                         Regs => S.FPU_Registers);
      pragma Debug (Dump.Print_Message (Msg => ">>> System Panic triggered"));
      pragma Debug (Subjects.Debug.Print_State (S => S));
      Error (Reason   => Crash_Audit_Types.Subj_System_Panic,
             Subj_Ctx => S);
   end Handle_System_Panic;

   -------------------------------------------------------------------------

   --D @Section Id => impl_handle_source_event, Label => Source Event Handling, Parent => impl_exit_handler, Priority => 50
   --D @Text Section => impl_handle_source_event
   --D Source events are actions performed when a given subject triggers a trap
   --D or a hypercall. Source events can also be triggered by the timed event
   --D mechanism.
   procedure Handle_Source_Event
     (Subject      :     Skp.Global_Subject_ID_Type;
      Event        :     Skp.Events.Source_Event_Type;
      Next_Subject : out Skp.Global_Subject_ID_Type)
   with
      Global =>
        (Input  => (CPU_Info.APIC_ID, CPU_Info.CPU_ID, FPU.State,
                   Subjects.State),
         In_Out => (Crash_Audit.State, IO_Apic.State,
                     Scheduler.State, Subjects_Events.State, X86_64.State))
   is
      use type Skp.CPU_Range;
      use type Skp.Events.Target_Event_Range;

      Dst_CPU : Skp.CPU_Range;
   begin
      --D @Text Section => impl_handle_source_event
      --D First, the next subject to be executed is initialized to the current
      --D one. A handover event may change this but otherwise the same subject
      --D is to be executed next.
      Next_Subject := Subject;

      --D @Text Section => impl_handle_source_event
      --D Then the operation corresponding to the given source event action is
      --D performed.
      --D @UL Id => impl_handle_source_event_actions, Section => impl_handle_source_event, Priority => 10
      case Event.Source_Action
      is
         when Skp.Events.No_Action       => null;
            --D @Item List => impl_handle_source_event_actions
            --D If the designated action is no action, then nothing is
            --D done.
         when Skp.Events.Subject_Sleep   =>
            Scheduler.Reschedule_Partition (Subject_ID => Subject,
                                            Sleep      => True);
         when Skp.Events.Subject_Yield   =>
            Scheduler.Reschedule_Partition (Subject_ID => Subject,
                                            Sleep      => False);
         when Skp.Events.System_Reboot   =>
            --D @Item List => impl_handle_source_event_actions
            --D If the designated action is system reboot, then a reboot with
            --D power-cycle is initiated.
            Power.Reboot (Power_Cycle => True);
         when Skp.Events.System_Poweroff =>
            --D @Item List => impl_handle_source_event_actions
            --D If the designated action is system poweroff, then a shutdown is
            --D initiated.
            Power.Shutdown;
         when Skp.Events.System_Panic =>
            --D @Item List => impl_handle_source_event_actions
            --D If the designated action is system panic, then the system panic
            --D handler is invoked.
            Handle_System_Panic (Subject => Subject);
         when Skp.Events.Unmask_Irq      =>
            --D @Item List => impl_handle_source_event_actions
            --D If the designated action is unmask IRQ, then use I/O APIC to
            --D unmask the IRQ designated by the event's IRQ number.
            IO_Apic.Unmask_IRQ
              (RTE_Index => Skp.Interrupts.RTE_Index_Type (Event.IRQ_Number));
      end case;

      if Event.Target_Subject /= Skp.Invalid_Subject then
         if Event.Target_Event /= Skp.Events.Invalid_Target_Event then
            --D @Text Section => impl_handle_source_event, Priority => 20
            --D If the source event has a valid target subject and target event
            --D set, then mark the target event pending for the designated
            --D subject.
            Subjects_Events.Set_Event_Pending
              (Subject  => Event.Target_Subject,
               Event_ID => Event.Target_Event);

            Dst_CPU := Skp.Subjects.Get_CPU_ID
              (Subject_ID => Event.Target_Subject);

            Scheduler.Indicate_Activity
              (Subject_ID => Event.Target_Subject,
               Same_CPU   => Dst_CPU = CPU_Info.CPU_ID);

            if Event.Send_IPI then
               --D @Text Section => impl_handle_source_event, Priority => 20
               --D Additionally, send an IPI to the CPU running the target
               --D subject if specified by the policy.
               Apic.Send_IPI (Vector => Constants.IPI_Vector,
                              CPU_ID => Dst_CPU);
            end if;
         end if;

         if Event.Handover then
            --D @Text Section => impl_handle_source_event, Priority => 20
            --D If the source event has a valid target subject and it is a
            --D handover event, then set the target subject as the next subject
            --D to run.
            Next_Subject := Event.Target_Subject;
            Scheduler.Set_Current_Subject_ID (Subject_ID => Next_Subject);
         end if;
      end if;
   end Handle_Source_Event;

   -------------------------------------------------------------------------

   --  Handle hypercall with given event number.
   --D @Section Id => hypercall_handling, Label => Hypercall Handling, Parent => impl_exit_handler, Priority => 20
   --D @Text Section => hypercall_handling
   --D Hypercalls can be triggered by subjects executing the \verb!vmcall!
   --D instruction in guest privilege level 0, which is assured by means of a
   --D precondition check. If subject user space/ring-3 tries to invoke
   --D hypercalls, the VM-Exit is handled as a trap with exit reason
   --D \verb!VMCALL!, see \verb!Handle_Trap!.
   procedure Handle_Hypercall
     (Current_Subject    : Skp.Global_Subject_ID_Type;
      Unchecked_Event_Nr : Word64)
   with
      Global =>
        (Input  => (CPU_Info.APIC_ID, CPU_Info.CPU_ID, FPU.State),
         In_Out => (Crash_Audit.State, IO_Apic.State, Scheduler.State,
                    Subjects.State, Subjects_Events.State, X86_64.State)),
      Pre    => Subjects.Is_CPL_0 (ID => Current_Subject)
   is
      --  XXX: Only current wavefronts report that use type clause has no
      --       effect. To keep compatibility with earlier toolchains disable
      --       all warnings.
      pragma $Release_Warnings (Off, -- "use clause for type * has no effect",
                                Reason => "Only used for debug output");
      use type Skp.Events.Source_Event_Type;
      pragma $Release_Warnings (On);

      --  The following code operating on Unchecked_Event_Nr was specifically
      --  constructed to defend against Spectre Variant 1 attacks
      --  (CVE-2017-5753), i.e. it is important NOT to use the unchecked
      --  user-supplied value as index into an array after a conditional
      --  checking this variable.

      Event_Nr        : constant Skp.Events.Event_Range
        := Skp.Events.Event_Range
          (Unchecked_Event_Nr and Skp.Events.Event_Mask);
      Valid_Event_Nr  : constant Boolean
        := Word64 (Event_Nr) = Unchecked_Event_Nr;
      Event           : Skp.Events.Source_Event_Type;
      Next_Subject_ID : Skp.Global_Subject_ID_Type := Current_Subject;
   begin
      --D @Text Section => hypercall_handling
      --D First the event number of the hypercall is checked. If it is valid
      --D then the corresponding subject source event as specified by the
      --D policy is looked up and processed, see
      --D \ref{impl_handle_source_event}. Note that events that are not
      --D specified in the policy are ignored since these are initialized to
      --D source events that have no action and an invalid target subject.
      if Valid_Event_Nr then
         Event := Skp.Events.Get_Source_Event
           (Subject_ID => Current_Subject,
            Event_Nr   => Event_Nr);
         Handle_Source_Event
           (Subject      => Current_Subject,
            Event        => Event,
            Next_Subject => Next_Subject_ID);
      end if;

      pragma Debug (not Valid_Event_Nr or else
                    Event = Skp.Events.Null_Source_Event,
                    Dump.Print_Spurious_Event
                      (Current_Subject => Current_Subject,
                       Event_Nr        => Unchecked_Event_Nr));

      --D @Text Section => hypercall_handling
      --D After handling the source event, the instruction pointer of the
      --D current subject is incremented so execution resumes after the
      --D \texttt{vmcall} instruction.
      Subjects.Increment_RIP (ID => Current_Subject);

      --D @Text Section => hypercall_handling, Priority => 20
      --D If the hypercall triggered a handover event, load the new VMCS.
      if Current_Subject /= Next_Subject_ID then
         VMX.Load (VMCS_Address => Skp.Subjects.Get_VMCS_Address
                   (Subject_ID => Next_Subject_ID));
      end if;
   end Handle_Hypercall;

   -------------------------------------------------------------------------

   --  Handle external interrupt request with given vector.
   --D @Section Id => impl_handle_irq, Label => External Interrupt Handling, Parent => impl_exit_handler, Priority => 10
   procedure Handle_Irq (Vector : Byte)
   with
      Global => (In_Out => (IO_Apic.State, Subjects_Interrupts.State,
                            X86_64.State))
   is
      Vect_Nr : Skp.Interrupts.Remapped_Vector_Type;
      Route   : Skp.Interrupts.Vector_Route_Type;
   begin
      --D @Text Section => impl_handle_irq
      --D First the vector of the external interrupt is validated. If it is an
      --D IPI or VT-d fault vector, no further action is taken since the purpose
      --D was to force a VM exit of the currently executing subject. A
      --D subsequent subject VM entry leads to the evaluation of pending target
      --D events and subject interrupts.
      if Vector >= Skp.Interrupts.Remap_Offset
        and then Vector < Constants.VTd_Fault_Vector
      then
         --D @Text Section => impl_handle_irq
         --D \paragraph{}
         --D If the vector is valid and neither an IPI nor VT-d fault vector,
         --D consult the vector routing table to determine the target subject
         --D and vector as specified by the policy and insert the target
         --D vector by marking it as pending. Note that there is no
         --D switching to the destination of the IRQ. The interrupt will be
         --D delivered whenever the target subject is executed according to the
         --D scheduling plan (i.e. IRQs are not preemptive).
         Vect_Nr := Skp.Interrupts.Remapped_Vector_Type (Vector);
         Route   := Skp.Interrupts.Vector_Routing (Vect_Nr);
         if Route.Subject in Skp.Global_Subject_ID_Type then
            Subjects_Interrupts.Insert_Interrupt
              (Subject => Route.Subject,
               Vector  => Byte (Route.Vector));
         end if;

         --D @Text Section => impl_handle_irq
         --D \paragraph{}
         --D If the interrupt vector designates an IRQ that must be masked,
         --D instruct the I/O APIC to mask the corresponding redirection
         --D table entry.
         if Vect_Nr in Skp.Interrupts.Mask_IRQ_Type then
            IO_Apic.Mask_IRQ
              (RTE_Index => Skp.Interrupts.RTE_Index_Type (Vector - 32));
         end if;

         pragma Debug (Route.Subject not in Skp.Global_Subject_ID_Type,
                       Dump.Print_Message
                         (Msg => "Spurious IRQ vector "
                          & Strings.Img (Vector)));
      end if;

      pragma Debug (Vector = Constants.VTd_Fault_Vector,
                    VTd.Debug.Process_Fault);
      pragma Debug (Vector < Skp.Interrupts.Remap_Offset,
                    Dump.Print_Message
                      (Msg => "IRQ with invalid vector "
                       & Strings.Img (Vector)));

      --D @Text Section => impl_handle_irq
      --D \paragraph{}
      --D Finally, signal to the local APIC that the interrupt servicing has
      --D completed and other IRQs may be issued once interrupts are re-enabled.
      Apic.EOI;
   end Handle_Irq;

   -------------------------------------------------------------------------

   --  Handle Xsetbv trap of current subject by validating new XCR0 value and
   --  setting the corresponding register in the associated FPU state.
   --D @Section Id => impl_handle_xsetbv, Label => Xsetbv handling, Parent => impl_exit_handler, Priority => 80
   procedure Handle_Xsetbv
     (Current_Subject : Skp.Global_Subject_ID_Type;
      RAX, RDX, RCX   : Word64)
   is
      Supported : constant Word64 := FPU.Get_Active_XCR0_Features;
      Index     : constant Word32 := Word32'Mod (RCX);
      New_Value : constant Word64 := Word64 (Word32'Mod (RDX)) * 2 ** 32
        + Word64 (Word32'Mod (RAX));
   begin
      --D @Text Section => impl_handle_xsetbv
      --D Subjects can toggle FPU features by writing to XCR0 using the
      --D \verb!xsetbv! instruction. The provided value is validated according
      --D to Intel SDM Vol. 1,
      --D "13.3 Enabling the XSAVE Feature Set and XSAVE-Enabled Features":
      --D @OL Id => impl_handle_xsetbv_checks, Section => impl_handle_xsetbv, Priority => 10
      --D @Item List => impl_handle_xsetbv_checks
      --D Privilege Level (CPL0) must be 0.
      if not Subjects.Is_CPL_0 (ID => Current_Subject)
        --D @Item List => impl_handle_xsetbv_checks
        --D Register index must be 0, only XCR0 is supported.
        or else Index /= 0
        --D @Item List => impl_handle_xsetbv_checks
        --D Only bits that we support must be set.
        or else ((Supported or New_Value) /= Supported)
          --D @Item List => impl_handle_xsetbv_checks
          --D XCR0_FPU_STATE_FLAG must always be set
        or else not Bitops.Bit_Test
          (Value => New_Value,
           Pos   => Constants.XCR0_FPU_STATE_FLAG)
        --D @Item List => impl_handle_xsetbv_checks
        --D If XCR0_AVX_STATE_FLAG is set then XCR0_SSE_STATE_FLAG must be set as
        --D well.
        or else (Bitops.Bit_Test
                 (Value => New_Value,
                  Pos   => Constants.XCR0_AVX_STATE_FLAG)
                 and not Bitops.Bit_Test
                   (Value => New_Value,
                    Pos   => Constants.XCR0_SSE_STATE_FLAG))
        --D @Item List => impl_handle_xsetbv_checks
        --D If any of XCR0_OPMASK_STATE_FLAG or XCR0_ZMM_HI256_STATE_FLAG
        --D or XCR0_HI16_ZMM_STATE_FLAG are set then all must be set.
        or else ((Bitops.Bit_Test
                  (Value => New_Value,
                   Pos   => Constants.XCR0_OPMASK_STATE_FLAG)
                  or Bitops.Bit_Test
                    (Value => New_Value,
                     Pos   => Constants.XCR0_ZMM_HI256_STATE_FLAG)
                  or Bitops.Bit_Test
                    (Value => New_Value,
                     Pos   => Constants.XCR0_ZMM_HI256_STATE_FLAG))
                 and not
                   (Bitops.Bit_Test
                        (Value => New_Value,
                         Pos   => Constants.XCR0_OPMASK_STATE_FLAG)
                    and Bitops.Bit_Test
                      (Value => New_Value,
                       Pos   => Constants.XCR0_ZMM_HI256_STATE_FLAG)
                    and Bitops.Bit_Test
                      (Value => New_Value,
                       Pos   => Constants.XCR0_ZMM_HI256_STATE_FLAG)))
        --D @Item List => impl_handle_xsetbv_checks
        --D If AVX512 is set then XCR0_AVX_STATE_FLAG must be set as well.
        or else (Bitops.Bit_Test
                 (Value => New_Value,
                  Pos   => Constants.XCR0_OPMASK_STATE_FLAG)
                 and not Bitops.Bit_Test
                   (Value => New_Value,
                    Pos   => Constants.XCR0_AVX_STATE_FLAG))
      then

         --D @Text Section => impl_handle_xsetbv
         --D If the value is invalid, inject a #GP exception. Note that
         --D effectively the inserted event has type \emph{external interrupt}.
         --D While it would not work in general but in this specific case the
         --D exception error code is 0.

         Subjects_Interrupts.Insert_Interrupt
           (Subject => Current_Subject,
            Vector  => Constants.GP_Exception_Vector);
      else

         --D @Text Section => impl_handle_xsetbv
         --D If the value is valid, set the corresponding subject XCR0 value,
         --D increment the subject RIP and resume execution.

         FPU.Set_XCR0 (ID    => Current_Subject,
                       Value => New_Value);
         Subjects.Increment_RIP (ID => Current_Subject);
      end if;
   end Handle_Xsetbv;

   -------------------------------------------------------------------------

   --D @Section Id => impl_handle_timer_expiry, Label => Timer Expiry, Parent => impl_exit_handler, Priority => 40
   --D @Text Section => impl_handle_timer_expiry
   --D The VMX timer expiration designates the end of a minor frame. Handle the
   --D timer expiry by updating the current scheduling information and checking
   --D if a timed event has expired as well.
   procedure Handle_Timer_Expiry (Current_Subject : Skp.Global_Subject_ID_Type)
     with
       Global =>
         (Input  => (CPU_Info.APIC_ID, CPU_Info.CPU_ID, CPU_Info.Is_BSP,
                     FPU.State, Subjects.State, Tau0_Interface.State),
          In_Out => (Crash_Audit.State, IO_Apic.State, MP.Barrier,
                     Scheduler.State, Scheduling_Info.State,
                     Subjects_Events.State, Timed_Events.State, X86_64.State))
   is
      Next_Subject_ID : Skp.Global_Subject_ID_Type;
   begin
      Scheduler.Update_Scheduling_Info (Next_Subject => Next_Subject_ID);

      --  Check and possibly handle timed event of subject.

      declare
         Event_Subj    : constant Skp.Global_Subject_ID_Type
           := Next_Subject_ID;
         Trigger_Value : Word64;
         Event_Nr      : Skp.Events.Event_Range;
         TSC_Now       : constant Word64 := CPU.RDTSC;
      begin

         --D @Text Section => impl_handle_timer_expiry, Priority => 20
         --D \paragraph{}
         --D Check if timed event has expired and handle source event if
         --D necessary.
         Timed_Events.Get_Event (Subject           => Event_Subj,
                                 TSC_Trigger_Value => Trigger_Value,
                                 Event_Nr          => Event_Nr);
         if Trigger_Value <= TSC_Now then
            Handle_Source_Event
              (Subject      => Event_Subj,
               Event        => Skp.Events.Get_Source_Event
                 (Subject_ID => Event_Subj,
                  Event_Nr   => Skp.Events.Target_Event_Range (Event_Nr)),
               Next_Subject => Next_Subject_ID);
            Timed_Events.Clear_Event (Subject => Event_Subj);
         end if;
      end;

      --D @Text Section => impl_handle_timer_expiry, Priority => 20
      --D If the new minor frame designates a different subject, load its VMCS.
      if Current_Subject /= Next_Subject_ID then

         --  New minor frame contains different subject -> Load VMCS.

         VMX.Load (VMCS_Address => Skp.Subjects.Get_VMCS_Address
                   (Subject_ID => Next_Subject_ID));
      end if;
   end Handle_Timer_Expiry;

   -------------------------------------------------------------------------

   --  Handle trap with given number using trap table of current subject.
   --D @Section Id => impl_handle_trap, Label => Trap Handling, Parent => impl_exit_handler, Priority => 30
   procedure Handle_Trap
     (Current_Subject : Skp.Global_Subject_ID_Type;
      Trap_Nr         : Word16)
   with
      Global =>
        (Input  => (CPU_Info.APIC_ID, CPU_Info.CPU_ID, FPU.State,
                    Subjects.State),
         In_Out => (Crash_Audit.State, IO_Apic.State, Scheduler.State,
                    Subjects_Events.State, X86_64.State))
   is
      Trap_Entry      : Skp.Events.Source_Event_Type;
      Next_Subject_ID : Skp.Global_Subject_ID_Type;
      Valid_Trap_Nr   : Boolean;

      ----------------------------------------------------------------------

      procedure Panic_Unknown_Trap
      with
         Global => (Input  => (Current_Subject, CPU_Info.APIC_ID, FPU.State,
                               Subjects.State),
                    In_Out => (Crash_Audit.State, X86_64.State)),
         No_Return
      is
         S : Crash_Audit_Types.Subj_Context_Type;
      begin
         Subjects.Create_Context (ID  => Current_Subject,
                                  Ctx => S);
         FPU.Get_Registers (ID   => Current_Subject,
                            Regs => S.FPU_Registers);
         pragma Debug (Dump.Print_Message (Msg => ">>> Unknown trap "
                                           & Strings.Img (Trap_Nr)));
         pragma Debug (Subjects.Debug.Print_State (S => S));
         Error (Reason   => Crash_Audit_Types.Subj_Unknown_Trap,
                Subj_Ctx => S);
      end Panic_Unknown_Trap;
   begin
      --D @Text Section => impl_handle_trap
      --D First the trap number is checked. If it is outside the valid trap
      --D range an appropriate crash audit record is written and an error
      --D condition is signaled.
      Valid_Trap_Nr := Trap_Nr <= Word16 (Skp.Events.Trap_Range'Last);
      if not Valid_Trap_Nr then
         Panic_Unknown_Trap;
      end if;

      --D @Text Section => impl_handle_trap
      --D \paragraph{}
      --D If the trap number is valid then the corresponding subject trap entry
      --D as specified by the policy is looked up. Note that the policy
      --D validation tools enforce that an event must be specified for each
      --D trap ID.
      Trap_Entry := Skp.Events.Get_Trap
        (Subject_ID => Current_Subject,
         Trap_Nr    => Skp.Events.Trap_Range (Trap_Nr));

      --D @Text Section => impl_handle_trap
      --D The source event designated by the policy trap entry is processed,
      --D see \ref{impl_handle_source_event}.
      Handle_Source_Event
        (Subject      => Current_Subject,
         Event        => Trap_Entry,
         Next_Subject => Next_Subject_ID);

      --D @Text Section => impl_handle_trap, Priority => 20
      --D If the trap triggered a handover event, load the new VMCS.
      if Current_Subject /= Next_Subject_ID then
         VMX.Load (VMCS_Address => Skp.Subjects.Get_VMCS_Address
                   (Subject_ID => Next_Subject_ID));
      end if;
   end Handle_Trap;

   -------------------------------------------------------------------------

   --D @Section Id => impl_exit_handler, Label => VMX Exit Handling, Parent => implementation, Priority => -5
   --D @Text Section => impl_exit_handler
   --D The VMX exit handle procedure \texttt{Handle\_Vmx\_Exit} is the main
   --D subprogram of the kernel.
   --D It is invoked whenever the execution of a subject stops and an exit into
   --D VMX root mode is performed by the hardware. The register state of the
   --D current subject is passed to the procedure by the
   --D \texttt{vmx\_exit\_handler} assembly code (which is set as kernel entry
   --D point in the VMCS of the trapping subject).
   procedure Handle_Vmx_Exit (Subject_Registers : in out CPU_Registers_Type)
   is
      --  See Intel SDM Vol. 3C, "27.2.2 Information for VM Exits Due to
      --  Vectored Events".
      Exception_Mask : constant := 16#07ff#;
      Exception_NMI  : constant := 16#0202#;
      Exception_MCE  : constant := 16#0312#;

      Exit_Reason            : Word64;
      Exit_Interruption_Info : Word64;
      Basic_Exit_Reason      : Word16;
      Current_Subject        : Skp.Global_Subject_ID_Type;
   begin
      Current_Subject := Scheduler.Get_Current_Subject_ID;

      VMX.VMCS_Read (Field => Constants.VMX_EXIT_REASON,
                     Value => Exit_Reason);
      Basic_Exit_Reason := Word16 (Exit_Reason and 16#ffff#);

      --D @Text Section => impl_exit_handler
      --D The \texttt{Handle\_Vmx\_Exit} procedure first saves the state of the
      --D subject that has just trapped into the exit handler, along with the
      --D register values and the exit reason, see
      --D \ref{impl_subjects_state_save}.
      --D Analogously, the FPU state of the current subject is saved.
      Subjects.Save_State (ID          => Current_Subject,
                           Exit_Reason => Exit_Reason,
                           Regs        => Subject_Registers);

      FPU.Save_State (ID => Current_Subject);

      VMX.VMCS_Read (Field => Constants.VMX_EXIT_INTR_INFO,
                     Value => Exit_Interruption_Info);

      --D @Text Section => impl_exit_handler
      --D Then, the exit reason is examined and depending on the cause the
      --D corresponding handler is called.
      --D \paragraph{}
      --D If an unrecoverable error occurs, i.e. NMI or MCE, a crash audit
      --D record with the appropriate error information is allocated and the
      --D kernel performs a controlled system restart.
      if Basic_Exit_Reason = Constants.EXIT_REASON_EXTERNAL_INT then
         Handle_Irq (Vector => Byte'Mod (Exit_Interruption_Info));
      elsif Basic_Exit_Reason = Constants.EXIT_REASON_VMCALL
        and then Subjects.Is_CPL_0 (ID => Current_Subject)
      then
         Handle_Hypercall (Current_Subject    => Current_Subject,
                           Unchecked_Event_Nr => Subject_Registers.RAX);
      elsif Basic_Exit_Reason = Constants.EXIT_REASON_TIMER_EXPIRY then
         Handle_Timer_Expiry (Current_Subject => Current_Subject);
      elsif Basic_Exit_Reason = Constants.EXIT_REASON_INTERRUPT_WINDOW then

         --  Resume subject to inject pending interrupt.

         VMX.VMCS_Set_Interrupt_Window (Value => False);
      elsif Basic_Exit_Reason = Constants.EXIT_REASON_EXCEPTION_NMI
        and then
          (Exit_Interruption_Info and Exception_Mask) = Exception_NMI
      then
         pragma Debug (Dump.Print_Message
                       (Msg => "*** CPU APIC ID " & Strings.Img
                        (Byte (CPU_Info.APIC_ID))
                        & " VM exit due to NMI; interruption information "
                        & Strings.Img (Exit_Interruption_Info)));
         Error (Reason => Crash_Audit_Types.Hardware_VMexit_NMI);
      elsif Basic_Exit_Reason = Constants.EXIT_REASON_EXCEPTION_NMI
        and then (Exit_Interruption_Info and Exception_Mask) = Exception_MCE
      then
         pragma Debug (Dump.Print_Message
                       (Msg => "*** CPU APIC ID " & Strings.Img
                        (Byte (CPU_Info.APIC_ID))
                        & " VM exit due to MCE; interruption information "
                        & Strings.Img (Exit_Interruption_Info)));
         declare
            Ctx : Crash_Audit_Types.MCE_Context_Type;
         begin
            MCE.Create_Context (Ctx => Ctx);
            Error (Reason  => Crash_Audit_Types.Hardware_VMexit_MCE,
                   MCE_Ctx => Ctx);
         end;
      elsif Basic_Exit_Reason = Constants.EXIT_REASON_ENTRY_FAIL_MCE then
         pragma Debug (Dump.Print_Message
                       (Msg => "*** CPU APIC ID " & Strings.Img
                        (Byte (CPU_Info.APIC_ID))
                        & " VM entry failed due to MCE"));
         declare
            Ctx : Crash_Audit_Types.MCE_Context_Type;
         begin
            MCE.Create_Context (Ctx => Ctx);
            Error (Reason  => Crash_Audit_Types.Hardware_VMentry_MCE,
                   MCE_Ctx => Ctx);
         end;
      elsif Basic_Exit_Reason = Constants.EXIT_REASON_XSETBV then
         Handle_Xsetbv (Current_Subject => Current_Subject,
                        RAX             => Subject_Registers.RAX,
                        RDX             => Subject_Registers.RDX,
                        RCX             => Subject_Registers.RCX);
      else
         Handle_Trap (Current_Subject => Current_Subject,
                      Trap_Nr         => Basic_Exit_Reason);
      end if;

      --D @Text Section => impl_exit_handler
      --D \paragraph{}
      --D Once the exit has been dealt with, the execution of the next subject
      --D is prepared. A pending target event, if present, is handled see
      --D \ref{impl_handle_target_event}.
      Current_Subject := Scheduler.Get_Current_Subject_ID;
      Handle_Pending_Target_Event (Subject_ID => Current_Subject);
      --D @Text Section => impl_exit_handler
      --D Then, a pending interrupt, if present, is prepared for injection, see
      --D \ref{impl_inject_interrupt}.
      Inject_Interrupt (Subject_ID => Current_Subject);

      --D @Text Section => impl_exit_handler
      --D \paragraph{}
      --D Finally, the VMX preemption timer is armed, the FPU and subject states
      --D are restored, see \ref{impl_subjects_state_restore}. Additionally, to
      --D ensure the precondition of \texttt{Subjects.Restore\_State}, the state
      --D is filtered beforehand, see \ref{impl_subjects_state_filter}.
      Scheduler.Set_VMX_Exit_Timer;
      FPU.Restore_State (ID => Current_Subject);
      Subjects.Filter_State (ID => Current_Subject);
      Subjects.Restore_State
        (ID   => Current_Subject,
         Regs => Subject_Registers);
      --D @Text Section => impl_exit_handler
      --D The register values of the subject to be executed are returned by the
      --D procedure. The calling assembler code then performs an entry to
      --D VMX non-root mode, thereby instructing the hardware to resume
      --D execution of the subject designated by the currently active VMCS.
   end Handle_Vmx_Exit;

   -------------------------------------------------------------------------

   --D @Section Id => impl_subject_init, Label => Subject Initialization, Parent => impl_kernel_init_sched, Priority => 20
   --D @Text Section => impl_subject_init
   --D Clear all state associated with the subject specified by ID and
   --D initialize to the values of the subject according to the policy. These
   --D steps are performed during startup and whenever a subject is reset.
   --D @OL Id => subject_init_steps, Section => impl_subject_init, Priority => 10
   procedure Init_Subject (ID : Skp.Global_Subject_ID_Type)
   is
      Controls  : constant Skp.Subjects.VMX_Controls_Type
        := Skp.Subjects.Get_VMX_Controls (Subject_ID => ID);
      VMCS_Addr : constant Word64
        := Skp.Subjects.Get_VMCS_Address (Subject_ID => ID);
      MSR_Count : constant Word32
        := Skp.Subjects.Get_MSR_Count (Subject_ID => ID);
   begin
      --D @Item List => subject_init_steps
      --D Reset FPU state for subject with given ID.
      FPU.Reset_State (ID => ID);
      --D @Item List => subject_init_steps
      --D Clear pending events of subject with given ID.
      Subjects_Events.Clear_Events (Subject => ID);
      --D @Item List => subject_init_steps
      --D Initialize pending interrupts of subject with given ID.
      Subjects_Interrupts.Init_Interrupts (Subject => ID);
      --D @Item List => subject_init_steps
      --D Initialize timed event of subject with given ID.
      Timed_Events.Init_Event (Subject => ID);

      if MSR_Count > 0 then
         --D @Item List => subject_init_steps
         --D Clear all MSRs in MSR storage area if subject has access to MSRs.
         Subjects_MSR_Store.Clear_MSRs (Subject => ID);
      end if;

      --D @Item List => subject_init_steps
      --D Reset VMCS of subject and make it active by loading it.
      VMX.Reset (VMCS_Address => VMCS_Addr,
                 Subject_ID   => ID);
      VMX.Load  (VMCS_Address => VMCS_Addr);
      --D @Item List => subject_init_steps
      --D Set VMCS control fields according to policy.
      VMX.VMCS_Setup_Control_Fields
        (IO_Bitmap_Address  => Skp.Subjects.Get_IO_Bitmap_Address
           (Subject_ID => ID),
         MSR_Bitmap_Address => Skp.Subjects.Get_MSR_Bitmap_Address
           (Subject_ID => ID),
         MSR_Store_Address  => Skp.Subjects.Get_MSR_Store_Address
           (Subject_ID => ID),
         MSR_Count          => MSR_Count,
         Ctls_Exec_Pin      => Controls.Exec_Pin,
         Ctls_Exec_Proc     => Controls.Exec_Proc,
         Ctls_Exec_Proc2    => Controls.Exec_Proc2,
         Ctls_Exit          => Controls.Exit_Ctrls,
         Ctls_Entry         => Controls.Entry_Ctrls,
         CR0_Mask           => Skp.Subjects.Get_CR0_Mask
           (Subject_ID => ID),
         CR4_Mask           => Skp.Subjects.Get_CR4_Mask
           (Subject_ID => ID),
         Exception_Bitmap   => Skp.Subjects.Get_Exception_Bitmap
           (Subject_ID => ID));
      --D @Item List => subject_init_steps
      --D Setup VMCS host fields.
      VMX.VMCS_Setup_Host_Fields;
      --D @Item List => subject_init_steps
      --D Setup VMCS guest fields according to policy.
      VMX.VMCS_Setup_Guest_Fields
        (PML4_Address => Skp.Subjects.Get_PML4_Address (Subject_ID => ID),
         EPT_Pointer  => Skp.Subjects.Get_EPT_Pointer (Subject_ID => ID));
      --D @Item List => subject_init_steps
      --D Reset CPU state of subject according to policy.
      Subjects.Reset_State
        (ID       => ID,
         GPRs     => Skp.Subjects.Get_GPRs (Subject_ID => ID),
         RIP      => Skp.Subjects.Get_Entry_Point (Subject_ID => ID),
         RSP      => Skp.Subjects.Get_Stack_Address (Subject_ID => ID),
         CR0      => Skp.Subjects.Get_CR0 (Subject_ID => ID),
         CR4      => Skp.Subjects.Get_CR4 (Subject_ID => ID),
         Segments => Skp.Subjects.Get_Segment_Registers (Subject_ID => ID));
   end Init_Subject;

   -------------------------------------------------------------------------

   --D @Section Id => impl_kernel_init, Label => Initialization, Parent => implementation, Priority => -10
   --D @Text Section => impl_kernel_init
   --D The \verb!SK.Kernel.Initialize! procedure is the Ada/SPARK entry point
   --D into the kernel during the boot phase. It is invoked from Assembler code
   --D after low-level system initialization has been performed.
   --D Kernel initialization consists of the following steps:
   --D @OL Id => impl_kernel_init_steps, Section => impl_kernel_init, Priority => 10
   procedure Initialize (Subject_Registers : out CPU_Registers_Type)
   is
   begin
      --D @Item List => impl_kernel_init_steps
      --D Initialize interrupt table (GDT, IDT) and setup interrupt stack.
      Interrupt_Tables.Initialize
        (Stack_Addr => Skp.Kernel.Intr_Stack_Address);

      pragma Debug (CPU_Info.Is_BSP, KC.Init);
      pragma Debug (CPU_Info.Is_BSP, KC.Put_Line
                    (Item => "Booting Muen kernel "
                     & Version.Version_String & " ("
                     & Standard'Compiler_Version & ")"));

      if CPU_Info.Is_BSP then
         --D @Item List => impl_kernel_init_steps
         --D Setup crash audit (BSP-only).
         Crash_Audit.Init;
      end if;

      declare
         Init_Ctx : Crash_Audit_Types.Init_Context_Type;

         Valid_Sys_State, Valid_FPU_State, Valid_MCE_State,
         Valid_VTd_State : Boolean;
      begin
         --D @Item List => impl_kernel_init_steps
         --D Validate required CPU (\ref{impl_kernel_init_check_state}),
         --D FPU, MCE and VT-d features.
         System_State.Check_State
           (Is_Valid => Valid_Sys_State,
            Ctx      => Init_Ctx.Sys_Ctx);
         FPU.Check_State
           (Is_Valid => Valid_FPU_State,
            Ctx      => Init_Ctx.FPU_Ctx);
         MCE.Check_State
           (Is_Valid => Valid_MCE_State,
            Ctx      => Init_Ctx.MCE_Ctx);
         VTd.Check_State
           (Is_Valid => Valid_VTd_State,
            Ctx      => Init_Ctx.VTd_Ctx);

         if not (Valid_Sys_State
                 and Valid_FPU_State
                 and Valid_MCE_State
                 and Valid_VTd_State)
         then
            declare
               Audit_Entry : Crash_Audit.Entry_Type;
            begin
               pragma Debug (KC.Put_Line
                             (Item => "System initialisation error"));

               --D @Item List => impl_kernel_init_steps
               --D If a required feature is not present, allocate a crash audit
               --D entry designating a system initialization failure and
               --D provide initialization context information.
               Subject_Registers := Null_CPU_Regs;
               Crash_Audit.Allocate (Audit => Audit_Entry);
               Crash_Audit.Set_Reason
                 (Audit  => Audit_Entry,
                  Reason => Crash_Audit_Types.System_Init_Failure);
               Crash_Audit.Set_Init_Context
                 (Audit   => Audit_Entry,
                  Context => Init_Ctx);
               Crash_Audit.Finalize (Audit => Audit_Entry);
            end;
         end if;

         --D @Item List => impl_kernel_init_steps
         --D Enable hardware features (FPU, APIC, MCE).
         FPU.Enable;
         Apic.Enable;
         MCE.Enable;

         if CPU_Info.Is_BSP then
            --D @Item List => impl_kernel_init_steps
            --D Setup of Multicore memory barries (BSP-only).
            MP.Initialize_All_Barrier;

            --D @Item List => impl_kernel_init_steps
            --D Disable legacy PIC/PIT (BSP-only).
            Interrupts.Disable_Legacy_PIT;
            Interrupts.Disable_Legacy_PIC;

            --D @Item List => impl_kernel_init_steps
            --D Setup of VT-d DMAR and IR (BSP-only).
            VTd.Initialize;
            VTd.Interrupts.Setup_IRQ_Routing;

            --D @Item List => impl_kernel_init_steps
            --D Initialize subject pending events (BSP-only).
            Subjects_Events.Initialize;

            --D @Item List => impl_kernel_init_steps
            --D Wake up application processors (BSP-only).
            Apic.Start_AP_Processors;
         end if;

         --D @Item List => impl_kernel_init_steps
         --D Synchronize all CPUs to make sure APs have performed all steps up
         --D until this point.
         MP.Wait_For_All;

         --D @Item List => impl_kernel_init_steps
         --D Enable VMX, enter VMX root-mode and initialize scheduler.
         System_State.Enable_VMX_Feature;
         VMX.Enter_Root_Mode;
         Scheduler.Init;

         --D @Item List => impl_kernel_init_steps
         --D Synchronize all logical CPUs prior to setting VMX preemption
         --D timer.
         MP.Wait_For_All;

         --D @Item List => impl_kernel_init_steps
         --D Arm VMX Exit timer of scheduler for preemption on end of initial
         --D minor frame.
         Scheduler.Set_VMX_Exit_Timer;

         declare
            Current_Subject : constant Skp.Global_Subject_ID_Type
              := Scheduler.Get_Current_Subject_ID;
         begin
            --D @Item List => impl_kernel_init_steps
            --D Prepare state of initial subject for execution.
            Subjects.Filter_State (ID => Current_Subject);
            Subjects.Restore_State
              (ID   => Current_Subject,
               Regs => Subject_Registers);
         end;
      end;

      --D @Text Section => impl_kernel_init, Priority => 20
      --D Registers of the first subject to schedule are returned by the
      --D initialization procedure to the calling assember code. The assembly
      --D then restores the subject register values prior to launching the first
      --D subject.
      --D This is done this way so the initialization code as well as the main
      --D VMX exit handler (\ref{impl_exit_handler}) operate the same way and
      --D the Assembler code in charge of resuming subject execution can be
      --D shared, which further simplifies the code flow.
   end Initialize;

end SK.Kernel;
