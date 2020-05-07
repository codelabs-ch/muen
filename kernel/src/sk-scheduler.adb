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

with Skp.Events;
with Skp.Interrupts;
with Skp.Subjects;

with SK.CPU;
with SK.Apic;
with SK.VTd;
with SK.Power;
with SK.Dump;
with SK.Subjects.Debug;
with SK.Strings;
with SK.Crash_Audit_Types;

package body SK.Scheduler
with
   Refined_State => (State => (Current_Minor_Frame_ID,
                               Global_Current_Major_Frame_ID,
                               Global_Current_Major_Start_Cycles,
                               Scheduling_Groups,
                               Scheduling_Plan))
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

   --  Set the currently active subject ID of the current scheduling group to
   --  the given value.
   procedure Set_Current_Subject_ID (Subject_ID : Skp.Global_Subject_ID_Type)
   with
      Global  => (Input  => (Current_Minor_Frame_ID,
                             Global_Current_Major_Frame_ID, Scheduling_Plan),
                  In_Out => Scheduling_Groups),
      Post    => Scheduling_Groups
       (Scheduling_Plan (Global_Current_Major_Frame_ID).Minor_Frames
        (Current_Minor_Frame_ID).Group_ID) = Subject_ID
   is
   begin
      Scheduling_Groups
        (Scheduling_Plan (Global_Current_Major_Frame_ID).Minor_Frames
         (Current_Minor_Frame_ID).Group_ID) := Subject_ID;
   end Set_Current_Subject_ID;

   -------------------------------------------------------------------------

   function Get_Current_Subject_ID return Skp.Global_Subject_ID_Type
   with
      Refined_Global => (Input => (Current_Minor_Frame_ID,
                                   Global_Current_Major_Frame_ID,
                                   Scheduling_Groups, Scheduling_Plan)),
      Refined_Post   =>
        Get_Current_Subject_ID'Result =
          Scheduling_Groups
            (Scheduling_Plan (Global_Current_Major_Frame_ID).Minor_Frames
             (Current_Minor_Frame_ID).Group_ID)
   is
   begin
      return Scheduling_Groups
        (Scheduling_Plan (Global_Current_Major_Frame_ID).Minor_Frames
         (Current_Minor_Frame_ID).Group_ID);
   end Get_Current_Subject_ID;

   -------------------------------------------------------------------------

   --  Inject pending interrupt into subject identified by ID. Sets interrupt
   --  window if interrupt(s) remain pending.
   procedure Inject_Interrupt (Subject_ID : Skp.Global_Subject_ID_Type)
   with
      Global => (Input  => (CPU_Info.APIC_ID, Subjects.State),
                 In_Out => (Crash_Audit.State, Subjects_Interrupts.State,
                            X86_64.State))
   is
      Vector            : SK.Byte;
      Interrupt_Pending : Boolean;
   begin
      if Subjects.Accepts_Interrupts (ID => Subject_ID) then
         Subjects_Interrupts.Consume_Interrupt
           (Subject => Subject_ID,
            Found   => Interrupt_Pending,
            Vector  => Vector);

         if Interrupt_Pending then
            VMX.VMCS_Write
              (Field => Constants.VM_ENTRY_INTERRUPT_INFO,
               Value => Constants.VM_INTERRUPT_INFO_VALID +
                 SK.Word64 (Vector));
         end if;
      end if;

      Subjects_Interrupts.Has_Pending_Interrupt
        (Subject           => Subject_ID,
         Interrupt_Pending => Interrupt_Pending);

      if Interrupt_Pending then
         VMX.VMCS_Set_Interrupt_Window (Value => True);
      end if;
   end Inject_Interrupt;

   -------------------------------------------------------------------------

   --  Update scheduling information. If the end of the current major frame is
   --  reached the major frame start time is updated by adding the period of
   --  the just expired major frame to the current start value. Additionally,
   --  the minor frame index is reset and the major frame is switched to the
   --  one set by Tau0.
   --  On regular minor frame switches the minor frame index is incremented by
   --  one.
   --  The ID of the next subject to schedule is returned to the caller.
   procedure Update_Scheduling_Info
     (Next_Subject : out Skp.Global_Subject_ID_Type)
   with
      Global =>
        (Input  => (Scheduling_Groups, Scheduling_Plan, CPU_Info.Is_BSP,
                    Tau0_Interface.State),
         In_Out => (Current_Minor_Frame_ID, Global_Current_Major_Frame_ID,
                    Global_Current_Major_Start_Cycles, MP.Barrier,
                    Scheduling_Info.State))
   is
      use type Skp.Scheduling.Major_Frame_Range;
      use type Skp.Scheduling.Minor_Frame_Range;
      use type Skp.Scheduling.Barrier_Index_Range;

      Current_Major_ID     : constant Skp.Scheduling.Major_Frame_Range
        := Global_Current_Major_Frame_ID;
      Current_Minor_ID     : constant Skp.Scheduling.Minor_Frame_Range
        := Current_Minor_Frame_ID;
      Current_Major_Length : constant Skp.Scheduling.Minor_Frame_Range
        := Scheduling_Plan (Current_Major_ID).Length;

      --  Save current major frame CPU cycles for schedule info export.
      Current_Major_Frame_Start : constant SK.Word64
        := Global_Current_Major_Start_Cycles;

      Next_Minor_ID : Skp.Scheduling.Minor_Frame_Range;
   begin
      if Current_Minor_ID < Current_Major_Length then

         --  Sync on minor frame barrier if necessary and switch to next
         --  minor frame in current major frame.

         declare
            Current_Barrier : constant Skp.Scheduling.Barrier_Index_Range
              := Scheduling_Plan
                (Current_Major_ID).Minor_Frames (Current_Minor_ID).Barrier;
         begin
            if Current_Barrier /= Skp.Scheduling.No_Barrier then
               MP.Wait_On_Minor_Frame_Barrier (Index => Current_Barrier);
            end if;
         end;

         Next_Minor_ID := Current_Minor_ID + 1;
      else

         --  Switch to first minor frame in next major frame.

         Next_Minor_ID := Skp.Scheduling.Minor_Frame_Range'First;

         MP.Wait_For_All;
         if CPU_Info.Is_BSP then
            declare

               --  Next major frame ID used to access the volatile New_Major
               --  variable. Do not move the declaration outside of this scope
               --  as it is only updated on the BSP. All other CPUs must get
               --  the value from CPU_Info.

               Next_Major_ID    : Skp.Scheduling.Major_Frame_Range;
               Next_Major_Start : SK.Word64;
            begin
               Tau0_Interface.Get_Major_Frame (ID => Next_Major_ID);

               --  Increment major frame start time by period of major frame
               --  that just ended.

               Next_Major_Start := Global_Current_Major_Start_Cycles
                 + Skp.Scheduling.Major_Frames (Current_Major_ID).Period;

               Global_Current_Major_Frame_ID     := Next_Major_ID;
               Global_Current_Major_Start_Cycles := Next_Major_Start;

               if Current_Major_ID /= Next_Major_ID then
                  MP.Set_Minor_Frame_Barrier_Config
                    (Config => Skp.Scheduling.Major_Frames
                       (Next_Major_ID).Barrier_Config);
               end if;
            end;
         end if;
         MP.Wait_For_All;
      end if;

      --  Update current minor frame globally.

      Current_Minor_Frame_ID := Next_Minor_ID;

      --  Subject switch.

      Next_Subject := Get_Current_Subject_ID;

      --  Set scheduling information of scheduling group.

      Scheduling_Info.Set_Scheduling_Info
        (ID                 => Skp.Scheduling.Get_Scheduling_Group_ID
           (Subject_ID => Next_Subject),
         TSC_Schedule_Start => Current_Major_Frame_Start +
           Scheduling_Plan
             (Current_Major_ID).Minor_Frames (Current_Minor_ID).Deadline,
         TSC_Schedule_End   => Global_Current_Major_Start_Cycles +
           Scheduling_Plan
             (Global_Current_Major_Frame_ID).Minor_Frames
             (Next_Minor_ID).Deadline);
   end Update_Scheduling_Info;

   -------------------------------------------------------------------------

   procedure Set_VMX_Exit_Timer
   is
      Now      : constant SK.Word64 := CPU.RDTSC;
      Deadline : SK.Word64;
      Cycles   : SK.Word64;
   begin

      --  Absolute deadline is given by start of major frame plus the number of
      --  CPU cycles until the end of the current minor frame relative to major
      --  frame start.

      Deadline := Global_Current_Major_Start_Cycles +
        Scheduling_Plan (Global_Current_Major_Frame_ID).Minor_Frames
        (Current_Minor_Frame_ID).Deadline;

      if Deadline > Now then
         Cycles := Deadline - Now;
      else
         Cycles := 0;
      end if;

      VMX.VMCS_Write (Field => Constants.GUEST_VMX_PREEMPT_TIMER,
                      Value => Cycles / 2 ** Skp.Scheduling.VMX_Timer_Rate);
   end Set_VMX_Exit_Timer;

   -------------------------------------------------------------------------

   --  Clear all state associated with the subject specified by ID and
   --  initialize to the values of the subject policy.
   procedure Init_Subject (ID : Skp.Global_Subject_ID_Type)
   with
      Global =>
        (Input  => (CPU_Info.APIC_ID, Interrupt_Tables.State,
                    VMX.Exit_Address),
         In_Out => (Crash_Audit.State, FPU.State, Subjects.State,
                    Subjects_Events.State, Subjects_Interrupts.State,
                    Subjects_MSR_Store.State, Timed_Events.State,
                    VMX.VMCS_State, X86_64.State))
   is
      Controls  : constant Skp.Subjects.VMX_Controls_Type
        := Skp.Subjects.Get_VMX_Controls (Subject_ID => ID);
      VMCS_Addr : constant SK.Word64
        := Skp.Subjects.Get_VMCS_Address (Subject_ID => ID);
      MSR_Count : constant SK.Word32
        := Skp.Subjects.Get_MSR_Count (Subject_ID => ID);
   begin
      FPU.Reset_State (ID => ID);
      Subjects_Events.Clear_Events (Subject => ID);
      Subjects_Interrupts.Init_Interrupts (Subject => ID);
      Timed_Events.Init_Event (Subject => ID);

      if MSR_Count > 0 then
         Subjects_MSR_Store.Clear_MSRs (Subject => ID);
      end if;

      VMX.Reset (VMCS_Address => VMCS_Addr,
                 Subject_ID   => ID);
      VMX.Load  (VMCS_Address => VMCS_Addr);
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
      VMX.VMCS_Setup_Host_Fields;
      VMX.VMCS_Setup_Guest_Fields
        (PML4_Address => Skp.Subjects.Get_PML4_Address (Subject_ID => ID),
         EPT_Pointer  => Skp.Subjects.Get_EPT_Pointer (Subject_ID => ID));
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

   procedure Init
   is
      use type Skp.CPU_Range;
   begin

      --  Setup VMCS and state of subjects running on this logical CPU.

      for I in Skp.Global_Subject_ID_Type loop
         if Skp.Subjects.Get_CPU_ID (Subject_ID => I) = CPU_Info.CPU_ID then
            Init_Subject (ID => I);
         end if;
      end loop;

      --  Load first subject and set preemption timer ticks.

      declare
         Now               : constant SK.Word64 := CPU.RDTSC;
         Current_Subject   : constant Skp.Global_Subject_ID_Type
           := Get_Current_Subject_ID;
         Current_VMCS_Addr : constant SK.Word64
           := Skp.Subjects.Get_VMCS_Address (Subject_ID => Current_Subject);
      begin
         VMX.Load (VMCS_Address => Current_VMCS_Addr);
         Scheduling_Info.Set_Scheduling_Info
           (ID                 => Skp.Scheduling.Get_Scheduling_Group_ID
              (Subject_ID => Current_Subject),
            TSC_Schedule_Start => Now,
            TSC_Schedule_End   => Now + Scheduling_Plan
              (Skp.Scheduling.Major_Frame_Range'First).Minor_Frames
                (Skp.Scheduling.Minor_Frame_Range'First).Deadline);

         if CPU_Info.Is_BSP then

            --  Set minor frame barriers config.

            MP.Set_Minor_Frame_Barrier_Config
              (Config => Skp.Scheduling.Major_Frames
                 (Skp.Scheduling.Major_Frame_Range'First).Barrier_Config);

            --  Set initial major frame start time to now.

            Global_Current_Major_Start_Cycles := Now;
         end if;
      end;
   end Init;

   -------------------------------------------------------------------------

   --  Check if the specified subject has a pending target event. If one is
   --  found, the event is consumed by performing the corresponding action.
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
      Subjects_Events.Consume_Event
        (Subject => Subject_ID,
         Found   => Found,
         Event   => Event_ID);

      if Found then
         declare
            Cur_Event : constant Skp.Events.Target_Event_Type
              := Skp.Events.Get_Target_Event (Subject_ID => Subject_ID,
                                              Event_Nr   => Event_ID);
         begin
            case Skp.Events.Get_Kind (Target_Event => Cur_Event)
            is
               when Skp.Events.No_Action        => null;
               when Skp.Events.Inject_Interrupt =>
                  SK.Subjects_Interrupts.Insert_Interrupt
                    (Subject => Subject_ID,
                     Vector  => SK.Byte (Skp.Events.Get_Vector
                       (Target_Event => Cur_Event)));
               when Skp.Events.Reset            =>
                  Init_Subject (ID => Subject_ID);
            end case;
         end;
      end if;
   end Handle_Pending_Target_Event;

   -------------------------------------------------------------------------

   --  Handle system panic action that was triggered by source event of given
   --  subject by invoking crash audit.
   procedure Handle_System_Panic (Subject : Skp.Global_Subject_ID_Type)
   with
       Global => (Input  => (CPU_Info.APIC_ID, Subjects.State),
                  In_Out => (Crash_Audit.State, X86_64.State)),
       No_Return
   is
      S : Crash_Audit_Types.Subj_Context_Type;
   begin
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

   --  Handle given source event of specified subject. If the event is of mode
   --  handover, the target subject ID is returned in Next_Subject, otherwise
   --  the parameter is set to the ID of the current subject.
   procedure Handle_Source_Event
     (Subject      :     Skp.Global_Subject_ID_Type;
      Event        :     Skp.Events.Source_Event_Type;
      Next_Subject : out Skp.Global_Subject_ID_Type)
   with
      Global =>
        (Input  => (Current_Minor_Frame_ID, Global_Current_Major_Frame_ID,
                    Scheduling_Plan, CPU_Info.APIC_ID, Subjects.State),
         In_Out => (Scheduling_Groups, Crash_Audit.State, IO_Apic.State,
                    Subjects_Events.State, X86_64.State))
   is
      use type Skp.Events.Target_Event_Range;

      Dst_CPU : Skp.CPU_Range;
   begin
      Next_Subject := Subject;

      case Event.Source_Action
      is
         when Skp.Events.No_Action       => null;
         when Skp.Events.System_Reboot   =>
            Power.Reboot (Power_Cycle => True);
         when Skp.Events.System_Poweroff =>
            Power.Shutdown;
         when Skp.Events.System_Panic =>
            Handle_System_Panic (Subject => Subject);
         when Skp.Events.Unmask_Irq      =>
            IO_Apic.Unmask_IRQ
              (RTE_Index => Skp.Interrupts.RTE_Index_Type (Event.IRQ_Number));
      end case;

      if Event.Target_Subject /= Skp.Invalid_Subject then
         if Event.Target_Event /= Skp.Events.Invalid_Target_Event then
            Subjects_Events.Set_Event_Pending
              (Subject  => Event.Target_Subject,
               Event_ID => Event.Target_Event);

            if Event.Send_IPI then
               Dst_CPU := Skp.Subjects.Get_CPU_ID
                 (Subject_ID => Event.Target_Subject);
               Apic.Send_IPI (Vector => SK.Constants.IPI_Vector,
                              CPU_ID => Dst_CPU);
            end if;
         end if;

         if Event.Handover then
            Next_Subject := Event.Target_Subject;
            Set_Current_Subject_ID (Subject_ID => Next_Subject);
         end if;
      end if;
   end Handle_Source_Event;

   -------------------------------------------------------------------------

   --  Handle hypercall with given event number.
   procedure Handle_Hypercall
     (Current_Subject    : Skp.Global_Subject_ID_Type;
      Unchecked_Event_Nr : Word64)
   with
      Global =>
        (Input  => (Current_Minor_Frame_ID, Global_Current_Major_Frame_ID,
                    Scheduling_Plan, CPU_Info.APIC_ID),
         In_Out => (Scheduling_Groups, Crash_Audit.State, IO_Apic.State,
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

      Subjects.Increment_RIP (ID => Current_Subject);

      --  If hypercall triggered a handover event, load new VMCS.

      if Current_Subject /= Next_Subject_ID then
         VMX.Load (VMCS_Address => Skp.Subjects.Get_VMCS_Address
                   (Subject_ID => Next_Subject_ID));
      end if;
   end Handle_Hypercall;

   -------------------------------------------------------------------------

   --  Handle external interrupt request with given vector.
   procedure Handle_Irq (Vector : SK.Byte)
   with
      Global => (In_Out => (IO_Apic.State, Subjects_Interrupts.State,
                            Skp.IOMMU.State, X86_64.State))
   is
      Vect_Nr : Skp.Interrupts.Remapped_Vector_Type;
      Route   : Skp.Interrupts.Vector_Route_Type;
   begin
      if Vector >= Skp.Interrupts.Remap_Offset
        and then Vector /= SK.Constants.IPI_Vector
      then
         if Vector = SK.Constants.VTd_Fault_Vector then
            VTd.Process_Fault;
         else
            Vect_Nr := Skp.Interrupts.Remapped_Vector_Type (Vector);
            Route   := Skp.Interrupts.Vector_Routing (Vect_Nr);
            if Route.Subject in Skp.Global_Subject_ID_Type then
               Subjects_Interrupts.Insert_Interrupt
                 (Subject => Route.Subject,
                  Vector  => SK.Byte (Route.Vector));
            end if;

            if Vect_Nr in Skp.Interrupts.Mask_IRQ_Type then
               IO_Apic.Mask_IRQ
                 (RTE_Index => Skp.Interrupts.RTE_Index_Type (Vector - 32));
            end if;

            pragma Debug (Route.Subject not in Skp.Global_Subject_ID_Type,
                          Dump.Print_Message
                            (Msg => "Spurious IRQ vector "
                             & Strings.Img (Vector)));
         end if;
      end if;

      pragma Debug (Vector < Skp.Interrupts.Remap_Offset,
                    Dump.Print_Message
                      (Msg => "IRQ with invalid vector "
                       & Strings.Img (Vector)));
      Apic.EOI;
   end Handle_Irq;

   -------------------------------------------------------------------------

   --  Handle trap with given number using trap table of current subject.
   procedure Handle_Trap
     (Current_Subject : Skp.Global_Subject_ID_Type;
      Trap_Nr         : SK.Word16)
   with
      Global =>
        (Input  => (Current_Minor_Frame_ID, Global_Current_Major_Frame_ID,
                    Scheduling_Plan, CPU_Info.APIC_ID, Subjects.State),
         In_Out => (Scheduling_Groups, Crash_Audit.State, IO_Apic.State,
                    Subjects_Events.State, X86_64.State))
   is
      Trap_Entry      : Skp.Events.Source_Event_Type;
      Next_Subject_ID : Skp.Global_Subject_ID_Type;
      Valid_Trap_Nr   : Boolean;

      ----------------------------------------------------------------------

      procedure Panic_Unknown_Trap
      with
         Global => (Input  => (Current_Subject, CPU_Info.APIC_ID,
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
      Valid_Trap_Nr := Trap_Nr <= SK.Word16 (Skp.Events.Trap_Range'Last);
      if not Valid_Trap_Nr then
         Panic_Unknown_Trap;
      end if;

      Trap_Entry := Skp.Events.Get_Trap
        (Subject_ID => Current_Subject,
         Trap_Nr    => Skp.Events.Trap_Range (Trap_Nr));
      Handle_Source_Event
        (Subject      => Current_Subject,
         Event        => Trap_Entry,
         Next_Subject => Next_Subject_ID);

      --  If trap triggered a handover, load new VMCS.

      if Current_Subject /= Next_Subject_ID then
         VMX.Load (VMCS_Address => Skp.Subjects.Get_VMCS_Address
                   (Subject_ID => Next_Subject_ID));
      end if;
   end Handle_Trap;

   -------------------------------------------------------------------------

   --  Minor frame ticks consumed, handle VMX preemption timer expiry.
   procedure Handle_Timer_Expiry (Current_Subject : Skp.Global_Subject_ID_Type)
   with
      Global =>
        (Input  => (Scheduling_Plan, CPU_Info.APIC_ID, CPU_Info.Is_BSP,
                    Subjects.State, Tau0_Interface.State),
         In_Out => (Current_Minor_Frame_ID, Global_Current_Major_Frame_ID,
                    Global_Current_Major_Start_Cycles, Scheduling_Groups,
                    Crash_Audit.State, IO_Apic.State, MP.Barrier,
                    Scheduling_Info.State, Subjects_Events.State,
                    Timed_Events.State, X86_64.State))
   is
      Next_Subject_ID : Skp.Global_Subject_ID_Type;
   begin
      Update_Scheduling_Info (Next_Subject => Next_Subject_ID);

      --  Check and possibly handle timed event of subject.

      declare
         Event_Subj    : constant Skp.Global_Subject_ID_Type
           := Next_Subject_ID;
         Trigger_Value : SK.Word64;
         Event_Nr      : Skp.Events.Event_Range;
         TSC_Now       : constant SK.Word64 := CPU.RDTSC;
      begin

         --  Set expired event pending.

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

      if Current_Subject /= Next_Subject_ID then

         --  New minor frame contains different subject -> Load VMCS.

         VMX.Load (VMCS_Address => Skp.Subjects.Get_VMCS_Address
                   (Subject_ID => Next_Subject_ID));
      end if;
   end Handle_Timer_Expiry;

   -------------------------------------------------------------------------

   procedure Handle_Vmx_Exit (Subject_Registers : in out SK.CPU_Registers_Type)
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
      Current_Subject := Get_Current_Subject_ID;

      VMX.VMCS_Read (Field => Constants.VMX_EXIT_REASON,
                     Value => Exit_Reason);
      Basic_Exit_Reason := SK.Word16 (Exit_Reason and 16#ffff#);

      Subjects.Save_State (ID          => Current_Subject,
                           Exit_Reason => Exit_Reason,
                           Regs        => Subject_Registers);

      FPU.Save_State (ID => Current_Subject);

      VMX.VMCS_Read (Field => Constants.VMX_EXIT_INTR_INFO,
                     Value => Exit_Interruption_Info);

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
      else
         Handle_Trap (Current_Subject => Current_Subject,
                      Trap_Nr         => Basic_Exit_Reason);
      end if;

      Current_Subject := Get_Current_Subject_ID;
      Handle_Pending_Target_Event (Subject_ID => Current_Subject);
      Inject_Interrupt (Subject_ID => Current_Subject);

      Set_VMX_Exit_Timer;
      FPU.Restore_State (ID => Current_Subject);
      Subjects.Filter_State (ID => Current_Subject);
      Subjects.Restore_State
        (ID   => Current_Subject,
         Regs => Subject_Registers);
   end Handle_Vmx_Exit;

end SK.Scheduler;
