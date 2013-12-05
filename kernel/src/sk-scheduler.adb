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

with Skp.Interrupts;

with SK.VMX;
with SK.Constants;
with SK.KC;
with SK.CPU;
with SK.CPU_Global;
with SK.Subjects;
with SK.Apic;
with SK.MP;
with SK.Events;
with SK.Dump;

package body SK.Scheduler
--# own
--#    State is in New_Major, Current_Major;
is

   --  IRQ constants.
   Timer_Vector : constant := 48;
   IPI_Vector   : constant := 254;

   Launched_Subject_State : constant SK.Subject_State_Type
     := SK.Subject_State_Type'
       (Launched           => True,
        Regs               => SK.Null_CPU_Regs,
        Exit_Reason        => 0,
        Exit_Qualification => 0,
        Guest_Phys_Addr    => 0,
        Interrupt_Info     => 0,
        Instruction_Len    => 0,
        RIP                => 0,
        CS                 => 0,
        RSP                => 0,
        SS                 => 0,
        CR0                => 0,
        SHADOW_CR0         => 0,
        CR2                => 0,
        CR3                => 0,
        CR4                => 0,
        RFLAGS             => 0,
        Kernel_GS_BASE     => 0,
        IA32_EFER          => 0,
        XSAVE_Area         => SK.XSAVE_Area_Type'(others => 0));

   Tau0_Kernel_Iface_Address : SK.Word64;
   pragma Import (C, Tau0_Kernel_Iface_Address, "tau0kernel_iface_ptr");

   New_Major : Skp.Scheduling.Major_Frame_Range;
   for New_Major'Address use System'To_Address (Tau0_Kernel_Iface_Address);
   --# assert New_Major'Always_Valid;

   --  Current major.
   Current_Major : Skp.Scheduling.Major_Frame_Range
     := Skp.Scheduling.Major_Frame_Range'First;

   -------------------------------------------------------------------------

   --  Inject pending event into subject identified by ID.
   procedure Inject_Event (Subject_Id : Skp.Subject_Id_Type)
   --# global
   --#    in     Subjects.State;
   --#    in out Events.State;
   --#    in out X86_64.State;
   --# derives
   --#    Events.State, X86_64.State from
   --#       *,
   --#       Subject_Id,
   --#       Subjects.State,
   --#       Events.State,
   --#       X86_64.State;
   is
      RFLAGS        : SK.Word64;
      Intr_State    : SK.Word64;
      Event         : SK.Byte;
      Event_Present : Boolean;
   begin
      RFLAGS := Subjects.Get_RFLAGS (Id => Subject_Id);

      --  Check guest interruptibility state (see Intel SDM Vol. 3C, chapter
      --  24.4.2).

      VMX.VMCS_Read (Field => Constants.GUEST_INTERRUPTIBILITY,
                 Value => Intr_State);

      if Intr_State = 0
        and then SK.Bit_Test
          (Value => RFLAGS,
           Pos   => Constants.RFLAGS_IF_FLAG)
      then
         Events.Consume_Event (Subject => Subject_Id,
                               Found   => Event_Present,
                               Event   => Event);

         if Event_Present then
            VMX.VMCS_Write
              (Field => Constants.VM_ENTRY_INTERRUPT_INFO,
               Value => Constants.VM_INTERRUPT_INFO_VALID + SK.Word64 (Event));
         end if;
      end if;

      if Events.Has_Pending_Events (Subject => Subject_Id) then
         VMX.VMCS_Set_Interrupt_Window (Value => True);
      end if;
   end Inject_Event;

   -------------------------------------------------------------------------

   --  Read VMCS fields and store them in the given subject state.
   procedure Store_Subject_Info (State : in out SK.Subject_State_Type)
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from * &
   --#    State        from *, X86_64.State;
   is
   begin
      VMX.VMCS_Read (Field => Constants.VMX_EXIT_QUALIFICATION,
                     Value => State.Exit_Qualification);
      VMX.VMCS_Read (Field => Constants.VMX_EXIT_INTR_INFO,
                     Value => State.Interrupt_Info);
      VMX.VMCS_Read (Field => Constants.VMX_EXIT_INSTRUCTION_LEN,
                     Value => State.Instruction_Len);

      VMX.VMCS_Read (Field => Constants.GUEST_PHYSICAL_ADDRESS,
                     Value => State.Guest_Phys_Addr);

      VMX.VMCS_Read (Field => Constants.GUEST_RIP,
                     Value => State.RIP);
      VMX.VMCS_Read (Field => Constants.GUEST_SEL_CS,
                     Value => State.CS);
      VMX.VMCS_Read (Field => Constants.GUEST_RSP,
                     Value => State.RSP);
      VMX.VMCS_Read (Field => Constants.GUEST_SEL_SS,
                     Value => State.SS);
      VMX.VMCS_Read (Field => Constants.GUEST_CR0,
                     Value => State.CR0);
      VMX.VMCS_Read (Field => Constants.CR0_READ_SHADOW,
                     Value => State.SHADOW_CR0);
      State.CR2 := CPU.Get_CR2;
      VMX.VMCS_Read (Field => Constants.GUEST_CR3,
                     Value => State.CR3);
      VMX.VMCS_Read (Field => Constants.GUEST_CR4,
                     Value => State.CR4);
      VMX.VMCS_Read (Field => Constants.GUEST_RFLAGS,
                     Value => State.RFLAGS);
      VMX.VMCS_Read (Field => Constants.GUEST_IA32_EFER,
                     Value => State.IA32_EFER);
      CPU.XSAVE (Target => State.XSAVE_Area);
   end Store_Subject_Info;

   -------------------------------------------------------------------------

   --  Perform subject handover from the old to the new subject.
   procedure Subject_Handover
     (Old_Id   : Skp.Subject_Id_Type;
      New_Id   : Skp.Subject_Id_Type;
      New_VMCS : SK.Word64)
   --# global
   --#    in out CPU_Global.State;
   --#    in out X86_64.State;
   --# derives
   --#    CPU_Global.State from *, Old_Id, New_Id &
   --#    X86_64.State     from *, New_VMCS;
   --# pre
   --#    Old_Id /= New_Id;
   is
      Remaining_Ticks : SK.Word64;
   begin
      CPU_Global.Swap_Subject
        (Old_Id => Old_Id,
         New_Id => New_Id);

      VMX.VMCS_Read (Field => Constants.GUEST_VMX_PREEMPT_TIMER,
                     Value => Remaining_Ticks);
      VMX.Load (VMCS_Address => New_VMCS);
      VMX.VMCS_Write (Field => Constants.GUEST_VMX_PREEMPT_TIMER,
                      Value => Remaining_Ticks);
   end Subject_Handover;

   -------------------------------------------------------------------------

   --  Update scheduling information. If the end of the current major frame is
   --  reached, the minor frame index is reset and the major frame is switched
   --  to the one set by Tau0. Otherwise the minor frame index is incremented
   --  by 1.
   procedure Update_Scheduling_Info
   --# global
   --#    in     New_Major;
   --#    in out X86_64.State;
   --#    in out Current_Major;
   --#    in out CPU_Global.State;
   --#    in out MP.Barrier;
   --#    in out Events.State;
   --# derives
   --#    MP.Barrier from
   --#       *,
   --#       Current_Major,
   --#       CPU_Global.State &
   --#    Current_Major, CPU_Global.State, Events.State, X86_64.State from
   --#       *,
   --#       Current_Major,
   --#       CPU_Global.State,
   --#       New_Major;
   is
      Minor_Frame : CPU_Global.Active_Minor_Frame_Type;
      Plan_Frame  : Skp.Scheduling.Minor_Frame_Type;
   begin
      Minor_Frame := CPU_Global.Get_Current_Minor_Frame;

      if Minor_Frame.Minor_Id < CPU_Global.Get_Major_Length
        (Major_Id => Current_Major)
      then

         --  Switch to next minor frame in current major frame.

         Minor_Frame.Minor_Id := Minor_Frame.Minor_Id + 1;
      else

         --  Switch to first minor frame in next major frame.

         Minor_Frame.Minor_Id := Skp.Scheduling.Minor_Frame_Range'First;

         MP.Wait_For_All;
         --# accept Flow, 22, "CPU ID differs per logical CPU";
         if CPU_Global.Is_BSP then
         --# end accept;
            Current_Major := New_Major;
         end if;
         MP.Wait_For_All;
      end if;

      Plan_Frame := CPU_Global.Get_Minor_Frame
        (Major_Id => Current_Major,
         Minor_Id => Minor_Frame.Minor_Id);

      if Plan_Frame.Subject_Id /= Minor_Frame.Subject_Id then

         --  New minor frame contains different subject -> Load VMCS.

         VMX.Load (VMCS_Address => Skp.Subjects.Get_VMCS_Address
                   (Subject_Id => Plan_Frame.Subject_Id));
      end if;

      Minor_Frame.Subject_Id := Plan_Frame.Subject_Id;
      CPU_Global.Set_Current_Minor (Frame => Minor_Frame);

      if Skp.Subjects.Get_Profile
        (Subject_Id => Minor_Frame.Subject_Id) = Skp.Subjects.Vm
      then
         Events.Insert_Event (Subject => Minor_Frame.Subject_Id,
                              Event   => Timer_Vector);
      end if;

      --  Update preemption timer ticks in subject VMCS.

      VMX.VMCS_Write (Field => Constants.GUEST_VMX_PREEMPT_TIMER,
                      Value => SK.Word64 (Plan_Frame.Ticks));
   end Update_Scheduling_Info;

   -------------------------------------------------------------------------

   procedure Init
   --# global
   --#    in     Current_Major;
   --#    in     Interrupts.State;
   --#    in     GDT.GDT_Pointer;
   --#    in     VMX.State;
   --#    in out Subjects.State;
   --#    in out CPU_Global.State;
   --#    in out X86_64.State;
   --# derives
   --#    Subjects.State   from * &
   --#    CPU_Global.State from *, Current_Major    &
   --#    X86_64.State from
   --#       *,
   --#       Current_Major,
   --#       Interrupts.State,
   --#       GDT.GDT_Pointer,
   --#       VMX.State,
   --#       CPU_Global.State;
   is
      Plan_Frame        : Skp.Scheduling.Minor_Frame_Type;
      Initial_VMCS_Addr : SK.Word64 := 0;
      Controls          : Skp.Subjects.VMX_Controls_Type;
      VMCS_Addr         : SK.Word64;
   begin
      CPU_Global.Set_Scheduling_Plan
        (Data => Skp.Scheduling.Scheduling_Plans (CPU_Global.CPU_ID));

      --  Set initial active minor frame.

      Plan_Frame := CPU_Global.Get_Minor_Frame
        (Major_Id => Current_Major,
         Minor_Id => Skp.Scheduling.Minor_Frame_Range'First);
      CPU_Global.Set_Current_Minor
        (Frame => CPU_Global.Active_Minor_Frame_Type'
           (Minor_Id   => Skp.Scheduling.Minor_Frame_Range'First,
            Subject_Id => Plan_Frame.Subject_Id));

      --  Setup VMCS and state of subjects running on this logical CPU.

      for I in Skp.Subject_Id_Type loop
         if Skp.Subjects.Get_CPU_Id (Subject_Id => I) = CPU_Global.CPU_ID then

            --  VMCS

            VMCS_Addr := Skp.Subjects.Get_VMCS_Address (Subject_Id => I);
            Controls  := Skp.Subjects.Get_VMX_Controls (Subject_Id => I);

            VMX.Clear (VMCS_Address => VMCS_Addr);
            VMX.Load  (VMCS_Address => VMCS_Addr);
            VMX.VMCS_Setup_Control_Fields
              (IO_Bitmap_Address  => Skp.Subjects.Get_IO_Bitmap_Address
                 (Subject_Id => I),
               MSR_Bitmap_Address => Skp.Subjects.Get_MSR_Bitmap_Address
                 (Subject_Id => I),
               Ctls_Exec_Pin      => Controls.Exec_Pin,
               Ctls_Exec_Proc     => Controls.Exec_Proc,
               Ctls_Exec_Proc2    => Controls.Exec_Proc2,
               Ctls_Exit          => Controls.Exit_Ctrls,
               Ctls_Entry         => Controls.Entry_Ctrls,
               CR0_Mask           => Skp.Subjects.Get_CR0_Mask
                 (Subject_Id => I),
               CR4_Mask           => Skp.Subjects.Get_CR4_Mask
                 (Subject_Id => I),
               Exception_Bitmap   => Skp.Subjects.Get_Exception_Bitmap
                 (Subject_Id => I));
            VMX.VMCS_Setup_Host_Fields;
            VMX.VMCS_Setup_Guest_Fields
              (PML4_Address => Skp.Subjects.Get_PML4_Address (Subject_Id => I),
               EPT_Pointer  => Skp.Subjects.Get_EPT_Pointer (Subject_Id => I),
               CR0_Value    => Skp.Subjects.Get_CR0 (Subject_Id => I),
               CR4_Value    => Skp.Subjects.Get_CR4 (Subject_Id => I),
               CS_Access    => Skp.Subjects.Get_CS_Access (Subject_Id => I));

            if Plan_Frame.Subject_Id = I then
               Initial_VMCS_Addr := VMCS_Addr;
            end if;

            --  State

            Subjects.Set_RIP
              (Id    => I,
               Value => Skp.Subjects.Get_Entry_Point (Subject_Id => I));
            Subjects.Set_RSP
              (Id    => I,
               Value => Skp.Subjects.Get_Stack_Address (Subject_Id => I));
            Subjects.Set_CR0
              (Id    => I,
               Value => Skp.Subjects.Get_CR0 (Subject_Id => I));
         end if;
      end loop;

      --  Load first subject and set preemption timer ticks.

      VMX.Load (VMCS_Address => Initial_VMCS_Addr);
      VMX.VMCS_Write (Field => Constants.GUEST_VMX_PREEMPT_TIMER,
                      Value => SK.Word64 (Plan_Frame.Ticks));
   end Init;

   -------------------------------------------------------------------------

   --  Handle hypercall with given event number.
   procedure Handle_Hypercall
     (Current_Subject : Skp.Subject_Id_Type;
      Event_Nr        : SK.Word64)
   --# global
   --#    in out CPU_Global.State;
   --#    in out Events.State;
   --#    in out Subjects.State;
   --#    in out X86_64.State;
   --# derives
   --#    Events.State from *, Current_Subject, Event_Nr &
   --#    CPU_Global.State, X86_64.State from
   --#       *,
   --#       Current_Subject,
   --#       Event_Nr &
   --#    Subjects.State from *, Current_Subject;
   is
      Event       : Skp.Subjects.Event_Entry_Type;
      Dst_CPU     : Skp.CPU_Range;
      Valid_Event : Boolean;
      RIP         : SK.Word64;
   begin
      Valid_Event := Event_Nr <= SK.Word64 (Skp.Subjects.Event_Range'Last);

      if Valid_Event then
         Event := Skp.Subjects.Get_Event
           (Subject_Id => Current_Subject,
            Event_Nr   => Skp.Subjects.Event_Range (Event_Nr));

         if Event.Dst_Subject /= Skp.Invalid_Subject then
            if Event.Dst_Vector /= Skp.Invalid_Vector then
               Events.Insert_Event (Subject => Event.Dst_Subject,
                                    Event   => SK.Byte (Event.Dst_Vector));

               if Event.Send_IPI then
                  Dst_CPU := Skp.Subjects.Get_CPU_Id
                    (Subject_Id => Event.Dst_Subject);
                  Apic.Send_IPI (Vector  => IPI_Vector,
                                 Apic_Id => SK.Byte (Dst_CPU));
               end if;
            end if;

            if Event.Handover then

               --# accept Warning, 444, "Guaranteed by validated policy";
               --# assume Current_Subject /= Event.Dst_Subject;
               --# end accept;

               Subject_Handover
                 (Old_Id   => Current_Subject,
                  New_Id   => Event.Dst_Subject,
                  New_VMCS => Skp.Subjects.Get_VMCS_Address
                    (Subject_Id => Event.Dst_Subject));
            end if;
         end if;
      end if;

      pragma Debug (not Valid_Event or Event = Skp.Subjects.Null_Event,
                    KC.Put_String (Item => "Ignoring spurious event "));
      pragma Debug (not Valid_Event or Event = Skp.Subjects.Null_Event,
                    KC.Put_Byte   (Item => SK.Byte (Event_Nr)));
      pragma Debug (not Valid_Event or Event = Skp.Subjects.Null_Event,
                    KC.Put_String (Item => " from subject "));
      pragma Debug (not Valid_Event or Event = Skp.Subjects.Null_Event,
                    KC.Put_Byte   (Item => SK.Byte (Current_Subject)));
      pragma Debug (not Valid_Event or Event = Skp.Subjects.Null_Event,
                    KC.New_Line);

      RIP := Subjects.Get_RIP (Id => Current_Subject);
      RIP := RIP + Subjects.Get_Instruction_Length (Id => Current_Subject);
      Subjects.Set_RIP (Id    => Current_Subject,
                        Value => RIP);
   end Handle_Hypercall;

   -------------------------------------------------------------------------

   --  Handle external interrupt request with given vector.
   procedure Handle_Irq (Vector : SK.Byte)
   --# global
   --#    in out Events.State;
   --#    in out X86_64.State;
   --# derives
   --#    Events.State from *, Vector &
   --#    X86_64.State from *;
   is
   begin
      if Vector in Skp.Interrupts.Remapped_Vector_Type then
         if Skp.Interrupts.Vector_Routing (Vector) in Skp.Subject_Id_Type then
            Events.Insert_Event
              (Subject => Skp.Interrupts.Vector_Routing (Vector),
               Event   => Vector);
         end if;

         pragma Debug
           (Skp.Interrupts.Vector_Routing (Vector) not in Skp.Subject_Id_Type
            and then Vector /= IPI_Vector,
            KC.Put_String (Item => "Spurious IRQ vector "));
         pragma Debug
           (Skp.Interrupts.Vector_Routing (Vector) not in Skp.Subject_Id_Type
            and then Vector /= IPI_Vector,
            KC.Put_Byte (Item => Vector));
         pragma Debug
           (Skp.Interrupts.Vector_Routing (Vector) not in Skp.Subject_Id_Type
            and then Vector /= IPI_Vector,
            KC.New_Line);
      end if;

      pragma Debug (Vector not in Skp.Interrupts.Remapped_Vector_Type,
                    KC.Put_String (Item => "IRQ with invalid vector "));
      pragma Debug (Vector not in Skp.Interrupts.Remapped_Vector_Type,
                    KC.Put_Byte (Item => Vector));
      pragma Debug (Vector not in Skp.Interrupts.Remapped_Vector_Type,
                    KC.New_Line);

      Apic.EOI;
   end Handle_Irq;

   -------------------------------------------------------------------------

   --  Handle trap with given number using trap table of current subject.
   procedure Handle_Trap
     (Current_Subject : Skp.Subject_Id_Type;
      Trap_Nr         : SK.Word64)
   --# global
   --#    in out CPU_Global.State;
   --#    in out Events.State;
   --#    in out X86_64.State;
   --# derives
   --#    CPU_Global.State, Events.State, X86_64.State from
   --#       *,
   --#       Current_Subject,
   --#       Trap_Nr;
   is
      Trap_Entry : Skp.Subjects.Trap_Entry_Type;
   begin
      if Trap_Nr <= SK.Word64 (Skp.Subjects.Trap_Range'Last) then
         Trap_Entry := Skp.Subjects.Get_Trap
           (Subject_Id => Current_Subject,
            Trap_Nr    => Skp.Subjects.Trap_Range (Trap_Nr));

         if Trap_Entry.Dst_Subject = Skp.Invalid_Subject then
            pragma Debug (KC.Put_Line (Item => ">>> No handler for trap <<<"));
            pragma Debug (Dump.Print_Subject (Subject_Id => Current_Subject,
                                              Dump_State => True));
            CPU.Panic;
         else
            if Trap_Entry.Dst_Vector < Skp.Invalid_Vector then
               Events.Insert_Event
                 (Subject => Trap_Entry.Dst_Subject,
                  Event   => SK.Byte (Trap_Entry.Dst_Vector));
            end if;

            --  Handover to trap handler subject.

            --# accept Warning, 444, "Guaranteed by validated policy";
            --# assume Current_Subject /= Trap_Entry.Dst_Subject;
            --# end accept;

            Subject_Handover
              (Old_Id   => Current_Subject,
               New_Id   => Trap_Entry.Dst_Subject,
               New_VMCS => Skp.Subjects.Get_VMCS_Address
                 (Subject_Id => Trap_Entry.Dst_Subject));
         end if;
      else
         pragma Debug (KC.Put_String (Item => ">>> Unknown trap "));
         pragma Debug (KC.Put_Word16 (Item => Word16 (Trap_Nr)));
         pragma Debug (KC.Put_Line (Item => " <<<"));
         pragma Debug (Dump.Print_Subject (Subject_Id => Current_Subject,
                                           Dump_State => False));
         CPU.Panic;
      end if;
   end Handle_Trap;

   -------------------------------------------------------------------------

   procedure Handle_Vmx_Exit (Subject_Registers : in out SK.CPU_Registers_Type)
   --# global
   --#    in     New_Major;
   --#    in out CPU_Global.State;
   --#    in out Current_Major;
   --#    in out MP.Barrier;
   --#    in out Subjects.State;
   --#    in out Events.State;
   --#    in out X86_64.State;
   --# derives
   --#    Current_Major, CPU_Global.State from
   --#       *,
   --#       Current_Major,
   --#       New_Major,
   --#       Subject_Registers,
   --#       CPU_Global.State,
   --#       X86_64.State &
   --#    Subject_Registers, Events.State from
   --#       *,
   --#       Current_Major,
   --#       New_Major,
   --#       Subject_Registers,
   --#       CPU_Global.State,
   --#       Subjects.State,
   --#       X86_64.State &
   --#    X86_64.State from
   --#       *,
   --#       Current_Major,
   --#       New_Major,
   --#       Subject_Registers,
   --#       CPU_Global.State,
   --#       Subjects.State,
   --#       Events.State &
   --#    MP.Barrier, Subjects.State from
   --#       *,
   --#       Current_Major,
   --#       Subject_Registers,
   --#       CPU_Global.State,
   --#       X86_64.State;
   is
      Current_Subject : Skp.Subject_Id_Type;
      Current_Minor   : CPU_Global.Active_Minor_Frame_Type;
      State           : SK.Subject_State_Type := Launched_Subject_State;
   begin
      Current_Minor   := CPU_Global.Get_Current_Minor_Frame;
      Current_Subject := CPU_Global.Get_Minor_Frame
        (Major_Id => Current_Major,
         Minor_Id => Current_Minor.Minor_Id).Subject_Id;

      VMX.VMCS_Read (Field => Constants.VMX_EXIT_REASON,
                     Value => State.Exit_Reason);

      if SK.Bit_Test (Value => State.Exit_Reason,
                      Pos   => Constants.VM_EXIT_ENTRY_FAILURE)
      then
         pragma Debug (KC.Put_String (Item => "Subject "));
         pragma Debug (KC.Put_Byte   (Item =>  Byte (Current_Subject)));
         pragma Debug (KC.Put_String (Item => " VM-entry failure ("));
         pragma Debug (KC.Put_Word16 (Item => Word16 (State.Exit_Reason)));
         pragma Debug (KC.Put_String (Item => ":"));
         pragma Debug (KC.Put_Word32
                       (Item => Word32 (State.Exit_Qualification)));
         pragma Debug (KC.Put_Line   (Item => ")"));
         CPU.Panic;
      end if;

      State.Regs := Subject_Registers;
      Store_Subject_Info (State => State);
      Subjects.Set_RIP (Id    => Current_Subject,
                        Value => State.RIP);

      if State.Exit_Reason = Constants.EXIT_REASON_EXTERNAL_INT then
         Handle_Irq (Vector => SK.Byte'Mod (State.Interrupt_Info));
      elsif State.Exit_Reason = Constants.EXIT_REASON_VMCALL then
         Handle_Hypercall (Current_Subject => Current_Subject,
                           Event_Nr        => Subject_Registers.RAX);
      elsif State.Exit_Reason = Constants.EXIT_REASON_TIMER_EXPIRY then

         --  Minor frame ticks consumed, update scheduling information.

         Update_Scheduling_Info;
      elsif State.Exit_Reason = Constants.EXIT_REASON_INTERRUPT_WINDOW then

         --  Resume subject to inject pending event.

         VMX.VMCS_Set_Interrupt_Window (Value => False);
      else
         Handle_Trap (Current_Subject => Current_Subject,
                      Trap_Nr         => State.Exit_Reason);
      end if;

      Subjects.Set_State (Id            => Current_Subject,
                          Subject_State => State);

      Inject_Event
        (Subject_Id => CPU_Global.Get_Current_Minor_Frame.Subject_Id);

      VMX.Restore_State
        (Subject_Id => CPU_Global.Get_Current_Minor_Frame.Subject_Id,
         Regs       => Subject_Registers);
   end Handle_Vmx_Exit;

end SK.Scheduler;
