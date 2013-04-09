with Skp;

with SK.KC;
with SK.Hypercall;

with Dumper_Kernel_Iface;

procedure Dumper
is
   package DKI renames Dumper_Kernel_Iface;

   use SK;

   VM_EXIT_TIMER_EXPIRY : constant := 52;

   --  Subject state after first launch (launched, no exit yet).
   Pristine_State : Subject_State_Type := Null_Subject_State;

   State : Subject_State_Type;
begin
   Pristine_State.Launched := True;

   for I in Skp.Subject_Id_Type loop
      State := DKI.Get_Subject_State (Id => I);
      if State /= Pristine_State
        and then State.Exit_Reason /= VM_EXIT_TIMER_EXPIRY
      then
         KC.Put_String (Item => "Subject ");
         KC.Put_Byte   (Item => Byte (I));
         KC.Put_String (Item => " EXIT (");
         KC.Put_Word16 (Item => Word16 (State.Exit_Reason));
         KC.Put_String (Item => ":");
         KC.Put_Word32 (Item => Word32 (State.Exit_Qualification));
         KC.Put_String (Item => ":");
         KC.Put_Word32 (Item => Word32 (State.Interrupt_Info));
         KC.Put_Line   (Item => ")");

         KC.Put_String ("RIP: ");
         KC.Put_Word64 (Item => State.RIP);
         KC.Put_String (" CS : ");
         KC.Put_Word16 (Item => Word16 (State.CS));
         KC.Put_String (" RFLAGS: ");
         KC.Put_Word32 (Item => Word32 (State.RFLAGS));
         KC.New_Line;
         KC.Put_String ("RSP: ");
         KC.Put_Word64 (Item => State.RSP);
         KC.Put_String (" SS : ");
         KC.Put_Word16 (Item => Word16 (State.SS));
         KC.New_Line;

         KC.Put_String (Item => "RAX: ");
         KC.Put_Word64 (Item => State.Regs.RAX);
         KC.Put_String (Item => " RBX: ");
         KC.Put_Word64 (Item => State.Regs.RBX);
         KC.Put_String (Item => " RCX: ");
         KC.Put_Word64 (Item => State.Regs.RCX);
         KC.New_Line;

         KC.Put_String (Item => "RDX: ");
         KC.Put_Word64 (Item => State.Regs.RDX);
         KC.Put_String (Item => " RSI: ");
         KC.Put_Word64 (Item => State.Regs.RSI);
         KC.Put_String (Item => " RDI: ");
         KC.Put_Word64 (Item => State.Regs.RDI);
         KC.New_Line;

         KC.Put_String (Item => "RBP: ");
         KC.Put_Word64 (Item => State.Regs.RBP);
         KC.Put_String (Item => " R08: ");
         KC.Put_Word64 (Item => State.Regs.R08);
         KC.Put_String (Item => " R09: ");
         KC.Put_Word64 (Item => State.Regs.R09);
         KC.New_Line;

         KC.Put_String (Item => "R10: ");
         KC.Put_Word64 (Item => State.Regs.R10);
         KC.Put_String (Item => " R11: ");
         KC.Put_Word64 (Item => State.Regs.R11);
         KC.Put_String (Item => " R12: ");
         KC.Put_Word64 (Item => State.Regs.R12);
         KC.New_Line;

         KC.Put_String (Item => "R13: ");
         KC.Put_Word64 (Item => State.Regs.R13);
         KC.Put_String (Item => " R14: ");
         KC.Put_Word64 (Item => State.Regs.R14);
         KC.Put_String (Item => " R15: ");
         KC.Put_Word64 (Item => State.Regs.R15);
         KC.New_Line;

         KC.Put_String (Item => "CR0: ");
         KC.Put_Word64 (Item => State.CR0);
         KC.Put_String (Item => " CR3: ");
         KC.Put_Word64 (Item => State.CR3);
         KC.Put_String (Item => " CR4: ");
         KC.Put_Word64 (Item => State.CR4);
         KC.New_Line;

         Hypercall.Swap_Relaunch (Subject_Id => Byte (I));
      end if;
   end loop;

end Dumper;
