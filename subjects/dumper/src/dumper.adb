with SK.CPU;
with SK.Hypercall;

with Skp.Subjects;

with Subject.Text_IO;
with Interrupts;
with Handler;

with Knl_States_Interface;

procedure Dumper
is
   package KSI renames Knl_States_Interface;

   State : SK.Subject_State_Type;
   Id    : Skp.Subject_Id_Type;
begin
   Interrupts.Initialize;
   Subject.Text_IO.Init;
   Subject.Text_IO.Put_Line ("Dumper subject running");

   SK.CPU.Sti;
   SK.CPU.Hlt;

   loop
      Id    := Handler.Current_Subject;
      State := KSI.Get_Subject_State (Id => Id);

      Subject.Text_IO.New_Line;
      Subject.Text_IO.Put_String (Item => "Subject ");
      Subject.Text_IO.Put_Byte   (Item => SK.Byte (Id));
      Subject.Text_IO.Put_String (Item => " EXIT (");
      Subject.Text_IO.Put_Word16 (Item => SK.Word16 (State.Exit_Reason));
      Subject.Text_IO.Put_String (Item => ":");
      Subject.Text_IO.Put_Word32
        (Item => SK.Word32 (State.Exit_Qualification));
      Subject.Text_IO.Put_String (Item => ":");
      Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Interrupt_Info));
      Subject.Text_IO.Put_Line   (Item => ")");

      Subject.Text_IO.Put_String ("RIP: ");
      Subject.Text_IO.Put_Word64 (Item => State.RIP);
      Subject.Text_IO.Put_String (" CS : ");
      Subject.Text_IO.Put_Word16 (Item => SK.Word16 (State.CS));
      Subject.Text_IO.Put_String (" RFLAGS: ");
      Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.RFLAGS));
      Subject.Text_IO.New_Line;
      Subject.Text_IO.Put_String ("RSP: ");
      Subject.Text_IO.Put_Word64 (Item => State.RSP);
      Subject.Text_IO.Put_String (" SS : ");
      Subject.Text_IO.Put_Word16 (Item => SK.Word16 (State.SS));
      Subject.Text_IO.New_Line;

      Subject.Text_IO.Put_String (Item => "RAX: ");
      Subject.Text_IO.Put_Word64 (Item => State.Regs.RAX);
      Subject.Text_IO.Put_String (Item => " RBX: ");
      Subject.Text_IO.Put_Word64 (Item => State.Regs.RBX);
      Subject.Text_IO.Put_String (Item => " RCX: ");
      Subject.Text_IO.Put_Word64 (Item => State.Regs.RCX);
      Subject.Text_IO.New_Line;

      Subject.Text_IO.Put_String (Item => "RDX: ");
      Subject.Text_IO.Put_Word64 (Item => State.Regs.RDX);
      Subject.Text_IO.Put_String (Item => " RSI: ");
      Subject.Text_IO.Put_Word64 (Item => State.Regs.RSI);
      Subject.Text_IO.Put_String (Item => " RDI: ");
      Subject.Text_IO.Put_Word64 (Item => State.Regs.RDI);
      Subject.Text_IO.New_Line;

      Subject.Text_IO.Put_String (Item => "RBP: ");
      Subject.Text_IO.Put_Word64 (Item => State.Regs.RBP);
      Subject.Text_IO.Put_String (Item => " R08: ");
      Subject.Text_IO.Put_Word64 (Item => State.Regs.R08);
      Subject.Text_IO.Put_String (Item => " R09: ");
      Subject.Text_IO.Put_Word64 (Item => State.Regs.R09);
      Subject.Text_IO.New_Line;

      Subject.Text_IO.Put_String (Item => "R10: ");
      Subject.Text_IO.Put_Word64 (Item => State.Regs.R10);
      Subject.Text_IO.Put_String (Item => " R11: ");
      Subject.Text_IO.Put_Word64 (Item => State.Regs.R11);
      Subject.Text_IO.Put_String (Item => " R12: ");
      Subject.Text_IO.Put_Word64 (Item => State.Regs.R12);
      Subject.Text_IO.New_Line;

      Subject.Text_IO.Put_String (Item => "R13: ");
      Subject.Text_IO.Put_Word64 (Item => State.Regs.R13);
      Subject.Text_IO.Put_String (Item => " R14: ");
      Subject.Text_IO.Put_Word64 (Item => State.Regs.R14);
      Subject.Text_IO.Put_String (Item => " R15: ");
      Subject.Text_IO.Put_Word64 (Item => State.Regs.R15);
      Subject.Text_IO.New_Line;

      Subject.Text_IO.Put_String (Item => "CR0: ");
      Subject.Text_IO.Put_Word64 (Item => State.CR0);
      Subject.Text_IO.Put_String (Item => " CR2: ");
      Subject.Text_IO.Put_Word64 (Item => State.CR2);
      Subject.Text_IO.Put_String (Item => " CR3: ");
      Subject.Text_IO.Put_Word64 (Item => State.CR3);
      Subject.Text_IO.New_Line;

      Subject.Text_IO.Put_String (Item => "CR4: ");
      Subject.Text_IO.Put_Word64 (Item => State.CR4);
      Subject.Text_IO.New_Line;

      State.Regs := SK.Null_CPU_Regs;
      State.RIP  := Skp.Subjects.Get_Entry_Point   (Subject_Id => Id);
      State.RSP  := Skp.Subjects.Get_Stack_Address (Subject_Id => Id);
      KSI.Set_Subject_State (Id    => Id,
                             State => State);
      SK.Hypercall.Trigger_Event (Number => SK.Byte (Id));
   end loop;
end Dumper;
