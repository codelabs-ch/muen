with System.Machine_Code;

with SK.CPU;
with SK.Console;
with SK.Console_VGA;
with SK.Hypercall;

with Skp.Subjects;

with Dump;

with Dumper_Kernel_Iface;

procedure Dumper
is
   package DKI renames Dumper_Kernel_Iface;

   subtype Width_Type  is Natural range 1 .. 80;
   subtype Height_Type is Natural range 1 .. 25;

   package VGA is new SK.Console_VGA
     (Width_Type   => Width_Type,
      Height_Type  => Height_Type,
      Base_Address => System'To_Address (16#000b_8000#));

   package Text_IO is new SK.Console
     (Initialize      => VGA.Init,
      Output_New_Line => VGA.New_Line,
      Output_Char     => VGA.Put_Char);

   State : SK.Subject_State_Type;
   Id    : Skp.Subject_Id_Type;
begin
   Dump.Initialize;
   Text_IO.Init;
   Text_IO.Put_Line ("Dumper subject running");

   System.Machine_Code.Asm
     (Template => "sti",
      Volatile => True);
   SK.CPU.Hlt;

   loop
      Id    := Dump.Current_Subject;
      State := DKI.Get_Subject_State (Id => Id);

      Text_IO.New_Line;
      Text_IO.Put_String (Item => "Subject ");
      Text_IO.Put_Byte   (Item => SK.Byte (Id));
      Text_IO.Put_String (Item => " EXIT (");
      Text_IO.Put_Word16 (Item => SK.Word16 (State.Exit_Reason));
      Text_IO.Put_String (Item => ":");
      Text_IO.Put_Word32 (Item => SK.Word32 (State.Exit_Qualification));
      Text_IO.Put_String (Item => ":");
      Text_IO.Put_Word32 (Item => SK.Word32 (State.Interrupt_Info));
      Text_IO.Put_Line   (Item => ")");

      Text_IO.Put_String ("RIP: ");
      Text_IO.Put_Word64 (Item => State.RIP);
      Text_IO.Put_String (" CS : ");
      Text_IO.Put_Word16 (Item => SK.Word16 (State.CS));
      Text_IO.Put_String (" RFLAGS: ");
      Text_IO.Put_Word32 (Item => SK.Word32 (State.RFLAGS));
      Text_IO.New_Line;
      Text_IO.Put_String ("RSP: ");
      Text_IO.Put_Word64 (Item => State.RSP);
      Text_IO.Put_String (" SS : ");
      Text_IO.Put_Word16 (Item => SK.Word16 (State.SS));
      Text_IO.New_Line;

      Text_IO.Put_String (Item => "RAX: ");
      Text_IO.Put_Word64 (Item => State.Regs.RAX);
      Text_IO.Put_String (Item => " RBX: ");
      Text_IO.Put_Word64 (Item => State.Regs.RBX);
      Text_IO.Put_String (Item => " RCX: ");
      Text_IO.Put_Word64 (Item => State.Regs.RCX);
      Text_IO.New_Line;

      Text_IO.Put_String (Item => "RDX: ");
      Text_IO.Put_Word64 (Item => State.Regs.RDX);
      Text_IO.Put_String (Item => " RSI: ");
      Text_IO.Put_Word64 (Item => State.Regs.RSI);
      Text_IO.Put_String (Item => " RDI: ");
      Text_IO.Put_Word64 (Item => State.Regs.RDI);
      Text_IO.New_Line;

      Text_IO.Put_String (Item => "RBP: ");
      Text_IO.Put_Word64 (Item => State.Regs.RBP);
      Text_IO.Put_String (Item => " R08: ");
      Text_IO.Put_Word64 (Item => State.Regs.R08);
      Text_IO.Put_String (Item => " R09: ");
      Text_IO.Put_Word64 (Item => State.Regs.R09);
      Text_IO.New_Line;

      Text_IO.Put_String (Item => "R10: ");
      Text_IO.Put_Word64 (Item => State.Regs.R10);
      Text_IO.Put_String (Item => " R11: ");
      Text_IO.Put_Word64 (Item => State.Regs.R11);
      Text_IO.Put_String (Item => " R12: ");
      Text_IO.Put_Word64 (Item => State.Regs.R12);
      Text_IO.New_Line;

      Text_IO.Put_String (Item => "R13: ");
      Text_IO.Put_Word64 (Item => State.Regs.R13);
      Text_IO.Put_String (Item => " R14: ");
      Text_IO.Put_Word64 (Item => State.Regs.R14);
      Text_IO.Put_String (Item => " R15: ");
      Text_IO.Put_Word64 (Item => State.Regs.R15);
      Text_IO.New_Line;

      Text_IO.Put_String (Item => "CR0: ");
      Text_IO.Put_Word64 (Item => State.CR0);
      Text_IO.Put_String (Item => " CR3: ");
      Text_IO.Put_Word64 (Item => State.CR3);
      Text_IO.Put_String (Item => " CR4: ");
      Text_IO.Put_Word64 (Item => State.CR4);
      Text_IO.New_Line;

      State.Regs := SK.Null_CPU_Regs;
      State.RIP  := Skp.Subjects.Get_Entry_Point   (Subject_Id => Id);
      State.RSP  := Skp.Subjects.Get_Stack_Address (Subject_Id => Id);
      DKI.Set_Subject_State (Id    => Id,
                             State => State);
      SK.Hypercall.Trigger_Event (Number => SK.Byte (Id));
   end loop;
end Dumper;
