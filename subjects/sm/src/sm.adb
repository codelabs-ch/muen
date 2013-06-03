with SK.CPU;
with SK.Hypercall;

with Skp;

with Subject.Text_IO;

with Interrupts;
with Handler;
with Knl_States_Interface;

procedure Sm
is
   use type SK.Word64;

   package KSI renames Knl_States_Interface;

   State : SK.Subject_State_Type;
   Id    : Skp.Subject_Id_Type;
begin
   Subject.Text_IO.Init;
   Subject.Text_IO.Put_Line ("SM subject running");
   Interrupts.Initialize;

   SK.CPU.Sti;
   SK.CPU.Hlt;

   loop
      Id    := Handler.Current_Subject;
      State := KSI.Get_Subject_State (Id => Id);

      if State.Exit_Reason = 30 then
         Subject.Text_IO.Put_String (Item => "Subject ");
         Subject.Text_IO.Put_Byte   (Item => SK.Byte (Id));
         Subject.Text_IO.Put_String (Item => ": I/O instruction on port ");
         Subject.Text_IO.Put_Word16
           (Item => SK.Word16 (State.Exit_Qualification / 2 ** 16));
         Subject.Text_IO.New_Line;
         State.RIP := State.RIP + State.Instruction_Len;
         KSI.Set_Subject_State (Id    => Id,
                                State => State);
         SK.Hypercall.Trigger_Event (Number => SK.Byte (Id));
      else
         Subject.Text_IO.New_Line;
         Subject.Text_IO.Put_String (Item => "Unhandled trap for subject ");
         Subject.Text_IO.Put_Byte   (Item => SK.Byte (Id));
         Subject.Text_IO.Put_String (Item => " EXIT (");
         Subject.Text_IO.Put_Word16 (Item => SK.Word16 (State.Exit_Reason));
         Subject.Text_IO.Put_String (Item => ":");
         Subject.Text_IO.Put_Word32
           (Item => SK.Word32 (State.Exit_Qualification));
         Subject.Text_IO.Put_String (Item => ":");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Interrupt_Info));
         Subject.Text_IO.Put_Line   (Item => ")");

         Subject.Text_IO.Put_String ("EIP: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.RIP));
         Subject.Text_IO.Put_String (" CS : ");
         Subject.Text_IO.Put_Word16 (Item => SK.Word16 (State.CS));
         Subject.Text_IO.Put_String (" EFLAGS: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.RFLAGS));
         Subject.Text_IO.New_Line;
         Subject.Text_IO.Put_String ("ESP: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.RSP));
         Subject.Text_IO.Put_String (" SS : ");
         Subject.Text_IO.Put_Word16 (Item => SK.Word16 (State.SS));
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "EAX: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RAX));
         Subject.Text_IO.Put_String (Item => " EBX: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RBX));
         Subject.Text_IO.Put_String (Item => " ECX: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RCX));
         Subject.Text_IO.Put_String (Item => " EDX: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RDX));
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "ESI: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RSI));
         Subject.Text_IO.Put_String (Item => " EDI: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RDI));
         Subject.Text_IO.Put_String (Item => " EBP: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RBP));
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "CR0: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.CR0));
         Subject.Text_IO.Put_String (Item => " CR2: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.CR2));
         Subject.Text_IO.Put_String (Item => " CR3: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.CR3));
         Subject.Text_IO.Put_String (Item => " CR4: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.CR4));
         Subject.Text_IO.New_Line;

         Subject.Text_IO.New_Line;
         Subject.Text_IO.Put_Line (Item => "Halting execution");

         loop
            SK.CPU.Hlt;
         end loop;
      end if;
   end loop;
end Sm;
