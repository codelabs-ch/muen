with System;

with Skp;

with SK.Console;
with SK.Console_VGA;
with SK.Hypercall;

with Dumper_Kernel_Iface;

procedure Dumper
is
   package DKI renames Dumper_Kernel_Iface;

   use SK;

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

   VM_EXIT_TIMER_EXPIRY : constant := 52;

   --  Subject state after first launch (launched, no exit yet).
   Pristine_State : Subject_State_Type := Null_Subject_State;

   State : Subject_State_Type;
begin
   Text_IO.Init;
   Pristine_State.Launched := True;

   for I in Skp.Subject_Id_Type loop
      State := DKI.Get_Subject_State (Id => I);
      if State /= Pristine_State
        and then State.Exit_Reason /= VM_EXIT_TIMER_EXPIRY
      then
         Text_IO.Put_String (Item => "Subject ");
         Text_IO.Put_Byte   (Item => Byte (I));
         Text_IO.Put_String (Item => " EXIT (");
         Text_IO.Put_Word16 (Item => Word16 (State.Exit_Reason));
         Text_IO.Put_String (Item => ":");
         Text_IO.Put_Word32 (Item => Word32 (State.Exit_Qualification));
         Text_IO.Put_String (Item => ":");
         Text_IO.Put_Word32 (Item => Word32 (State.Interrupt_Info));
         Text_IO.Put_Line   (Item => ")");

         Text_IO.Put_String ("RIP: ");
         Text_IO.Put_Word64 (Item => State.RIP);
         Text_IO.Put_String (" CS : ");
         Text_IO.Put_Word16 (Item => Word16 (State.CS));
         Text_IO.Put_String (" RFLAGS: ");
         Text_IO.Put_Word32 (Item => Word32 (State.RFLAGS));
         Text_IO.New_Line;
         Text_IO.Put_String ("RSP: ");
         Text_IO.Put_Word64 (Item => State.RSP);
         Text_IO.Put_String (" SS : ");
         Text_IO.Put_Word16 (Item => Word16 (State.SS));
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

         Hypercall.Swap_Relaunch (Subject_Id => Byte (I));
      end if;
   end loop;

end Dumper;
