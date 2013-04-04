with System;

with SK.Console_VGA;
with SK.Console;

with Dumper_Kernel_Iface;
with Dump;

procedure Dumper
is

   package DKI renames Dumper_Kernel_Iface;

   --  Dumper console width and height.
   subtype Width_Type  is Natural range 1 .. 80;
   subtype Height_Type is Natural range 1 .. 17;

   package VGA is new SK.Console_VGA
     (Width_Type   => Width_Type,
      Height_Type  => Height_Type,
      Base_Address => System'To_Address (16#000b_8500#));

   package Text_IO is new SK.Console
     (Initialize      => VGA.Init,
      Output_New_Line => VGA.New_Line,
      Output_Char     => VGA.Put_Char);

   State : SK.Subject_State_Type;
begin
   Text_IO.Init;
   Text_IO.Put_Line (Item => "Dumper");
   Text_IO.New_Line;

   for I in DKI.Descriptors_Range loop
      State := DKI.Get_Subject_State (Id => I);
      if Dump.Is_Valid (Info => State.Interrupt_Info) then
         Text_IO.Put_String (Item => "Subject ");
         Text_IO.Put_Byte (Item => SK.Byte (I));
         Text_IO.New_Line;
         Text_IO.Put_String (Item => "Exception ");
         Text_IO.Put_Byte
           (Item => Dump.Get_Vector
              (Intr_Info => State.Interrupt_Info));
         Text_IO.New_Line;

         Text_IO.Put_String ("RIP: ");
         Text_IO.Put_Word64 (Item => State.RIP);
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
      end if;
   end loop;

   loop
      null;
   end loop;
end Dumper;
