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

with SK.CPU;
with SK.Hypercall;
with SK.Constants;

with Skp;

with Subject.Console;
with Subject.Text_IO;

with Interrupts;
with Interrupt_Handler;
with Subject_Info;
with Exit_Handlers.CPUID;
with Exit_Handlers.IO_Instruction;
with Exit_Handlers.RDMSR;
with Exit_Handlers.WRMSR;
with Exit_Handlers.CR_Access;
with Exit_Handlers.RDTSC;

procedure Sm
is
   use type SK.Word64;
   use Subject_Info;

   Id            : Skp.Subject_Id_Type;
   Dump_And_Halt : Boolean := False;
begin
   Subject.Console.Enable_Notification;
   Subject.Text_IO.Init;
   Subject.Text_IO.Put_Line ("SM subject running");
   Interrupts.Initialize;

   SK.CPU.Sti;
   SK.CPU.Hlt;

   loop
      Id := Interrupt_Handler.Current_Subject;

      if State.Exit_Reason = SK.Constants.EXIT_REASON_CPUID then
         Exit_Handlers.CPUID.Process (Halt => Dump_And_Halt);
      elsif State.Exit_Reason = SK.Constants.EXIT_REASON_INVLPG
        or else State.Exit_Reason = SK.Constants.EXIT_REASON_DR_ACCESS
      then

         --  Ignore INVLPG and MOV DR for now.
         null;

      elsif State.Exit_Reason = SK.Constants.EXIT_REASON_RDTSC then
         Exit_Handlers.RDTSC.Process (Halt => Dump_And_Halt);
      elsif State.Exit_Reason = SK.Constants.EXIT_REASON_IO_INSTRUCTION then
         Exit_Handlers.IO_Instruction.Process (Halt => Dump_And_Halt);
      elsif State.Exit_Reason = SK.Constants.EXIT_REASON_RDMSR then
         Exit_Handlers.RDMSR.Process (Halt => Dump_And_Halt);
      elsif State.Exit_Reason = SK.Constants.EXIT_REASON_WRMSR then
         Exit_Handlers.WRMSR.Process (Halt => Dump_And_Halt);
      elsif State.Exit_Reason = SK.Constants.EXIT_REASON_CR_ACCESS then
         Exit_Handlers.CR_Access.Process (Halt => Dump_And_Halt);
      else
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

         Dump_And_Halt := True;
      end if;

      if not Dump_And_Halt then
         State.RIP := State.RIP + State.Instruction_Len;
         SK.Hypercall.Trigger_Event (Number => SK.Byte (Id));
      else
         if (State.IA32_EFER and 16#400#) = 0 then
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
            Subject.Text_IO.Put_String (Item => "Shadow CR0: ");
            Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.SHADOW_CR0));
            Subject.Text_IO.New_Line;

            Subject.Text_IO.Put_String (Item => "IA32_EFER: ");
            Subject.Text_IO.Put_Word64 (Item => State.IA32_EFER);
            Subject.Text_IO.New_Line;
         else
            Subject.Text_IO.Put_String ("RIP: ");
            Subject.Text_IO.Put_Word64 (Item => State.RIP);
            Subject.Text_IO.Put_String (" CS : ");
            Subject.Text_IO.Put_Word16 (Item => SK.Word16 (State.CS));
            Subject.Text_IO.Put_String (" RFLAGS: ");
            Subject.Text_IO.Put_Word64 (Item => State.RFLAGS);
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
            Subject.Text_IO.New_Line;
            Subject.Text_IO.Put_String (Item => "CR3: ");
            Subject.Text_IO.Put_Word64 (Item => State.CR3);
            Subject.Text_IO.Put_String (Item => " CR4: ");
            Subject.Text_IO.Put_Word64 (Item => State.CR4);
            Subject.Text_IO.New_Line;
            Subject.Text_IO.New_Line;

            Subject.Text_IO.Put_String (Item => "Shadow CR0: ");
            Subject.Text_IO.Put_Word64 (Item => State.SHADOW_CR0);
            Subject.Text_IO.New_Line;

            Subject.Text_IO.Put_String (Item => "IA32_EFER : ");
            Subject.Text_IO.Put_Word64 (Item => State.IA32_EFER);
            Subject.Text_IO.New_Line;
         end if;

         Subject.Text_IO.New_Line;
         Subject.Text_IO.Put_Line (Item => "Halting execution");

         loop
            SK.CPU.Hlt;
         end loop;
      end if;
   end loop;
end Sm;
