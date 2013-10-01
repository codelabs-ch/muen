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

with SK.CPU;
with SK.Hypercall;

with Skp;

with Subject.Text_IO;

with Interrupts;
with Handler;

procedure Sm
is
   use type SK.Word64;

   Id    : Skp.Subject_Id_Type;
   State : SK.Subject_State_Type;
   for State'Address use System'To_Address (16#1e0000#);
begin
   Subject.Text_IO.Init;
   Subject.Text_IO.Put_Line ("SM subject running");
   Interrupts.Initialize;

   SK.CPU.Sti;
   SK.CPU.Hlt;

   loop
      Id := Handler.Current_Subject;

      if State.Exit_Reason = 30 then
         Subject.Text_IO.Put_String (Item => "Subject ");
         Subject.Text_IO.Put_Byte   (Item => SK.Byte (Id));
         Subject.Text_IO.Put_String (Item => ": I/O instruction on port ");
         Subject.Text_IO.Put_Word16
           (Item => SK.Word16 (State.Exit_Qualification / 2 ** 16));
         Subject.Text_IO.New_Line;
         State.RIP := State.RIP + State.Instruction_Len;
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
