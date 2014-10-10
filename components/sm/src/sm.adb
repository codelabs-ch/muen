--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Subject.Console;
with Subject.Text_IO;

with Interrupts;
with Interrupt_Handler;
with Subject_Info;
with Exit_Handlers.CPUID;
with Exit_Handlers.EPT_Violation;
with Exit_Handlers.IO_Instruction;
with Exit_Handlers.RDMSR;
with Exit_Handlers.WRMSR;
with Exit_Handlers.CR_Access;
with Exit_Handlers.RDTSC;

with Debug_Ops;

pragma Unreferenced (Interrupt_Handler);

procedure Sm
is
   use type SK.Word64;
   use Subject_Info;

   Resume_Event  : constant := 4;
   Dump_And_Halt : Boolean  := False;
begin
   pragma Debug (Subject.Console.Enable_Notification);
   pragma Debug (Subject.Text_IO.Init);
   pragma Debug (Subject.Text_IO.Put_Line ("SM subject running"));
   Interrupts.Initialize;

   SK.CPU.Sti;
   SK.CPU.Hlt;

   loop
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
      elsif State.Exit_Reason = SK.Constants.EXIT_REASON_EPT_VIOLATION then
         Exit_Handlers.EPT_Violation.Process (Halt => Dump_And_Halt);
      else
         pragma Debug (Subject.Text_IO.Put_Line
                       (Item => "Unhandled trap for associated subject"));

         Dump_And_Halt := True;
      end if;

      if not Dump_And_Halt then
         State.RIP := State.RIP + State.Instruction_Len;
         SK.Hypercall.Trigger_Event (Number => Resume_Event);
      else
         pragma Debug (Debug_Ops.Dump_State);

         loop
            SK.CPU.Hlt;
         end loop;
      end if;
   end loop;
end Sm;
