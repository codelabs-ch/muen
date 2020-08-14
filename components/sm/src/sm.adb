--
--  Copyright (C) 2013, 2014, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with X86_64;

with SK.CPU;
with SK.Hypercall;
with SK.Constants;
with SK.Interrupt_Tables;

with Debuglog.Client;

with Mucontrol.Command.Instance;

with Mutime.Info;
with Musinfo.Instance;
with Mudm.Client;

with Component_Constants;

with Sm_Component.Events;

with Time;
with Types;
with Interrupt_Handler;
with Subject_Info;
with Exit_Handlers.CPUID;
with Exit_Handlers.EPT_Violation;
with Exit_Handlers.IO_Instruction;
with Exit_Handlers.Invalid_Guest_State;
with Exit_Handlers.RDMSR;
with Exit_Handlers.WRMSR;
with Exit_Handlers.CR_Access;
with Exit_Handlers.RDTSC;
with Devices.RTC;
with Devices.UART8250;

with Debug_Ops;

pragma Unreferenced (Interrupt_Handler);

procedure Sm
with
   Global => (Input  => (Musinfo.Instance.State, Mutime.Info.State,
                         Musinfo.Instance.Scheduling_Info,
                         Mucontrol.Command.Instance.State),
              In_Out => (Devices.RTC.State, Mudm.Client.State,
                         Devices.UART8250.State, Exit_Handlers.RDTSC.State,
                         Mutime.Info.Valid, Subject_Info.State,
                         SK.Interrupt_Tables.State, X86_64.State),
              Output => Debuglog.Client.State)
is
   use type SK.Word16;
   use type SK.Word32;
   use type SK.Word64;
   use type Types.Subject_Action_Type;
   use Subject_Info;

   Cur_Epoch : constant Interfaces.Unsigned_64
        := Mucontrol.Command.Instance.Get_Epoch;

   Action : Types.Subject_Action_Type;
   Exit_Reason, Instruction_Len : SK.Word32;
   RIP : SK.Word64;
begin
   Debuglog.Client.Init (Epoch => Cur_Epoch);
   pragma Debug (Debug_Ops.Put_Line (Item => "SM subject running"));
   SK.Interrupt_Tables.Initialize
     (Stack_Addr => Component_Constants.Interrupt_Stack_Address);
   Time.Initialize;

   if not Musinfo.Instance.Is_Valid then
      pragma Debug (Debug_Ops.Put_Line (Item => "Sinfo not valid"));
      SK.CPU.Stop;
   end if;

   loop
      Exit_Reason := State.Exit_Reason;

      if Exit_Reason = SK.Constants.EXIT_REASON_CPUID then
         Exit_Handlers.CPUID.Process (Action => Action);
      elsif Exit_Reason = SK.Constants.EXIT_REASON_INVLPG
        or else Exit_Reason = SK.Constants.EXIT_REASON_DR_ACCESS
        or else Exit_Reason = SK.Constants.EXIT_REASON_WBINVD
        or else Exit_Reason = SK.Constants.EXIT_REASON_VMCALL
      then

         --  Ignore VMCALL, WBINVD, INVLPG and MOV DR for now.

         Action := Types.Subject_Continue;

      elsif Exit_Reason = SK.Constants.EXIT_REASON_RDTSC then
         Exit_Handlers.RDTSC.Process (Action => Action);
      elsif Exit_Reason = SK.Constants.EXIT_REASON_IO_INSTRUCTION then
         Exit_Handlers.IO_Instruction.Process (Action => Action);
      elsif Exit_Reason = SK.Constants.EXIT_REASON_RDMSR then
         Exit_Handlers.RDMSR.Process (Action => Action);
      elsif Exit_Reason = SK.Constants.EXIT_REASON_WRMSR then
         Exit_Handlers.WRMSR.Process (Action => Action);
      elsif Exit_Reason = SK.Constants.EXIT_REASON_CR_ACCESS then
         Exit_Handlers.CR_Access.Process (Action => Action);
      elsif Exit_Reason = SK.Constants.EXIT_REASON_EPT_VIOLATION then
         Exit_Handlers.EPT_Violation.Process (Action => Action);
      elsif SK.Word16'Mod (Exit_Reason)
        = SK.Constants.EXIT_REASON_ENTRY_FAIL_GSTATE
      then
         Exit_Handlers.Invalid_Guest_State.Process (Action => Action);
      else
         pragma Debug (Debug_Ops.Put_Line
                       (Item => "Unhandled trap for associated subject"));

         Action := Types.Subject_Halt;
      end if;

      case Action
      is
         when Types.Subject_Start    =>
            SK.Hypercall.Trigger_Event
              (Number => Sm_Component.Events.Resume_Subject_ID);
         when Types.Subject_Continue =>
            RIP             := State.RIP;
            Instruction_Len := State.Instruction_Len;
            State.RIP       := RIP + SK.Word64 (Instruction_Len);
            SK.Hypercall.Trigger_Event
              (Number => Sm_Component.Events.Resume_Subject_ID);
         when Types.Subject_Halt     =>
            pragma Debug (Debug_Ops.Dump_State);
            SK.CPU.Stop;
      end case;
   end loop;
end Sm;
