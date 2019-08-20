--
--  Copyright (C) 2013-2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with SK.Interrupt_Tables;
with SK.Strings;
with SK.Hypercall;

with Musinfo.Instance;

with Component_Constants;

with Foo.Receiver;
with Foo.Sender;

with Debuglog.Client;

with Log;
with Subject_Info;
with Timed_Events;
with Interrupt_Handler;
pragma Unreferenced (Interrupt_Handler);

with Example_Component.Config;

procedure Example
is
   Request_Valid : Boolean;
   Request       : Foo.Message_Type;
   Response      : Foo.Message_Type := Foo.Null_Message;
begin

   --  Initialize interrupt handling.

   SK.Interrupt_Tables.Initialize
     (Stack_Addr => Component_Constants.Interrupt_Stack_Address);

   --  Check sinfo validity.

   if not Musinfo.Instance.Is_Valid then
      Debuglog.Client.Put_Line
        (Item => "Error: Muen subject info (sinfo) not valid -> HLT");
      SK.CPU.Stop;
   end if;

   --  Say hello via dbglog.

   Log.Put_Line (Item => Example_Component.Config.Greeter);

   pragma Debug (Example_Component.Config.Print_Serial,
                 Log.Put_Line (Item => "Serial " & SK.Strings.Img
                               (SK.Word64 (Example_Component.Config.Serial))));
   pragma Debug (Example_Component.Config.Print_Vcpu_Speed,
                 Log.Put_Line (Item => "VCPU running with " & SK.Strings.Img
                               (Musinfo.Instance.TSC_Khz) & " Khz"));

   --  Enable interrupts.

   SK.CPU.Sti;

   --  Trigger a self-event to wakeup from hlt.

   declare
      use type SK.Word64;

      Minor_Start : constant SK.Word64 := Musinfo.Instance.TSC_Schedule_Start;
      Minor_End   : constant SK.Word64 := Musinfo.Instance.TSC_Schedule_End;
      Trigger     : constant SK.Word64 := Minor_End + 1000;
   begin
      Log.Put_Line
        (Item => "Current minor frame ticks " &
           SK.Strings.Img (Minor_Start) & " .. " & SK.Strings.Img (Minor_End));
      Log.Put_Line (Item => "Triggering self-event");
      Timed_Events.Timed_Evt.Event_Nr          := 3;
      Timed_Events.Timed_Evt.TSC_Trigger_Value := Trigger;
   end;

   Log.Put_Line (Item => "Halting");
   SK.CPU.Hlt;

   declare
      Minor_Start : constant SK.Word64 := Musinfo.Instance.TSC_Schedule_Start;
      Minor_End   : constant SK.Word64 := Musinfo.Instance.TSC_Schedule_End;
   begin
      Log.Put_Line
        (Item => "Wakeup in frame "
         & SK.Strings.Img (Minor_Start) & " .. " & SK.Strings.Img (Minor_End));
   end;

   declare
      RIP : constant SK.Word64 := Subject_Info.State.RIP;
   begin

      --  Print some register values of monitored subject.

      Log.Put_Line (Item => "Monitored subject RIP " & SK.Strings.Img (RIP));
   end;

   --  Give up CPU.

   Log.Put_Line (Item => "Yielding CPU");
   SK.Hypercall.Trigger_Event (Number => 2);

   --  Act as a service: process events from associated subject.

   loop
      Foo.Receiver.Receive (Req => Request);
      Request_Valid := Foo.Is_Valid (Msg => Request);

      if Request_Valid then
         Log.Put_Line (Item => "Copying response");
         Response := Request;
      else
         Log.Put_Line (Item => "Invalid request message size " & SK.Strings.Img
                       (Request.Size));
         Response := Foo.Null_Message;
      end if;

      --  Send response and switch back to requester.

      Foo.Sender.Send (Res => Response);
   end loop;
end Example;
