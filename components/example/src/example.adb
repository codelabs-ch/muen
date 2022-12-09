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

with Interfaces;

with SK.CPU;
with SK.Interrupt_Tables;
with SK.Strings;
with SK.Hypercall;

with Musinfo.Instance;

with Component_Constants;

with Exceptions;

with Foo.Receiver;
with Foo.Sender;

with Muenblock_Example;

with Debuglog.Client;

with Log;
with Subject_Info;
with Timed_Events;
with Interrupt_Handler;

with Example_Component.Config;
with Example_Component.Events;

with Mucontrol.Command.Instance;
with Mucontrol.Status.Instance;

procedure Example
is
   Cur_Epoch     : constant Interfaces.Unsigned_64
     := Mucontrol.Command.Instance.Get_Epoch;

   Request_Valid : Boolean;
   Request       : Foo.Message_Type;
   Response      : Foo.Message_Type;
begin

   --  Initialize debug log.

   Debuglog.Client.Init (Epoch => Cur_Epoch);

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

   --D @Lst Configbegin
   pragma Debug (Example_Component.Config.Print_Serial,
                 Log.Put_Line (Item => "Serial " & SK.Strings.Img
                               (SK.Word64 (Example_Component.Config.Serial))));
   pragma Debug (Example_Component.Config.Print_Vcpu_Speed,
                 Log.Put_Line (Item => "VCPU running with " & SK.Strings.Img
                               (Musinfo.Instance.TSC_Khz) & " Khz"));
   --D @Lst Configend

   Log.Put_Line (Item => "Current Status: " & SK.Strings.Img
                 (Item => SK.Word64 (Mucontrol.Status.Instance.Get)));
   Log.Put_Line (Item => "Current Epoch : " & SK.Strings.Img
                 (Item => Cur_Epoch));

   --  Enable interrupts.

   SK.CPU.Sti;

   --  Muenblock Showcase
   Muenblock_Example.Show;

   --  Trigger a self-event to wakeup from hlt.

   declare
      use type SK.Word64;

      --D @Lst Sinfobegin
      Minor_Start : constant SK.Word64 := Musinfo.Instance.TSC_Schedule_Start;
      Minor_End   : constant SK.Word64 := Musinfo.Instance.TSC_Schedule_End;
      --D @Lst Sinfoend
      Trigger     : constant SK.Word64 := Minor_End + 1000;
   begin
      Log.Put_Line
        (Item => "Current minor frame ticks " &
           SK.Strings.Img (Minor_Start) & " .. " & SK.Strings.Img (Minor_End));
      Log.Put_Line (Item => "Triggering self-event");
      Timed_Events.Timed_Evt.Event_Nr := Example_Component.Events.Timer_ID;
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

   --  Trigger a self-event to wakeup from sleep with interrupts disabled. When
   --  a sleeping subject becomes active execution will resume after the
   --  instruction that triggered the sleep event. A subject can become active
   --  for the following reasons:
   --  - External interrupt was marked pending
   --  - Target Event was marked pending
   --  - Timed Event expired

   SK.CPU.Cli;

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
      Timed_Events.Timed_Evt.Event_Nr := Example_Component.Events.Timer_ID;
      Timed_Events.Timed_Evt.TSC_Trigger_Value := Trigger;
   end;

   Log.Put_Line (Item => "Sleeping");
   SK.Hypercall.Trigger_Event (Number => Example_Component.Events.Sleep_ID);

   declare
      Minor_Start : constant SK.Word64 := Musinfo.Instance.TSC_Schedule_Start;
      Minor_End   : constant SK.Word64 := Musinfo.Instance.TSC_Schedule_End;
   begin
      Log.Put_Line
        (Item => "Wakeup in frame "
         & SK.Strings.Img (Minor_Start) & " .. " & SK.Strings.Img (Minor_End));
   end;

   --  Yield remaining time of current minor frame.

   declare
      use type SK.Word64;

      Minor_End       : constant SK.Word64 := Musinfo.Instance.TSC_Schedule_End;
      New_Minor_Start : SK.Word64;
   begin
      SK.Hypercall.Trigger_Event (Number => Example_Component.Events.Yield_ID);
      New_Minor_Start := Musinfo.Instance.TSC_Schedule_Start;

      if Minor_End <= New_Minor_Start then
         Log.Put_Line (Item => "Successfully yielded minor frame");
      else
         Log.Put_Line (Item => "Error yielding CPU with last end "
                       & SK.Strings.Img (Minor_End) & " and new start "
                       & SK.Strings.Img (New_Minor_Start));
      end if;
   end;

   --  Re-enable interrupts so they can be processed, e.g. when a new request is
   --  placed into the Foo interface an interrupt will be injected.

   SK.CPU.Sti;

   declare
      RIP : constant SK.Word64 := Subject_Info.State.RIP;
   begin

      --  Print some register values of monitored subject.

      Log.Put_Line (Item => "Monitored subject RIP " & SK.Strings.Img (RIP));
   end;

   --  Trigger breakpoint exception and verify that corresponding handler/ISR
   --  was executed including correct continuation of control flow.

   Exceptions.BP_Triggered := False;
   Exceptions.Trigger_Breakpoint;
   declare
      Triggered : constant Boolean := Exceptions.BP_Triggered;
   begin
      if Triggered then
         Log.Put_Line (Item => "Breakpoint exception processed successfully");
      else
         Log.Put_Line (Item => "Error triggering #BP exception!");
      end if;
   end;

   --  Act as a service: process events from associated subject.

   declare
      Request_Pending : Boolean;
   begin
      loop
         Request_Pending := Interrupt_Handler.Foo_Request_Pending;

         if Request_Pending then
            Interrupt_Handler.Foo_Request_Pending := False;

            Foo.Receiver.Receive (Req => Request);
            Request_Valid := Foo.Is_Valid (Msg => Request);

            if Request_Valid then
               Log.Put_Line (Item => "Copying request with size "
                             & SK.Strings.Img (Request.Size)
                             & " to response");
               Response := Request;
            else
               Log.Put_Line (Item => "Invalid request message size "
                             & SK.Strings.Img (Request.Size));
               Response := Foo.Null_Message;
            end if;

            --  Send response and notify requester.

            Foo.Sender.Send (Res => Response);
         end if;

         --  Sleep until next notification via event.

         SK.Hypercall.Trigger_Event
           (Number => Example_Component.Events.Sleep_ID);
         Log.Put_Line
           (Item => "Wakeup after sleep, checking whether work is pending");
      end loop;
   end;
end Example;
