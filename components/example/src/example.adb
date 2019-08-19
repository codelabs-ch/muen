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

with Musinfo.Instance;

with Component_Constants;

with Foo.Receiver;
with Foo.Sender;

with Debuglog.Client;

with Interrupt_Handler;
pragma Unreferenced (Interrupt_Handler);

with Example_Component.Config;

procedure Example
is
   Request_Valid : Boolean;
   Request       : Foo.Message_Type;
   Response      : Foo.Message_Type := Foo.Null_Message;
begin
   SK.Interrupt_Tables.Initialize
     (Stack_Addr => Component_Constants.Interrupt_Stack_Address);

   Debuglog.Client.Put_Line (Item => Example_Component.Config.Greeter);

   if not Musinfo.Instance.Is_Valid then
      Debuglog.Client.Put_Line
        (Item => "Error: Muen subject info (sinfo) not valid -> HLT");
      SK.CPU.Stop;
   end if;

   pragma Debug (Example_Component.Config.Print_Serial,
                 Debuglog.Client.Put_Line
                   (Item => "Serial " & SK.Strings.Img
                      (SK.Word64 (Example_Component.Config.Serial))));
   pragma Debug (Example_Component.Config.Print_Vcpu_Speed,
                 Debuglog.Client.Put_Line
                   (Item => "VCPU running with " & SK.Strings.Img
                      (Musinfo.Instance.TSC_Khz) & " Khz"));

   SK.CPU.Sti;

   loop
      Debuglog.Client.Put_Line (Item => "Waiting for event...");
      SK.CPU.Hlt;

      Foo.Receiver.Receive (Req => Request);
      Request_Valid := Foo.Is_Valid (Msg => Request);

      if Request_Valid then
         Debuglog.Client.Put_Line (Item => "Copying response...");
         Response := Request;
      else
         Debuglog.Client.Put_Line
           (Item => "Invalid request message size "
            & SK.Strings.Img (Request.Size));
         Response := Foo.Null_Message;
      end if;

      Foo.Sender.Send (Res => Response);
   end loop;
end Example;
