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

with X86_64;

with SK.CPU;
with SK.Interrupt_Tables;

with Component_Constants;

with Foo.Receiver;
with Foo.Sender;
with Foo.Hasher;

with Foo.Debug;

with Interrupt_Handler;
pragma Unreferenced (Interrupt_Handler);

procedure Example
with
   Global =>
     (Input  => Foo.Receiver.State,
      Output => Foo.Sender.State,
      In_Out => (X86_64.State, SK.Interrupt_Tables.State))
is
   Request_Valid : Boolean;
   Request       : Foo.Message_Type;
   Response      : Foo.Message_Type := Foo.Null_Message;
begin
   pragma Debug (Foo.Debug.Put_Greeter);
   SK.Interrupt_Tables.Initialize
     (Stack_Addr => Component_Constants.Interrupt_Stack_Address);

   SK.CPU.Sti;

   loop
      SK.CPU.Hlt;
      pragma Debug (Foo.Debug.Put_Process_Message);

      Foo.Receiver.Receive (Req => Request);
      Request_Valid := Foo.Is_Valid (Msg => Request);

      if Request_Valid then
         pragma Debug (Foo.Debug.Put_Word16
                       (Message => " Size",
                        Value   => Request.Size));
         Foo.Hasher.SHA256_Hash (Input  => Request,
                                   Output => Response);
         pragma Debug (Foo.Debug.Put_Hash (Item => Response));
      end if;
      pragma Debug (not Request_Valid,
                    Foo.Debug.Put_Word16
                      (Message => "Invalid request message size",
                       Value   => Request.Size));

      Foo.Sender.Send (Res => Response);
   end loop;
end Example;
