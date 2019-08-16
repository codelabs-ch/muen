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

with Crypt.Receiver;
with Crypt.Sender;
with Crypt.Hasher;

with Crypt.Debug;

with Handler;
pragma Unreferenced (Handler);

procedure Example
with
   Global =>
     (Input  => Crypt.Receiver.State,
      Output => Crypt.Sender.State,
      In_Out => (X86_64.State, SK.Interrupt_Tables.State))
is
   Request_Valid : Boolean;
   Request       : Crypt.Message_Type;
   Response      : Crypt.Message_Type := Crypt.Null_Message;
begin
   pragma Debug (Crypt.Debug.Put_Greeter);
   SK.Interrupt_Tables.Initialize
     (Stack_Addr => Component_Constants.Interrupt_Stack_Address);

   SK.CPU.Sti;

   loop
      SK.CPU.Hlt;
      pragma Debug (Crypt.Debug.Put_Process_Message);

      Crypt.Receiver.Receive (Req => Request);
      Request_Valid := Crypt.Is_Valid (Msg => Request);

      if Request_Valid then
         pragma Debug (Crypt.Debug.Put_Word16
                       (Message => " Size",
                        Value   => Request.Size));
         Crypt.Hasher.SHA256_Hash (Input  => Request,
                                   Output => Response);
         pragma Debug (Crypt.Debug.Put_Hash (Item => Response));
      end if;
      pragma Debug (not Request_Valid,
                    Crypt.Debug.Put_Word16
                      (Message => "Invalid request message size",
                       Value   => Request.Size));

      Crypt.Sender.Send (Res => Response);
   end loop;
end Example;
