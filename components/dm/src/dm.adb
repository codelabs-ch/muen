--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Mudm;
with Musinfo.Instance;

with Dev_Mngr.Receiver;
with Dev_Mngr.Sender;
with Dev_Mngr.Pciconf;
with Log;

with Dm_Component.Channels;

procedure Dm
is
   use Dev_Mngr;

   Request, Response : Mudm.Emul_Message_Type;
begin
   Log.Init (Epoch => 1);
   Log.Put_Line (Item => "DM subject running");

   if not Musinfo.Instance.Is_Valid then
      Log.Put_Line (Item => "Error: Sinfo data not valid");
      SK.CPU.Stop;
   end if;

   loop
      Receiver.Receive (Req => Request);
      Response := Request;
      Pciconf.Emulate (SID    => Request.SID,
                       Op     => Request.Op,
                       Offset => Request.Offset,
                       Value  => Request.Value,
                       Result => Response.Result);
      Sender.Send (Res => Response);
      SK.Hypercall.Trigger_Event
        (Number => Dm_Component.Channels.Response_Event);
   end loop;
end Dm;
