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

with System;

with Dm_Component.Channels;

package body Dev_Mngr.Receiver
with
   Refined_State => (State => Request)
is

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "This global variable is effectively read-only.");
   Request : Mudm.Emul_Message_Type
   with
      Volatile,
      Async_Writers,
      Address => System'To_Address (Dm_Component.Channels.Request_Address);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

   -------------------------------------------------------------------------

   procedure Receive (Req : out Mudm.Emul_Message_Type)
   with
      Refined_Global  => (Input => Request),
      Refined_Depends => (Req => Request)
   is
   begin
      Req := Request;
   end Receive;

end Dev_Mngr.Receiver;
