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

with SK.Hypercall;

with Example_Component.Channels;

package body Crypt.Sender
with
   Refined_State => (State => Response)
is

   Response : Crypt.Message_Type
     with
       Volatile,
       Async_Readers,
       Address => System'To_Address
         (Example_Component.Channels.Response_Address);

   -------------------------------------------------------------------------

   procedure Send (Res : Crypt.Message_Type)
   with
      Refined_Global  => (Output => Response, In_Out => X86_64.State),
      Refined_Depends => (Response => Res, X86_64.State =>+ null)
   is
   begin
      Response := Res;

      SK.Hypercall.Trigger_Event
        (Number => Example_Component.Channels.Response_Event);
   end Send;

begin
   Response := Crypt.Null_Message;
end Crypt.Sender;
