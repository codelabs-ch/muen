--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Input.Event_Channel.Writer_Instance;

with Ps2_Drv_Component.Channels;

package body PS2.Output
with
   Refined_State => (State => Event_Channel)
is

   Event_Channel : Input.Event_Channel.Channel_Type
   with
     Async_Readers,
     Address => System'To_Address
       (Ps2_Drv_Component.Channels.Input_Events_Address);

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      Input.Event_Channel.Writer_Instance.Initialize
        (Channel => Event_Channel,
         Epoch   => 1);
   end  Init;

   -------------------------------------------------------------------------

   procedure Write (Event : Input.Input_Event_Type)
   is
   begin
      Input.Event_Channel.Writer_Instance.Write
        (Channel => Event_Channel,
         Element => Event);
      SK.Hypercall.Trigger_Event
        (Number => Ps2_Drv_Component.Channels.Input_Events_Event);
   end Write;

end PS2.Output;
