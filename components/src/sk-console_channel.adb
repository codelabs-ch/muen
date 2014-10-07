--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with VT_Channels;
with SK.Hypercall;

package body SK.Console_Channel
is

   use VT_Channels;

   Channel : VT_Channel.Channel_Type
     with
       Address => System'To_Address (Channel_Address);

   Notify : Boolean := False;

   -------------------------------------------------------------------------

   procedure Enable_Notification
   is
   begin
      Notify := True;
   end Enable_Notification;

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      VT_Channel_Wtr.Initialize (Channel => Channel,
                                 Epoch   => 1);
   end Init;

   -------------------------------------------------------------------------

   procedure New_Line
   is
   begin
      VT_Channel_Wtr.Write (Channel => Channel,
                            Element => ASCII.LF);

      if Notify then
         Hypercall.Trigger_Event (Number => 1);
      end if;
   end New_Line;

   -------------------------------------------------------------------------

   procedure Put_Char (Item : Character)
   is
   begin
      VT_Channel_Wtr.Write (Channel => Channel,
                            Element => Item);
   end Put_Char;

end SK.Console_Channel;
