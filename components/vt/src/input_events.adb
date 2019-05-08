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

with Vt_Component.Channels;

with Input.Event_Channel.Reader;
with Log;
with Mux.Terminals;

package body Input_Events
is

   Event_Channel : Input.Event_Channel.Channel_Type
   with
      Address => System'To_Address
        (Vt_Component.Channels.Input_Events_Address);

   Reader : Input.Event_Channel.Reader.Reader_Type;

   -------------------------------------------------------------------------

   procedure Process
   with
      SPARK_Mode => Off
   is
      use type Input.Event_Channel.Reader.Result_Type;

      Ev  : Input.Input_Event_Type;
      Res : Input.Event_Channel.Reader.Result_Type;
   begin
      loop
         Input.Event_Channel.Reader.Read
           (Channel => Event_Channel,
            Reader  => Reader,
            Element => Ev,
            Result  => Res);
         case Res is
            when Input.Event_Channel.Reader.Incompatible_Interface =>
               Log.Text_IO.Put_Line
                 (Item => "Input event channel: Incompatible interface"
                  & " detected");
            when Input.Event_Channel.Reader.Epoch_Changed =>
               Log.Text_IO.Put_Line
                 (Item => "Input event channel: Epoch changed");
            when Input.Event_Channel.Reader.No_Data
               | Input.Event_Channel.Reader.Overrun_Detected =>
               null;
            when Input.Event_Channel.Reader.Inactive =>
               Log.Text_IO.Put_Line
                 (Item => "Input event channel: Inactive");
            when Input.Event_Channel.Reader.Success =>
               Mux.Terminals.Process_Input (Event => Ev);
         end case;

         exit when Res /= Input.Event_Channel.Reader.Success;
      end loop;
   end Process;

end Input_Events;
