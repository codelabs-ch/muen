--
--  Copyright (C) 2014  secunet Security Networks AG
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

with Dbg.Buffers;
with Dbg.Serial;
with Dbg.Byte_Queue;

package body Dbg
is

   type Channel_Type is record
      Buffer : Buffers.Buffer_Type;
      Input  : Byte_Queue.Queue_Type;
      Output : Byte_Queue.Queue_Type;
   end record;

   type Debug_Interfaces_Type is (INTERFACE_SERIAL);

   type Channels_Type is array (Debug_Interfaces_Type) of Channel_Type;

   Channels : Channels_Type;

   -------------------------------------------------------------------------

   procedure Initialize
   is
      --  Initialize given channel.
      procedure Initialize_Channel (Channel : out Channel_Type);
      procedure Initialize_Channel (Channel : out Channel_Type)
      is
         Banner : constant String := "DBG-LOG>";
      begin
         Buffers.Initialize (Buffer => Channel.Buffer);
         Byte_Queue.Initialize (Queue => Channel.Input);
         Byte_Queue.Initialize (Queue => Channel.Output);
         Byte_Queue.Append_String (Queue  => Channel.Output,
                                   Buffer => Banner,
                                   Length => Banner'Length);
      end Initialize_Channel;
   begin
      for Channel in Debug_Interfaces_Type loop
         Initialize_Channel (Channel => Channels (Channel));
      end loop;

      Serial.Init;
   end Initialize;

   -------------------------------------------------------------------------

   procedure Run
   is

      ----------------------------------------------------------------------

      --  Run buffers.
      procedure Run_Buffers (Channel : in out Channel_Type);
      procedure Run_Buffers (Channel : in out Channel_Type)
      is
      begin
         Buffers.Run
            (Buffer       => Channel.Buffer,
             Input_Queue  => Channel.Input,
             Output_Queue => Channel.Output);
      end Run_Buffers;

      ----------------------------------------------------------------------

      --  Run serial debug interface.
      procedure Run_Serial (Channel : in out Channel_Type);
      procedure Run_Serial (Channel : in out Channel_Type)
      is
      begin
         Serial.Run
            (Input_Queue   => Channel.Input,
              Output_Queue => Channel.Output);
      end Run_Serial;
   begin
      for Channel in Debug_Interfaces_Type loop
         Run_Buffers (Channel => Channels (Channel));
      end loop;

      Run_Serial (Channel => Channels (INTERFACE_SERIAL));
   end Run;

end Dbg;
