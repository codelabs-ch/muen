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
with Dbg.PC_Speaker_Dbg;
with Dbg.Serial;
with Dbg.Shared_Memory;
with Dbg.Xhci_Dbg;
with Dbg.Byte_Queue;
with Dbg.Channels;

package body Dbg
is

   use Channels;

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
         Initialize_Channel (Channel => Instance (Channel));
      end loop;

      Serial.Init;
      Shared_Memory.Init;
      Xhci_Dbg.Init;
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

      --  Run PC speaker debug interface.
      procedure Run_PC_Speaker_Dbg (Channel : in out Channel_Type);
      procedure Run_PC_Speaker_Dbg (Channel : in out Channel_Type)
      is
      begin
         PC_Speaker_Dbg.Run (Output_Queue => Channel.Output);
      end Run_PC_Speaker_Dbg;

      --  Run serial debug interface.
      procedure Run_Serial (Channel : in out Channel_Type);
      procedure Run_Serial (Channel : in out Channel_Type)
      is
      begin
         Serial.Run
           (Input_Queue  => Channel.Input,
            Output_Queue => Channel.Output);
      end Run_Serial;

      --  Run shared memory interface.
      procedure Run_Shared_Memory (Channel : in out Channel_Type);
      procedure Run_Shared_Memory (Channel : in out Channel_Type)
      is
      begin
         Shared_Memory.Run (Output_Queue => Channel.Output);
      end Run_Shared_Memory;

      --  Run xHCI Debug Capability.
      procedure Run_xHC_Dbg (Channel : in out Channel_Type);
      procedure Run_xHC_Dbg (Channel : in out Channel_Type)
      is
      begin
         Xhci_Dbg.Run
           (Input_Queue  => Channel.Input,
            Output_Queue => Channel.Output);
      end Run_xHC_Dbg;
   begin
      for Channel in Debug_Interfaces_Type loop
         Run_Buffers (Channel => Instance (Channel));
      end loop;

      Run_Serial (Channel => Instance (INTERFACE_SERIAL));
      Run_Shared_Memory (Channel => Instance (INTERFACE_SHMEM));
      Run_xHC_Dbg (Channel => Instance (INTERFACE_XHCDBG));
      Run_PC_Speaker_Dbg (Channel => Instance (INTERFACE_PCSPKR));
   end Run;

end Dbg;
