--
--  Copyright (C) 2018  secunet Security Networks AG
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

with Dbg.Byte_Queue.Format;

with Dbgserver_Component.Config;

package body Dbg.Channels
with SPARK_Mode => Off
is

   package Cspecs_Cfg renames Dbgserver_Component.Config;

   type Dbg_Interface_Info is record
      Name    : String (1 .. 6);
      Enabled : Boolean;
   end record;

   type Dbg_Interface_To_String_Array is
     array (Debug_Interfaces_Type) of Dbg_Interface_Info;

   Debug_Interface_Info : constant Dbg_Interface_To_String_Array
     := (INTERFACE_SERIAL => (Name => "Serial",
                              Enabled => Cspecs_Cfg.Sink_Serial),
         INTERFACE_XHCDBG => (Name => "xHCI  ",
                              Enabled => Cspecs_Cfg.Sink_Xhcidbg),
         INTERFACE_PCSPKR => (Name => "PCSPKR",
                              Enabled => Cspecs_Cfg.Sink_Pcspkr),
         INTERFACE_SHMEM  => (Name => "SHMEM ",
                              Enabled => Cspecs_Cfg.Sink_Shmem));

   -------------------------------------------------------------------------

   procedure Print_State (Queue : in out Byte_Queue.Queue_Type)
   is
      --  Print general debug interface info.
      procedure Print_Interface_Info
        (Debug_Interface : Debug_Interfaces_Type);

      --  Print state of channel of given debug interface.
      procedure Print_Channel_State (Debug_Interface : Debug_Interfaces_Type);

      ----------------------------------------------------------------------

      procedure Print_Channel_State (Debug_Interface : Debug_Interfaces_Type)
      is
      begin
         if Debug_Interface in Debug_Console_Type then
            Byte_Queue.Format.Append_Line
              (Queue => Queue,
               Item  => "[Input queue]");
            Byte_Queue.Format.Append_State
              (Queue => Queue,
               Item  => Instance (Debug_Interface).Input);
         end if;

         Byte_Queue.Format.Append_Line
           (Queue => Queue,
            Item  => "[Output queue]");
         Byte_Queue.Format.Append_State
           (Queue => Queue,
            Item  => Instance (Debug_Interface).Output);

         Byte_Queue.Format.Append_Line
           (Queue => Queue,
            Item  => "[Sources]");
         Buffers.Print_State
           (Buffer => Instance (Debug_Interface).Buffer,
            Queue  => Queue);
      end Print_Channel_State;

      ----------------------------------------------------------------------

      procedure Print_Interface_Info (Debug_Interface : Debug_Interfaces_Type)
      is
      begin
         Byte_Queue.Format.Append_String
           (Queue => Queue,
            Item  => "Sink ");
         Byte_Queue.Format.Append_Line
           (Queue => Queue,
            Item  => Debug_Interface_Info (Debug_Interface).Name);
         Byte_Queue.Format.Append_Line
           (Queue => Queue,
            Item  => "-----------");
         if Debug_Interface_Info (Debug_Interface).Enabled then
            Byte_Queue.Format.Append_Line
              (Queue => Queue,
               Item  => "<enabled>");
            Print_Channel_State (Debug_Interface => Debug_Interface);
         else
            Byte_Queue.Format.Append_Line
              (Queue => Queue,
               Item  => "<disabled>");
         end if;
      end Print_Interface_Info;
   begin
      for Debug_Iface in Debug_Interfaces_Type loop
         Print_Interface_Info (Debug_Interface => Debug_Iface);
         if Debug_Iface /= Debug_Interfaces_Type'Last then
            Byte_Queue.Format.Append_New_Line (Queue => Queue);
         end if;
      end loop;
   end Print_State;

end Dbg.Channels;
