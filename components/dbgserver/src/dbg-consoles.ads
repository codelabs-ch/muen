--
--  Copyright (C) 2017  secunet Security Networks AG
--  Copyright (C) 2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Dbg.Byte_Queue;

package Dbg.Consoles
is

   Command_Buffer_Size : constant := 80;

   subtype Command_Buffer_Range is Natural range 1 .. Command_Buffer_Size;

   subtype Command_Buffer_Type is String (Command_Buffer_Range);

   type Console_Type is record
      --  The console has its own output queue to separate the emitted strings
      --  from the log messages.
      Output_Queue       : Byte_Queue.Queue_Type;
      --  Storage for the received commands.
      Command_Buffer     : Command_Buffer_Type;
      --  Buffer position of the next free character.
      Command_Buffer_Pos : Natural;
   end record;

   --  Initializes the given debug console.
   procedure Initialize (Console : out Console_Type);

   --  Processes input from the given input queue and execute commands present
   --  in the input.
   procedure Run
     (Console     : in out Console_Type;
      Input_Queue : in out Byte_Queue.Queue_Type);

private

   Empty_Command_Buffer : constant Command_Buffer_Type := (others => ' ');

   type Command_Kind is
     (Clear_Line,
      Failure,
      List_Channels,
      List_Subjects,
      Log_All,
      Log_None,
      Log_Toggle,
      Print_Help,
      Print_Status,
      Reboot,
      Show_Prompt,
      Shutdown,
      Stream_Reset,
      Trigger_Event);

   type Command_Type (Kind : Command_Kind := Failure) is record
      case Kind is
         when Trigger_Event => Event_Number   : Natural := 0;
         when Log_Toggle    => Channel_Number : Natural := 0;
         when others        => null;
      end case;
   end record;

end Dbg.Consoles;
