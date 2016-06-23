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

with Input;

package Mux.Terminals
is

   --  Set active session slot.
   procedure Set (Slot : Output_Channel_Range);

   --  Get active session slot.
   function Get_Active_Slot return Output_Channel_Range;

   --  Initialize communication channels.
   procedure Initialize;

   --  Read data from channels and synchronize VGA output with active terminal
   --  framebuffer.
   procedure Run;

   --  Process given input event.
   procedure Process_Input (Event : Input.Input_Event_Type);

   --  Set pending data flag for given channel.
   procedure Set_Pending_Flag (Channel_Nr : Input_Channel_Range);

end Mux.Terminals;
