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
with VT_Channels;

package Mux.Channels
is

   --  Init channels.
   procedure Init;

   --  Read next character from input channel given by index.
   procedure Read
     (Channel :     Input_Channel_Range;
      Char    : out Character;
      Result  : out VT_Channels.VT_Channel_Rdr.Result_Type);

   --  Write character to output channel given by index.
   procedure Write
     (Channel : Output_Channel_Range;
      Event   : Input.Input_Event_Type);

   --  Returns True if input channel given by index has pending data.
   function Has_Pending_Data (Channel : Input_Channel_Range) return Boolean;

end Mux.Channels;
