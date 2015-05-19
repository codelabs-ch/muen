--
--  Copyright (C) 2013, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Muchannel.Readers;
with Muchannel.Writer;

with Input;

package VT_Channels
is

   package VT_Channel is new Muchannel
     (Element_Type => Character,
      Elements     => 65472,
      Null_Element => ASCII.NUL,
      Protocol     => 1);
   package VT_Channel_Rdr is new VT_Channel.Readers;

   --  Input event channel used to report keyboard/mouse events.
   package Input_Event_Channel is new Muchannel
     (Element_Type => Input.Input_Event_Type,
      Elements     => 168,
      Null_Element => Input.Null_Input_Event,
      Protocol     => 16#9a0a8679dbc22dcb#);
   package Input_Event_Channel_Wtr is new Input_Event_Channel.Writer;

end VT_Channels;
