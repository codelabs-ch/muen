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

with Muchannel.Reader;
with Muchannel.Writer;

package Mux.Channels
is

   package VT_Channel is new Muchannel
     (Element_Type => Character,
      Elements     => 4032);

   package VT_Channel_Rdr is new VT_Channel.Reader (Protocol => 1);
   package VT_Channel_Wtr is new VT_Channel.Writer
     (Protocol     => 1,
      Null_Element => ASCII.NUL);

   --  Init channels.
   procedure Init;

   --  Read next character from input channel given by index.
   procedure Read
     (Channel :     Input_Channel_Range;
      Char    : out Character;
      Result  : out VT_Channel_Rdr.Result_Type);

   --  Syncronize input channel given by index.
   procedure Synchronize (Channel : Input_Channel_Range);

   --  Write character to output channel.
   procedure Write (Char : Character);

end Mux.Channels;
