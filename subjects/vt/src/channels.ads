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

with System;

with Muchannel.Reader;
with Muchannel.Writer;

package Channels
is

   package VT_Channel is new Muchannel
     (Element_Type => Character,
      Elements     => 4032);

   package VT_Channel_Rdr is new VT_Channel.Reader (Protocol => 1);
   package VT_Channel_Wtr is new VT_Channel.Writer
     (Protocol     => 1,
      Null_Element => ASCII.NUL);

   Channel_1_In : VT_Channel.Channel_Type;
   for Channel_1_In'Address use System'To_Address (16#40000#);

   Channel_1_Reader : VT_Channel_Rdr.Reader_Type;

   Channel_1_Out : VT_Channel.Channel_Type;
   for Channel_1_Out'Address use System'To_Address (16#50000#);

end Channels;
