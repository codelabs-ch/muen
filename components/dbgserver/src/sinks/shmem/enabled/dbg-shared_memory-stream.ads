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

with Muchannel;
with Muchannel_Constants;

with Dbg.Shared_Memory.Types;

with Dbgserver_Component.Channels;

pragma Elaborate_All (Muchannel);

package Dbg.Shared_Memory.Stream is new Muchannel
  (Element_Type => Types.Data_Type,
   Elements     =>
     (Dbgserver_Component.Channels.Debug_Shm_Sink_Memory_Size
      - Muchannel_Constants.Header_Size) / (Types.Data_Type'Size / 8),
   Null_Element => Types.Null_Data,
   Protocol     => 16#6d3a_cd5d_ced2_3445#);
