--
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

with Interfaces;

with Muchannel;
with Muchannel_Constants;

with SHMStream_Constants;

with Dbgserver_Component.Channel_Arrays;

pragma Elaborate_All (Muchannel);

package Dbg.Subject_Consoles.Stream is new Muchannel
  (Element_Type => Interfaces.Unsigned_8,
   Elements     =>
     (Dbgserver_Component.Channel_Arrays.Subject_Consoles_In_Element_Size
      - Muchannel_Constants.Header_Size),
   Null_Element => 0,
   Protocol     => SHMStream_Constants.HVC_MUEN_PROTOCOL);
