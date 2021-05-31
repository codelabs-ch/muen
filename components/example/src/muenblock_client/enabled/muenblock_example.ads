--
--  Copyright (C) 2020 secunet Security Networks AG
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

with Musinfo.Instance;

with Muenblock_Client;
with Example_Component.Channels;

package Muenblock_Example
is
   package Muenblock_Client_Instance is new Muenblock_Client
      (Req_Channel_Address  => System'To_Address
            (Example_Component.Channels.Blockdev_Request2_Address),
       Resp_Channel_Address => System'To_Address
            (Example_Component.Channels.Blockdev_Response2_Address),
       Event_Number         =>
         Example_Component.Channels.Blockdev_Request2_Event,
       Devices_Cnt_Max      => 1);

   procedure Show
   with
      Pre => Musinfo.Instance.Is_Valid;

end Muenblock_Example;
