--
--  Copyright (C) 2014  secunet Security Networks AG
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

private with Dbgserver_Component.Channel_Arrays;

package Dbg
is

   --  Initialize log server.
   procedure Initialize;

   --  Run log server.
   procedure Run;

private

   type Subject_Buffer_Range is range
     1 .. Dbgserver_Component.Channel_Arrays.Log_Channels_Element_Count;

end Dbg;
