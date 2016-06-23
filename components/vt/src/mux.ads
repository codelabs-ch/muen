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

with Vt_Component.Channel_Arrays;

package Mux
is

   package Cspecs renames Vt_Component.Channel_Arrays;

   --  Number of input channels.
   type Input_Channel_Range is range 1 .. Cspecs.Input_Devices_Element_Count;

   --  Number of output channels.
   type Output_Channel_Range is range 1 .. Cspecs.Console_Element_Count;

end Mux;
