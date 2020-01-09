--
--  Copyright (C) 2013-2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Console;
with SK.Legacy_VGA;

pragma Elaborate (SK.Console);

--  Kernel VGA debug console.
package SK.KC is new SK.Console
  (Initialize      => SK.Legacy_VGA.VGA.Init,
   Output_New_Line => SK.Legacy_VGA.VGA.New_Line,
   Output_Char     => SK.Legacy_VGA.VGA.Put_Char);
