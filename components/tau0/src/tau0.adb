--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK;

with Tau0_Kernel_Iface;

use type SK.Word32;

procedure Tau0
with
   Global => (In_Out => Tau0_Kernel_Iface.State)
is
   Counter : SK.Word32;
begin
   Counter := 0;

   loop
      if Counter mod 2**20 = 0 then
         Tau0_Kernel_Iface.Switch_Major_Frame;
      end if;
      Counter := Counter + 1;
   end loop;
end Tau0;
