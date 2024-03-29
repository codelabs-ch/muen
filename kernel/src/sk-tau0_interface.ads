--
--  Copyright (C) 2013, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp.Scheduling;

with SK.CPU_Info;

--D @Interface
--D This package provides access to the Tau0 runtime interface.
package SK.Tau0_Interface
with
   Abstract_State => (State with External => Async_Writers)
is

   --  Returns major frame ID as specified by Tau0.
   procedure Get_Major_Frame (ID : out Skp.Scheduling.Major_Frame_Range)
   with
      Global  => (Input    => State,
                  Proof_In => CPU_Info.Is_BSP),
      Depends => (ID => State),
      Pre     => CPU_Info.Is_BSP;

end SK.Tau0_Interface;
