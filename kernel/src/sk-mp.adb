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

with Skp;

with SK.Barriers;

package body SK.MP
with
   Refined_State => (Barrier => All_Barrier)
is

   All_Barrier : Barriers.Sense_Barrier_Type
     with
       Async_Readers,
       Async_Writers;

   -------------------------------------------------------------------------

   procedure Wait_For_All
   with
      Refined_Global  => (In_Out => All_Barrier),
      Refined_Depends => (All_Barrier =>+ null)
   is
   begin

      --  Workaround for [NA10-010] (no named arguments)

      Barriers.Wait (All_Barrier);
   end Wait_For_All;

begin
   Barriers.Set_Size
     (All_Barrier,                    --  Workaround for [NA10-010]
      SK.Byte (Skp.CPU_Range'Last));  --  (no named arguments)
end SK.MP;
