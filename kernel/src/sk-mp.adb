--
--  Copyright (C) 2013-2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Barriers;

pragma Elaborate_All (SK.Barriers);

package body SK.MP
with
   Refined_State => (Barrier => (All_Barrier, Minor_Frame_Barriers))
is

   type Minor_Frame_Barriers_Array is
     array (Skp.Scheduling.Barrier_Index_Range) of Barriers.Sense_Barrier_Type;

   Minor_Frame_Barriers : Minor_Frame_Barriers_Array
     with
       Volatile,
       Async_Readers,
       Async_Writers;

   All_Barrier : Barriers.Sense_Barrier_Type
     with
       Async_Readers,
       Async_Writers;

   -------------------------------------------------------------------------

   procedure Set_Minor_Frame_Barrier_Config
     (Config : Skp.Scheduling.Barrier_Config_Array)
   with
      SPARK_Mode      => $Complete_Proofs,  -- [NB04-057]
      Refined_Global  => (In_Out => Minor_Frame_Barriers),
      Refined_Depends => (Minor_Frame_Barriers =>+ Config)
   is
   begin
      for I in Config'Range loop
         Barriers.Initialize (Barrier => Minor_Frame_Barriers (I),
                              Size    => SK.Byte (Config (I)));
      end loop;
   end Set_Minor_Frame_Barrier_Config;

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

   -------------------------------------------------------------------------

   procedure Wait_On_Minor_Frame_Barrier
     (Index : Skp.Scheduling.Barrier_Index_Range)
   with
      SPARK_Mode      => $Complete_Proofs,  -- [NB04-057]
      Refined_Global  => (In_Out => Minor_Frame_Barriers),
      Refined_Depends => (Minor_Frame_Barriers =>+ Index)
   is
   begin

      --  Workaround for [NA10-010] (no named arguments)

      Barriers.Wait (Minor_Frame_Barriers (Index));
   end Wait_On_Minor_Frame_Barrier;

begin
   Barriers.Initialize (Barrier => All_Barrier,
                        Size    => SK.Byte (Skp.CPU_Count));
end SK.MP;
