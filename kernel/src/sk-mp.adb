--
--  Copyright (C) 2013-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with SK.Constants;

pragma Elaborate_All (SK.Barriers);

package body SK.MP
with
   Refined_State => (Barrier => (Global_All_Barrier,
                                 Global_Minor_Frame_Barriers))
is

   type Minor_Frame_Barriers_Array is
     array (Skp.Scheduling.Barrier_Range) of Barriers.Sense_Barrier_Type;

   Global_Minor_Frame_Barriers : Minor_Frame_Barriers_Array
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Linker_Section => Constants.Global_Data_Section;

   Global_All_Barrier : Barriers.Sense_Barrier_Type
   with
      Async_Readers,
      Async_Writers,
      Linker_Section => Constants.Global_Data_Section;

   -------------------------------------------------------------------------

   procedure Initialize_All_Barrier
   with
      Refined_Global  => (Output   => Global_All_Barrier,
                          Proof_In => CPU_Info.Is_BSP),
      Refined_Depends => (Global_All_Barrier => null)
   is
   begin
      Barriers.Initialize (Barrier => Global_All_Barrier,
                           Size    => Byte (Skp.CPU_Count));
   end Initialize_All_Barrier;

   -------------------------------------------------------------------------

   procedure Set_Minor_Frame_Barrier_Config
     (Config : Skp.Scheduling.Barrier_Config_Array)
   with
      Refined_Global  => (Output   => Global_Minor_Frame_Barriers,
                          Proof_In => CPU_Info.Is_BSP),
      Refined_Depends => (Global_Minor_Frame_Barriers => Config)
   is
   begin
      pragma Warnings (Off);
      for I in Config'Range loop
         pragma Warnings (On);
         Barriers.Initialize (Barrier => Global_Minor_Frame_Barriers (I),
                              Size    => Byte (Config (I)));
      end loop;
   end Set_Minor_Frame_Barrier_Config;

   -------------------------------------------------------------------------

   procedure Wait_For_All
   with
      Refined_Global  => (In_Out => Global_All_Barrier),
      Refined_Depends => (Global_All_Barrier =>+ null)
   is
   begin
      Barriers.Wait (Barrier => Global_All_Barrier);
   end Wait_For_All;

   -------------------------------------------------------------------------

   procedure Wait_On_Minor_Frame_Barrier
     (Index : Skp.Scheduling.Barrier_Range)
   with
      Refined_Global  => (In_Out => Global_Minor_Frame_Barriers),
      Refined_Depends => (Global_Minor_Frame_Barriers =>+ Index)
   is
   begin
      Barriers.Wait (Barrier => Global_Minor_Frame_Barriers (Index));
   end Wait_On_Minor_Frame_Barrier;

end SK.MP;
