--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body SK.Subjects_Events
with
   Refined_State => (State => Global_Pending_Events)
is

   -------------------------------------------------------------------------

   procedure Initialize
   with
      Refined_Global  => (Output   => Global_Pending_Events,
                          Proof_In => CPU_Info.Is_BSP),
      Refined_Depends => (Global_Pending_Events => null)
   is
   begin
      for Subj_ID in Skp.Global_Subject_ID_Type'Range loop
         --D @Interface
         --D Set pending events of all subjects to zero.
         Atomics.Init (Atomic => Global_Pending_Events (Subj_ID));
      end loop;
   end Initialize;

   -------------------------------------------------------------------------

   procedure Set_Event_Pending
     (Subject  : Skp.Global_Subject_ID_Type;
      Event_ID : Skp.Events.Event_Range)
   with
      Refined_Global  => (In_Out => Global_Pending_Events),
      Refined_Depends => (Global_Pending_Events =>+ (Event_ID, Subject))
   is
   begin
      Atomics.Set (Atomic => Global_Pending_Events (Subject),
                   Bit    => Byte (Event_ID));
   end Set_Event_Pending;

   -------------------------------------------------------------------------

   procedure Clear_Events (Subject : Skp.Global_Subject_ID_Type)
   with
      Refined_Global  => (In_Out => Global_Pending_Events),
      Refined_Depends => (Global_Pending_Events =>+ Subject)
   is
   begin
      --D @Interface
      --D Set pending events of subject with given ID to zero.
      Atomics.Init (Atomic => Global_Pending_Events (Subject));
   end Clear_Events;

   -------------------------------------------------------------------------

   procedure Consume_Event
     (Subject :     Skp.Global_Subject_ID_Type;
      Found   : out Boolean;
      Event   : out Skp.Events.Event_Range)
   with
      Refined_Global  => (In_Out => Global_Pending_Events),
      Refined_Depends => ((Event, Found, Global_Pending_Events) =>
                           (Global_Pending_Events, Subject))
   is
      Bit_Pos : Atomics.Bit_Pos;
   begin
      Event := 0;

      Atomics.Find_Highest_Bit_Set
        (Atomic => Global_Pending_Events (Subject),
         Found  => Found,
         Bit    => Bit_Pos);
      if Found then
         Event := Skp.Events.Event_Range (Bit_Pos);
         Atomics.Clear (Atomic => Global_Pending_Events (Subject),
                        Bit    => Byte (Event));
      end if;
   end Consume_Event;

end SK.Subjects_Events;
