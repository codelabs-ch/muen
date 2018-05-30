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

with Skp.Events;

with SK.CPU_Info;

private with SK.Constants;

package SK.Subjects_Events
with
   Abstract_State => (State with External => (Async_Writers, Async_Readers))
is

   --  Initialize pending events of all subjects.
   procedure Initialize
   with
      Global  => (Output   => State,
                  Proof_In => CPU_Info.Is_BSP),
      Depends => (State => null),
      Pre     => CPU_Info.Is_BSP;

   --  Set event with given ID of specified subject pending.
   procedure Set_Event_Pending
     (Subject  : Skp.Global_Subject_ID_Type;
      Event_ID : Skp.Events.Event_Range)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ (Event_ID, Subject));

   --  Consume an event of the subject given by ID. Returns False if no
   --  pending event is found.
   procedure Consume_Event
     (Subject :     Skp.Global_Subject_ID_Type;
      Found   : out Boolean;
      Event   : out Skp.Events.Event_Range)
   with
      Global  => (In_Out => State),
      Depends => ((Event, Found, State) => (State, Subject));

   --  Clear events of subject with given ID.
   procedure Clear_Events (Subject : Skp.Global_Subject_ID_Type)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ Subject);

private

   type Atomic32_Type is record
      Bits : Word64 with Atomic;
   end record
   with
      Atomic,
      Size      => 64,
      Alignment => 8;

   pragma Compile_Time_Error
     ((Atomic32_Type'Size < 2 ** Skp.Events.Event_Bits),
      "Pending event bit size too small");

   type Pending_Events_Array is array (Skp.Global_Subject_ID_Type)
     of Atomic32_Type
   with
      Independent_Components;

   Global_Pending_Events : Pending_Events_Array
   with
      Volatile,
      Async_Writers,
      Async_Readers,
      Linker_Section => Constants.Global_Data_Section,
      Part_Of        => State;

end SK.Subjects_Events;
