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

--D @Interface
--D This package provides facilities for managing subject target events. Each
--D subject has a fixed number of events that can be marked as pending. Events
--D are processed prior to resuming the execution of the associated subject.
--D Pending events are marked by their ID, which is used as a lookup index into
--D the static policy event array.
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

   --D @Interface
   --D 64-bit atomic type, where each bit represents a pending event with the
   --D event number corresponding to the bit position.
   type Atomic64_Type is record
      --D @Interface
      --D 64-bits accessible atomically to enable concurrent access.
      Bits : Word64 with Atomic;
   end record
   with
      Atomic,
      Size      => 64,
      Alignment => 8;

   pragma Compile_Time_Error
     ((Atomic64_Type'Size < 2 ** Skp.Events.Event_Bits),
      "Pending event bit size too small");

   type Pending_Events_Array is array (Skp.Global_Subject_ID_Type)
     of Atomic64_Type
   with
      Independent_Components;

   --D @Interface
   --D Bitmap of the currently pending subject target events. Each subject has
   --D a corresponding pending events data structure which may be accessed
   --D asynchronously by all CPU cores. The CPU executing the associated
   --D subject consumes pending events while all CPUs may mark target events as
   --D pending if allowed by the policy. Data consistency is established via
   --D atomic access.
   Global_Pending_Events : Pending_Events_Array
   with
      Volatile,
      Async_Writers,
      Async_Readers,
      Linker_Section => Constants.Global_Data_Section,
      Part_Of        => State;

end SK.Subjects_Events;
