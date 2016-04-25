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

with Skp;

package SK.Subject_Interrupts
with
   Abstract_State => (State with External => (Async_Writers, Async_Readers)),
   Initializes    => State
is

   --  Insert new interrupt for given subject.
   procedure Insert_Interrupt
     (Subject : Skp.Subject_Id_Type;
      Event   : SK.Byte)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ (Event, Subject));

   --  Return True if the subject identified by ID has events pending.
   procedure Has_Pending_Events
     (Subject       :     Skp.Subject_Id_Type;
      Event_Pending : out Boolean)
   with
      Global  => (Input => State),
      Depends => (Event_Pending => (Subject, State));

   --  Consume an event of a subject given by ID. Returns False if no
   --  outstanding event is found.
   procedure Consume_Event
     (Subject :     Skp.Subject_Id_Type;
      Found   : out Boolean;
      Event   : out SK.Byte)
   with
      Global  => (In_Out => State),
      Depends => ((Event, Found, State) => (State, Subject));

end SK.Subject_Interrupts;
