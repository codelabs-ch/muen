--
--  Copyright (C) 2014, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

private with System;

private with Skp.Kernel;

private with Mutimedevents;

with Skp.Events;

--D @Interface
--D This package provide facilities to manage timed events of each subject.
--D Timed events allow a subject to trigger a policy defined event at a given
--D time given as CPU tick value.
package SK.Timed_Events
with
   Abstract_State => (State with External => (Async_Writers, Async_Readers)),
   Initializes    => State
is

   --  Get timed event information for a subject with given ID.
   procedure Get_Event
     (Subject           :     Skp.Global_Subject_ID_Type;
      TSC_Trigger_Value : out SK.Word64;
      Event_Nr          : out Skp.Events.Event_Range)
   with
       Global  => (Input => State),
       Depends => ((TSC_Trigger_Value, Event_Nr) => (State, Subject));

   --  Clear timed event of subject with given ID.
   procedure Clear_Event (Subject : Skp.Global_Subject_ID_Type)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ Subject);

   --  Initialize timed event of subject with given ID.
   procedure Init_Event (Subject : Skp.Global_Subject_ID_Type)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ Subject);

private

   type Padding_Type is array
     (Mutimedevents.Timed_Event_Interface_Size / 8 .. Page_Size - 1) of Byte
   with
      Size => Page_Size * 8 - Mutimedevents.Timed_Event_Interface_Size;

   type Timed_Event_Page is record
      Data    : Mutimedevents.Timed_Event_Interface_Type;
      Padding : Padding_Type;
   end record
   with
      Size => Page_Size * 8;

   type Subject_Event_Array is array
     (Skp.Global_Subject_ID_Type) of Timed_Event_Page
   with
      Independent_Components,
      Component_Size => Page_Size * 8,
      Alignment      => Page_Size,
      Object_Size => (Skp.Global_Subject_ID_Type'Last + 1) * Page_Size * 8;

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   --D @Interface
   --D Subject timed events array. Each subject has an associated timed event,
   --D identified by subject ID, which it can use to trigger a policy-defined
   --D event at a specified timestamp.
   Subject_Events : Subject_Event_Array
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Part_Of => State,
      Address => System'To_Address (Skp.Kernel.Subj_Timed_Events_Address);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

end SK.Timed_Events;
