--
--  Copyright (C) 2014-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with System;

with Skp.Kernel;

package body SK.Timed_Events
with
   Refined_State => (State => Subject_Timers)
is

   type Timed_Event_Interface_Type is record
      TSC_Trigger_Value : SK.Word64;
      Event_Nr          : SK.Byte;
   end record;

   for Timed_Event_Interface_Type use record
      TSC_Trigger_Value at 0 range 0 .. 63;
      Event_Nr          at 8 range 0 ..  7;
   end record;

   Null_Event : constant Timed_Event_Interface_Type
     := (TSC_Trigger_Value => SK.Word64'Last,
         Event_Nr          => 0);

   pragma Warnings (GNAT, Off, "*padded by * bits");
   type Subject_Timer_Array is array
     (Skp.Subject_Id_Type) of Timed_Event_Interface_Type
   with
      Independent_Components,
      Component_Size => Page_Size * 8,
      Alignment      => Page_Size;
   pragma Warnings (GNAT, On, "*padded by * bits");

   --  Subject timer pages.
   Subject_Timers : Subject_Timer_Array
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Skp.Kernel.Subj_Timed_Events_Address);

   -------------------------------------------------------------------------

   procedure Clear_Timer (Subject : Skp.Subject_Id_Type)
   with
      Refined_Global  => (In_Out => Subject_Timers),
      Refined_Depends => (Subject_Timers =>+ Subject)
   is
   begin
      Subject_Timers (Subject).TSC_Trigger_Value := SK.Word64'Last;
   end Clear_Timer;

   -------------------------------------------------------------------------

   procedure Get_Timer
     (Subject :     Skp.Subject_Id_Type;
      Value   : out SK.Word64;
      Vector  : out SK.Byte)
   with
      Refined_Global  => (Input => Subject_Timers),
      Refined_Depends => ((Value, Vector) => (Subject_Timers, Subject))
   is
   begin
      Value  := Subject_Timers (Subject).TSC_Trigger_Value;
      Vector := Subject_Timers (Subject).Event_Nr;
   end Get_Timer;

   -------------------------------------------------------------------------

   procedure Init_Timer (Subject : Skp.Subject_Id_Type)
   with
      Refined_Global  => (In_Out => Subject_Timers),
      Refined_Depends => (Subject_Timers =>+ Subject)
   is
   begin
      Subject_Timers (Subject) := Null_Event;
   end Init_Timer;

end SK.Timed_Events;
