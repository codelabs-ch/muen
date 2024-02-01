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

with SK.Arch;

package body SK.Timed_Events
with
   Refined_State => (State => Subject_Events)
is

   Null_Event : constant Mutimedevents.Timed_Event_Interface_Type
     := (TSC_Trigger_Value => SK.Word64'Last,
         Event_Nr          => 0,
         Padding           => 0);

   -------------------------------------------------------------------------

   procedure Clear_Event (Subject : Skp.Global_Subject_ID_Type)
   with
      Refined_Global  => (In_Out => Subject_Events),
      Refined_Depends => (Subject_Events =>+ Subject)
   is
   begin
      --D @Interface
      --D Set TSC trigger value of timed event for specific subject to
      --D \textt{SK.Word'Last $2^{(64}-1$}.
      Subject_Events (Subject).Data.TSC_Trigger_Value := SK.Word64'Last;
   end Clear_Event;

   -------------------------------------------------------------------------

   procedure Get_Event
     (Subject           :     Skp.Global_Subject_ID_Type;
      TSC_Trigger_Value : out SK.Word64;
      Event_Nr          : out Skp.Events.Event_Range)
   with
      Refined_Global  => (Input => Subject_Events),
      Refined_Depends =>
        ((TSC_Trigger_Value, Event_Nr) => (Subject_Events, Subject))
   is
   begin
      --D @Interface
      --D Read current TSC trigger value from timed event of specific subject.
      TSC_Trigger_Value := Subject_Events (Subject).Data.TSC_Trigger_Value;
      --D @Interface
      --D Read current event number value from timed event of specific subject.
      Event_Nr          := Skp.Events.Event_Range
        (Subject_Events (Subject).Data.Event_Nr);
   end Get_Event;

   -------------------------------------------------------------------------

   function Get_Trigger_Value
     (Subject : Skp.Global_Subject_ID_Type)
      return Word64
   is (Subject_Events (Subject).Data.TSC_Trigger_Value)
   with
      Refined_Global  => (Input => Subject_Events),
      Refined_Depends =>
        (Get_Trigger_Value'Result => (Subject_Events, Subject));

   -------------------------------------------------------------------------

   function Has_Expired (Subject : Skp.Global_Subject_ID_Type) return Boolean
   with
      Refined_Global  => (Input => (Subject_Events, X86_64.State)),
      Refined_Depends =>
         (Has_Expired'Result => (Subject_Events, Subject, X86_64.State))
   is
      Now               : constant Word64 := Arch.Get_Current_Timestamp;
      Cur_Trigger_value : constant Word64
        := Subject_Events (Subject).Data.TSC_Trigger_Value;
   begin
      return Cur_Trigger_value <= Now;
   end Has_Expired;

   -------------------------------------------------------------------------

   procedure Init_Event (Subject : Skp.Global_Subject_ID_Type)
   with
      Refined_Global  => (In_Out => Subject_Events),
      Refined_Depends => (Subject_Events =>+ Subject)
   is
   begin
      --D @Interface
      --D Set timed event of specific subject to \texttt{Null\_Event}.
      Subject_Events (Subject).Data := Null_Event;
   end Init_Event;

end SK.Timed_Events;
