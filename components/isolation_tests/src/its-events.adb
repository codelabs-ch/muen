--
--  Copyright (C) 2021  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2021  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with SK.Hypercall;
with SK.Strings;

with ITS.Log_Buffer;
with ITS.Results;
with ITS.Subject_State;

package body ITS.Events
is

   -------------------------------------------------------------------------

   procedure Trigger_Invalid_Event
   is
      use type Interfaces.Unsigned_32;

      Exit_Marker     : constant Interfaces.Unsigned_32 := 16#1234_5678#;
      Event_Nr        : constant Interfaces.Unsigned_8 := 16#ff#;
      Title           : constant String
        := "Trigger Event with Number outside of valid Range";
      Description     : constant String
        := "This test verifies that triggering a hypercall with an event "
        & "number that is out of range is ignored and does not lead to a "
        & "trap by setting Exit Reason to an arbitrary value and asserting "
        & "that it remains unchanged.";
      Expected_Result : constant String
        := "No VM-Exit/unchanged result state.";
      Log_ID          : Log_Buffer.Ext_Log_Entries_Range;
      Result          : SK.Subject_State_Type;
      Success         : Boolean;
      Start, Stop     : Interfaces.Unsigned_64;
      Src_Info        : constant String
        := Enclosing_Entity & ", " & Source_Location;
   begin
      Start := Musinfo.Instance.TSC_Schedule_Start;
      ITS.Subject_State.Result_State := SK.Null_Subject_State;
      ITS.Subject_State.Result_State.Exit_Reason := 16#ffff#;

      Log_Buffer.Start_Entry (ID => Log_ID);

      Log_Buffer.Put_Line (Str => "Setting Exit Reason to "
                           & SK.Strings.Img (Item => Exit_Marker) & ".");
      ITS.Subject_State.Result_State.Exit_Reason := Exit_Marker;

      Log_Buffer.Put_Line (Str => "Triggering event "
                           & SK.Strings.Img (Item => Event_Nr) & ".");
      Log_Buffer.New_Line;
      SK.Hypercall.Trigger_Event (Number => Event_Nr);
      Result := ITS.Subject_State.Result_State;

      Log_Buffer.Put_Line (Str => ".:[ Assertions ]:.");
      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Exit Reason");
      Log_Buffer.Put_Line
        (Str => "  Expected : " & SK.Strings.Img (Item => Exit_Marker));
      Log_Buffer.Put_Line
        (Str => "  Result   : " & SK.Strings.Img (Item => Result.Exit_Reason));

      Success := Result.Exit_Reason = Exit_Marker;

      Stop := Musinfo.Instance.TSC_Schedule_End;
      Results.Append
        (Title           => Title,
         Description     => Description,
         Expected        => Expected_Result,
         Source_Info     => Src_Info,
         Success         => Success,
         Start_Timestamp => Start,
         End_Timestamp   => Stop,
         Log_Entry       => Log_ID);
   end Trigger_Invalid_Event;

end ITS.Events;
