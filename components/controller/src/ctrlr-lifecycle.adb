--
--  Copyright (C) 2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Hypercall;
with SK.Strings;

with Debuglog.Client;

with Mucontrol.Command;
with Mucontrol.Status;

with Ctrlr.Commands;
with Ctrlr.Config;
with Ctrlr.Slot_Control;
with Ctrlr.Status;

package body Ctrlr.Lifecycle
is

   use type  Mucontrol.Status.State_Type;

   --  Returns True if the given subject status designates an error.
   function Has_Error
     (Subject_State : Mucontrol.Status.State_Type)
      return Boolean
   is ((Subject_State and Mucontrol.Status.STATE_ERROR) > 0);

   --  Returns True if all runtime dependencies for the subject specified by ID
   --  are met.
   function Run_Dependencies_Satisfied
     (ID : Managed_Subjects_Range)
      return Boolean;

   --  Log the given state as string.
   procedure Put (State : Run_State_Type);

   -------------------------------------------------------------------------

   procedure Put (State : Run_State_Type)
   is
   begin
      case State is
         when FSM_Start        => Debuglog.Client.Put (Item => "Start");
         when FSM_Initial      => Debuglog.Client.Put (Item => "Initial");
         when FSM_Syncing      => Debuglog.Client.Put (Item => "Syncing");
         when FSM_Erasing      => Debuglog.Client.Put (Item => "Erasing");
         when FSM_Preparing    => Debuglog.Client.Put (Item => "Preparing");
         when FSM_Validating   => Debuglog.Client.Put (Item => "Validating");
         when FSM_Running      => Debuglog.Client.Put (Item => "Running");
         when FSM_Finished     => Debuglog.Client.Put (Item => "Finished");
         when FSM_Resetting    => Debuglog.Client.Put (Item => "Resetting");
         when FSM_Error        => Debuglog.Client.Put (Item => "Error");
         when FSM_Self_Control => Debuglog.Client.Put (Item => "Self-Control");
      end case;
   end Put;

   -------------------------------------------------------------------------

   procedure Dump_Command (ID : Managed_Subjects_Range)
   is
      Cur_Cmd         : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64 (Commands.Get_Command (ID => ID));
      Cur_Epoch       : constant Interfaces.Unsigned_64
        := Commands.Get_Epoch (ID => ID);
      Cur_WD_Interval : constant Interfaces.Unsigned_64
        := Commands.Get_Watchdog_Interval (ID => ID);
   begin
      Debuglog.Client.Put (Item => SK.Strings.Img (SK.Byte (ID)));
      Debuglog.Client.Put (Item => ": Command ");
      Debuglog.Client.Put (Item => SK.Strings.Img (Cur_Cmd));
      Debuglog.Client.Put (Item => ", Epoch ");
      Debuglog.Client.Put (Item => SK.Strings.Img (Cur_Epoch));
      Debuglog.Client.Put (Item => ", WD Interval ");
      Debuglog.Client.Put_Line (Item => SK.Strings.Img (Cur_WD_Interval));
   end Dump_Command;

   -------------------------------------------------------------------------

   procedure Dump_Status (ID : Managed_Subjects_Range)
   is
      Cur_State       : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64 (Status.Get_State (ID => ID));
      Cur_Diagnostics : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64 (Status.Get_Diagnostics (ID => ID));
      Cur_WD_Value    : constant Interfaces.Unsigned_64
        := Status.Get_Watchdog (ID => ID);
   begin
      Debuglog.Client.Put (Item => SK.Strings.Img (SK.Byte (ID)));
      Debuglog.Client.Put (Item => ": State   ");
      Debuglog.Client.Put (Item => SK.Strings.Img (Cur_State));
      Debuglog.Client.Put (Item => ", Diagnostics ");
      Debuglog.Client.Put (Item => SK.Strings.Img (Cur_Diagnostics));
      Debuglog.Client.Put (Item => ", WD ");
      Debuglog.Client.Put_Line (Item => SK.Strings.Img (Cur_WD_Value));
   end Dump_Status;

   -------------------------------------------------------------------------

   procedure Log_Transition
     (ID        : Managed_Subjects_Range;
      Old_State : Run_State_Type;
      New_State : Run_State_Type)
   is
   begin
      Debuglog.Client.Put (Item => SK.Strings.Img (SK.Byte (ID)));
      Debuglog.Client.Put (Item => ": ");
      Put (State => Old_State);
      Debuglog.Client.Put (Item => "->");
      Put (State => New_State);
      Debuglog.Client.New_Line;
   end Log_Transition;

   -------------------------------------------------------------------------

   function Run_Dependencies_Satisfied
     (ID : Managed_Subjects_Range)
      return Boolean
   is
      use type Config.Subject_Dependency_Type;

      Result : Boolean;
   begin
      Result := True;
      for Dep of Config.Instance (ID).Run_Deps loop
         if Dep /= Config.No_Dep
           and then States (Dep.Dep_ID).Current_State /= Dep.Dep_State
         then
            Result := False;
         end if;
      end loop;

      return Result;
   end Run_Dependencies_Satisfied;

   -------------------------------------------------------------------------

   function Get_Next_State
     (ID            : Managed_Subjects_Range;
      Subject_State : Mucontrol.Status.State_Type;
      Control_Era   : Interfaces.Unsigned_64)
     return Run_State_Type
   is
      use type Interfaces.Unsigned_64;

      Next_State : Run_State_Type := States (ID).Current_State;
   begin
      case States (ID).Current_State is
         when FSM_Start
            | FSM_Resetting =>
            Next_State := FSM_Initial;
         when FSM_Initial  =>
            if Config.Instance (ID).Self_Governed then
               if Run_Dependencies_Satisfied (ID => ID) then
                  Next_State := FSM_Self_Control;
               end if;
            else
               if Subject_State = Mucontrol.Status.STATE_INITIAL then
                  Next_State := FSM_Syncing;
               end if;
            end if;
         when FSM_Syncing =>
            if Subject_State = Mucontrol.Status.STATE_SYNCED then
               if Config.Instance (ID).Initial_Erase then
                  Next_State := FSM_Erasing;
               else
                  Next_State := FSM_Preparing;
               end if;
            end if;
         when FSM_Erasing =>
            if Subject_State = Mucontrol.Status.STATE_ERASED then
               Next_State := FSM_Preparing;
            elsif Has_Error (Subject_State => Subject_State) then
               Next_State := FSM_Error;
            end if;
         when FSM_Preparing =>
            if Subject_State = Mucontrol.Status.STATE_PREPARED then
               Next_State := FSM_Validating;
            elsif Has_Error (Subject_State => Subject_State) then
               Next_State := FSM_Error;
            end if;
         when FSM_Validating =>
            if Subject_State = Mucontrol.Status.STATE_VALIDATED
              and then Run_Dependencies_Satisfied (ID => ID)
            then
               Next_State := FSM_Running;
            elsif Has_Error (Subject_State => Subject_State) then
               Next_State := FSM_Error;
            end if;
         when FSM_Running
            | FSM_Self_Control =>
            if States (ID).Current_Era /= Control_Era then
               Next_State := FSM_Resetting;
            elsif Subject_State = Mucontrol.Status.STATE_FINISHED then
               Next_State := FSM_Finished;
            elsif Has_Error (Subject_State => Subject_State) then
               Next_State := FSM_Error;
            end if;
         when FSM_Finished
            | FSM_Error =>
            if States (ID).Current_Era /= Control_Era then
               Next_State := FSM_Resetting;
            end if;
      end case;

      return Next_State;
   end Get_Next_State;

   -------------------------------------------------------------------------

   procedure Perform_Transition
     (ID        : Managed_Subjects_Range;
      New_State : Run_State_Type;
      Era       : Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_64;
      use type Config.Reset_Event_Range;
   begin
      case New_State is
         when FSM_Initial =>
            States (ID).Current_Epoch := States (ID).Current_Epoch + 1;
            Commands.Initialize
              (ID          => ID,
               Command     => (if Config.Instance (ID).Self_Governed
                               then Mucontrol.Command.CMD_SELF_CTRL
                               else Mucontrol.Command.CMD_NOP),
               Epoch       => States (ID).Current_Epoch,
               WD_Interval => Config.Instance (ID).WD_Interval);
         when FSM_Syncing =>
            Commands.Set_Command (ID    => ID,
                                  Value => Mucontrol.Command.CMD_SYNC);
         when FSM_Erasing =>
            Commands.Set_Command (ID    => ID,
                                  Value => Mucontrol.Command.CMD_ERASE);
         when FSM_Preparing =>
            Commands.Set_Command (ID    => ID,
                                  Value => Mucontrol.Command.CMD_PREPARE);
         when FSM_Validating =>
            Commands.Set_Command (ID    => ID,
                                  Value => Mucontrol.Command.CMD_VALIDATE);
         when FSM_Running =>
            Commands.Set_Command (ID    => ID,
                                  Value => Mucontrol.Command.CMD_RUN);
         when FSM_Self_Control =>
            Commands.Set_Command (ID    => ID,
                                  Value => Mucontrol.Command.CMD_SELF_CTRL);
         when FSM_Resetting =>
            Commands.Set_Command (ID    => ID,
                                  Value => Mucontrol.Command.CMD_NOP);
            for Evt of Config.Instance (ID).Reset_Events loop
               if Evt /= Config.Null_Reset_Event then
                  SK.Hypercall.Trigger_Event
                    (Number => Interfaces.Unsigned_8 (Evt));
               end if;
            end loop;
            States (ID).Current_Era := Era;
         when FSM_Error =>
            Dump_Status (ID => ID);
            Dump_Command (ID => ID);
         when FSM_Start
            | FSM_Finished =>
            null;
      end case;
   end Perform_Transition;

   -------------------------------------------------------------------------

   procedure Process (ID : Managed_Subjects_Range)
   is
      Cur_State : Mucontrol.Status.State_Type;
      Cur_Era   : Interfaces.Unsigned_64;
      New_State : Run_State_Type;
   begin
      Cur_State := Status.Get_State (ID => ID);
      Cur_Era   := Slot_Control.Get_Current_Era (ID => ID);
      New_State := Get_Next_State
        (ID            => ID,
         Subject_State => Cur_State,
         Control_Era   => Cur_Era);

      if New_State /= States (ID).Current_State then
         Perform_Transition (ID        => ID,
                             New_State => New_State,
                             Era       => Cur_Era);
         Log_Transition (ID        => ID,
                         Old_State => States (ID).Current_State,
                         New_State => New_State);
         States (ID).Current_State := New_State;
      end if;
   end Process;

end Ctrlr.Lifecycle;
