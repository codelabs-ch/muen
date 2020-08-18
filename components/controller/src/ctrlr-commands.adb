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

package body Ctrlr.Commands
is

   package CTRL_CMD renames Mucontrol.Command;

   -------------------------------------------------------------------------

   function Get_Command
     (ID : Managed_Subjects_Range)
      return CTRL_CMD.Command_Type
   is (Command_Pages (ID).Command);

   -------------------------------------------------------------------------

   function Get_Epoch
     (ID : Managed_Subjects_Range)
      return Interfaces.Unsigned_64
   is (Command_Pages (ID).Epoch);

   -------------------------------------------------------------------------

   function Get_Watchdog_Interval
     (ID : Managed_Subjects_Range)
      return Interfaces.Unsigned_64
   is (Command_Pages (ID).Watchdog_Interval);

   -------------------------------------------------------------------------

   procedure Initialize
     (ID          : Managed_Subjects_Range;
      Command     : Mucontrol.Command.Command_Type;
      Epoch       : Interfaces.Unsigned_64;
      WD_Interval : Interfaces.Unsigned_64 := Mucontrol.Command.WD_DISABLED)
   is
   begin
      Command_Pages (ID) := (Command           => Command,
                             Epoch             => Epoch,
                             Watchdog_Interval => WD_Interval,
                             Reserved          => (others => 0));
   end Initialize;

   -------------------------------------------------------------------------

   procedure Set_Command
     (ID    : Managed_Subjects_Range;
      Value : Mucontrol.Command.Command_Type)
   is
   begin
      Command_Pages (ID).Command := Value;
   end Set_Command;

   -------------------------------------------------------------------------

   procedure Set_Epoch
     (ID    : Managed_Subjects_Range;
      Value : Interfaces.Unsigned_64)
   is
   begin
      Command_Pages (ID).Epoch := Value;
   end Set_Epoch;

   -------------------------------------------------------------------------

   procedure Set_Watchdog_Interval
     (ID    : Managed_Subjects_Range;
      Value : Interfaces.Unsigned_64)
   is
   begin
      Command_Pages (ID).Watchdog_Interval := Value;
   end Set_Watchdog_Interval;

end Ctrlr.Commands;
