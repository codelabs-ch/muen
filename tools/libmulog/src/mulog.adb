--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Command_Line;

with Alog.Logger;
with Alog.Facilities.File_Descriptor;

package body Mulog
is

   use Ada.Strings.Unbounded;

   Name     : Unbounded_String;
   Instance : Alog.Logger.Instance (Init => True);

   Level_Map : constant array (Log_Level) of Alog.Log_Level
     := (Debug     => Alog.Debug,
         Info      => Alog.Info,
         Notice    => Alog.Notice,
         Warning   => Alog.Warning,
         Error     => Alog.Error,
         Critical  => Alog.Critical,
         Alert     => Alog.Alert,
         Emergency => Alog.Emergency);

   -------------------------------------------------------------------------

   procedure Log
     (Level : Log_Level := Info;
      Msg   : String)
   is
   begin
      Instance.Log_Message (Source => To_String (Name),
                            Level  => Level_Map (Level),
                            Msg    => Msg);
   end Log;

begin
   declare
      Cmd : constant String   := Ada.Command_Line.Command_Name;
      Sep : constant Positive := Ada.Strings.Fixed.Index
        (Source  => Cmd,
         Pattern => "/",
         Going   => Ada.Strings.Backward);
   begin
      Name := To_Unbounded_String (Cmd (Sep + 1 .. Cmd'Last));
   end;
end Mulog;
