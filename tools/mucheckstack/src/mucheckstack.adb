--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Command_Line;
with Ada.Exceptions;

with Mulog;

with Stackcheck.Cmd_Line;
with Stackcheck.Files;
with Stackcheck.Types;

procedure Mucheckstack
is
   Error    : Boolean;
   Overflow : Boolean;
   Dynamic  : Boolean;
begin
   Stackcheck.Cmd_Line.Init (Description => "Muen stack usage checker");
   Stackcheck.Run (Project_File => Stackcheck.Cmd_Line.Get_GPR_File,
                   Limit        => Stackcheck.Cmd_Line.Get_Stack_Limit,
                   Overflow     => Overflow,
                   Dynamic      => Dynamic);
   Error := Overflow or
     (Dynamic and not Stackcheck.Cmd_Line.Get_Allow_Dynamic);

   Ada.Command_Line.Set_Exit_Status
     (Code => (if Error then Ada.Command_Line.Failure
               else Ada.Command_Line.Success));

exception
   when Stackcheck.Cmd_Line.Invalid_Cmd_Line =>
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : Stackcheck.Process_Error
      | Stackcheck.Files.IO_Error
      | Stackcheck.Types.Circular_Graph
      | Stackcheck.Types.Missing_Subprogram
      | Stackcheck.Types.Duplicate_Subprogram =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Stack check failed, aborting");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Message (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : others =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Unexpected exception");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Information (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
end Mucheckstack;
