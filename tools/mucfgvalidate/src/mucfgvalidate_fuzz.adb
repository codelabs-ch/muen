--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with GNAT.Exception_Actions;

with Mulog;
with Muxml;
with Mucfgcheck;

with Validate.Cmd_Line;

procedure Mucfgvalidate_Fuzz
is
begin
   Validate.Cmd_Line.Init (Description => "Muen policy validator");
   Validate.Run (Policy => Validate.Cmd_Line.Get_Policy);

exception
   when Validate.Cmd_Line.Invalid_Cmd_Line
      | Muxml.XML_Input_Error
      | Muxml.Validation_Error
      | Mucfgcheck.Validation_Error =>
      null;
   when E : others =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Unexpected exception");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Information (X => E));
      GNAT.Exception_Actions.Core_Dump (Occurrence => E);
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
end Mucfgvalidate_Fuzz;
